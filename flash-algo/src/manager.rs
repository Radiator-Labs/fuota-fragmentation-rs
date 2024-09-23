//! This module contains the functional behavior of the firmware update process
//!
//! The flash chip is divided into evenly-sized "slots".
//!
//! Each "slot" contains three main parts:
//!
//! * A `[SlotHeader]`, which contains details about the contents and status of
//!    a given slot
//! * A "Written Segments" array, which notes which parts of the data of this slot
//!   have or have not been written
//! * A "Data Region", containing the data
//!
//! As we don't have a true "filesystem", we instead treat the slots within a flash
//! part as a ring buffer, each with a sequential sequence number, allowing us to
//! determine which slots are oldest, and which are newest, even in the presence of
//! wraparounds of the sequence numbers, or wrap around of the ring buffer space.
//!
//! ## Slots
//!
//! Each slot may be one of three kinds:
//!
//! * Empty/Invalid - containing no data
//! * Firmware - containing a binary firmware image, potentially useful as a FUOTA
//!   update image
//! * Parity - containing the parity data for a firmware image
//!
//! ### Header
//!
//! Each slot (of any kind) contains a `[SlotHeader]`. See that struct for more details.
//!
//! ### Written Segments
//!
//! The Written Segments region is an array of `u8` values that note whether a given
//! segment has been received or not. The actual number of segments is stored in the
//! `[SlotHeader]`.
//!
//! This metadata is stored separately from the segments so that the data contained
//! within the segments can be stored contiguously.
//!
//! ### Segments
//!
//! Each Slot is broken up into "segments". These segments are sized to match the
//! size of each packet we expect to receive over the air. For example, we may do
//! a FUOTA with 128x 200-byte data segments, and 64x 200-byte parity segments.
//!
//! ## Method of operation
//!
//! The Flash Algo crate aims to support two separate binaries on the same MCU:
//!
//! * An "Application" - which is the main application code of the product. It is responsible
//!   for receiving new firmware images, and placing them on the external flash, and triggering
//!   the bootloader to perform a firmware update when the download has completed.
//! * A "Bootloader" - who is responsible only for retrieving and validating the new firmware on
//!   the external flash, and applying it to the internal flash, replacing the current Application.

use crate::{
    bitcache::{BitCache, BitIter, FillError},
    fragmentation::get_parity_matrix_row,
    protocol::{
        segment_status_table::{DATA_NOT_WRITTEN, DATA_WRITTEN, MAX_SEGMENTS, MAX_SEGMENT_SIZE},
        total_status, BootOutcome, Crc32, FlashRepr, FlashReprError, Kind, NumberOfSegments,
        SegmentSize, SequenceNumber, Signature, SlotHeader, TotalStatus, WriteExtStatus,
        WriteIntStatus,
    },
    ring::{get_next_seq_no, get_ordered_headers, get_two_newest, IndexedHeader, TwoHdrs},
    spi_flash::{SpiFlash, SpiFlashError},
};
use bitvec::array::BitArray;
use core::iter::Take;
use crc::{Crc, CRC_32_CKSUM};

/// Offset in bytes of the Header within a slot
pub const HEADER_OFFSET: usize = 0;

/// Maximum size in bytes of a Header on-disk
pub const HEADER_SIZE: usize = 1024;

/// Offset in bytes of the "Written segments" array within a slot
pub const WRITTEN_OFFSET: usize = HEADER_OFFSET + HEADER_SIZE;

/// Maximum size in bytes of the "Written Segments" array
pub const WRITTEN_SIZE: usize = MAX_SEGMENTS;

/// Offset in bytes of the data region within a slot
pub const DATA_REGION_OFFSET: usize = WRITTEN_OFFSET + WRITTEN_SIZE;
/// Offset in bytes of the CRC32 header inside the data region (Firmware only)
pub const DATA_CRC32_OFFSET: usize = DATA_REGION_OFFSET;
/// Offset in bytes of the Signature header inside the data region (Firmware only)
pub const DATA_SIGNATURE_OFFSET: usize = DATA_CRC32_OFFSET + Crc32::SIZE;
/// Offset in bytes of the actual firmware contents (Firmware only)
pub const DATA_PAYLOAD_OFFSET: usize = DATA_SIGNATURE_OFFSET + Signature::SIZE;

/// Error type used by `[SlotManager]` and `[ActiveStatus]`
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[derive(Debug, PartialEq)]
pub enum ManagerError<E> {
    /// Errors coming from the underlying SPI flash hardware or
    /// `[SpiFlash]` trait implementation
    Spi(SpiFlashError<E>),

    /// Errors coming from serialization of data
    FlashRepr(FlashReprError),

    /// Something critical has gone wrong, such as a mismatch in buffer size,
    /// or other unrecoverable error.
    Fatal,

    /// The manager expected a header, but no valid header was found
    UnexpectedMissingHeader,

    /// The firmware and parity segments have mismatched segment counts
    SegmentCountMismatch,

    /// The firmware and parity segments have mismatched segment sizes
    SegmentSizeMismatch,

    /// The header's segment count was larger than the allowable maximum
    TooManySegments,

    /// The header's segment length was larger than the allowable maximum
    SegmentsTooLarge,

    /// The header CRC32 and calculated CRC32 are mismatched.
    Crc32Mismatch,

    /// We attempted to check and mark done, but we are not done.
    CheckFailNotDone,
}

impl<E> From<SpiFlashError<E>> for ManagerError<E> {
    fn from(value: SpiFlashError<E>) -> Self {
        Self::Spi(value)
    }
}

impl<E> From<FlashReprError> for ManagerError<E> {
    fn from(value: FlashReprError) -> Self {
        Self::FlashRepr(value)
    }
}

// struct `SlotManager` ---------------------------

/// The `SlotManager` is the primary interface used to query the state of the flash contents
///
/// The `SlotManager` is used in both application and bootloader.
pub struct SlotManager<const N: usize> {
    slot_size: usize,
}

impl<const N: usize> SlotManager<N> {
    /// Create a new slot manager with the given Slot Size, and N slots
    ///
    /// # Panics
    ///
    /// Panics if the given slot size is unsuitable to hold at least the fixed
    /// header size.
    #[must_use]
    pub const fn new(slot_size: usize) -> Self {
        let baseline = HEADER_SIZE + MAX_SEGMENTS;
        assert!(slot_size > baseline);
        Self { slot_size }
    }

    /// Accessor function for the slot size of this slot manager
    #[must_use]
    pub fn slot_size(&self) -> usize {
        self.slot_size
    }

    /// Calculates the largest possible "data segment", including checksum and
    /// CRC32. This is used to ensure that [`NumberOfSegments`] x [`SegmentSize`]
    /// is a reasonable number.
    #[must_use]
    fn max_data_size(&self) -> usize {
        // Subtract off the size required for fixed headers
        let mut max = self.slot_size;
        max = max.saturating_sub(HEADER_SIZE);
        max = max.saturating_sub(MAX_SEGMENTS);
        max
    }

    /// Will the given segment size and number of segments fit into
    /// our data range?
    ///
    /// ## Errors
    ///
    /// Returns an error if the size is not reasonable
    fn is_reasonably_sized<T: SpiFlash>(
        &self,
        segment_size: u32,
        firmware_segments: u32,
    ) -> Result<(), ManagerError<T::Error>> {
        // First sanity check: Is this a feasible size?
        let Ok(mds_u32) = u32::try_from(self.max_data_size()) else {
            // Only happens if we were created with a segment size > u32::MAX
            return Err(ManagerError::SegmentsTooLarge);
        };
        let Some(new_size_u32) = segment_size.checked_mul(firmware_segments) else {
            // Happens if size x segments overflows a u32
            return Err(ManagerError::SegmentsTooLarge);
        };
        if new_size_u32 > mds_u32 {
            // Happens if size x segments exceeds our capacity
            return Err(ManagerError::SegmentsTooLarge);
        }
        // All good!
        Ok(())
    }

    /// Check that the given index is a valid firmware slot
    ///
    /// This is used to determine whether a firmware slot is completed and not
    /// corrupted.
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    pub async fn validate_firmware_slot<T: SpiFlash>(
        &self,
        flash: &mut T,
        scratch: &mut ScratchRam,
        idx: usize,
    ) -> Result<Validated, ManagerError<T::Error>> {
        let start_addr = idx * self.slot_size;

        let hdr = read_header_from_slot(flash, scratch, start_addr)
            .await?
            .ok_or(SpiFlashError::HardwareFailure)?;

        let SlotHeader {
            kind,
            seq_no: _seq_no,
            segment_size,
            num_segments,
            write_ext_status: status,
            // We don't care about internal write status
            write_int_status: _write_int_status,
            boot_outcome,
        } = hdr;

        let mut is_good = true;
        is_good &= kind == Kind::Firmware;
        is_good &= status == WriteExtStatus::Complete;

        is_good &= match boot_outcome {
            BootOutcome::Untested | BootOutcome::Successful => true,
            BootOutcome::Unsuccessful => false,
        };

        if !is_good {
            return Err(SpiFlashError::HardwareFailure.into());
        }

        let slot_start = idx * self.slot_size;

        // Ensure the CRC checks out
        check_crc_from_index(
            flash,
            scratch,
            Some(segment_size.0 as usize),
            Some(num_segments.0 as usize),
            slot_start,
        )
        .await
        .map_err(|_ignore_err| SpiFlashError::HardwareFailure)?;

        let len = segment_size.0 as usize * num_segments.0 as usize;

        Ok(Validated {
            data_start: slot_start + DATA_PAYLOAD_OFFSET,
            data_len: len,
        })
    }

    /// Erase the given slot by index
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    ///
    /// # Panics
    /// Panics if `self.slot_size` is not a multiple of `flash.block_size()`
    async fn erase_slot<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        slot_idx: usize,
    ) -> Result<(), ManagerError<T::Error>> {
        let firm_start = slot_idx * self.slot_size;
        let block_size = flash.block_size();
        let end = firm_start + self.slot_size;
        assert_eq!(self.slot_size % block_size, 0);
        let mut cur = firm_start;
        while cur < end {
            flash.erase_block(cur).await?;
            cur += block_size;
        }
        Ok(())
    }

    /// Start a FUOTA process
    ///
    /// The `SlotManager` will find the two oldest slots, and initialize
    /// a firmware and parity slot with the given metadata.
    ///
    /// On success, returns an `[ActiveStatus]` that can be used to execute
    /// the firmware update process.
    ///
    /// This should typically be called when commanded to start a FUOTA
    /// via a command from the server.
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    ///
    /// # Panics
    /// Panics if write to scratch area fails. This is an unwrap that should be eliminated.
    pub async fn start<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
        segment_size: u32,
        firmware_segments: u32,
    ) -> Result<ActiveStatus, ManagerError<T::Error>> {
        // First sanity check: Is this a feasible size?
        self.is_reasonably_sized::<T>(segment_size, firmware_segments)?;

        let mut firmware_slot_idx = usize::MAX;
        let mut parity_slot_idx = usize::MAX;
        #[allow(clippy::cast_possible_truncation, reason = "Embedded usize is u32")]
        let todo_list = [
            (Kind::Firmware, firmware_segments, &mut firmware_slot_idx),
            // TODO(AJM): Is this okay?
            (Kind::Parity, MAX_SEGMENTS as u32, &mut parity_slot_idx),
        ];

        for (kind, segments, dst_idx) in todo_list {
            let OldestReport {
                slot_idx,
                next_seq_no,
            } = self.find_oldest_slot(flash, scratch).await?;
            *dst_idx = slot_idx;
            self.erase_slot(flash, slot_idx).await?;
            let firm_start = slot_idx * self.slot_size;
            let firmware_hdr = SlotHeader {
                kind,
                seq_no: SequenceNumber(next_seq_no),
                segment_size: SegmentSize(segment_size),
                num_segments: NumberOfSegments(segments),
                write_ext_status: WriteExtStatus::InProgress,
                write_int_status: WriteIntStatus::InProgress,
                boot_outcome: BootOutcome::Untested,
            };
            firmware_hdr
                .write_to_bytes(&mut scratch.header_scratch)
                .map_err(|_e| ManagerError::Fatal)?;
            flash
                .write_from(firm_start, &scratch.header_scratch)
                .await?;
        }

        #[allow(
            clippy::cast_possible_truncation,
            reason = "Embedded usize is u32.. again!"
        )]
        Ok(ActiveStatus {
            segment_size: segment_size as usize,
            slot_size: self.slot_size,
            firmware_slot_idx,
            total_firmware_segments: firmware_segments,
            remaining_firmware_segments: firmware_segments,
            parity_slot_idx,
            total_parity_segments: MAX_SEGMENTS as u32, // TODO(AJM): Is this okay?
            remaining_parity_segments: MAX_SEGMENTS as u32, // TODO(AJM): Is this okay?
        })
    }

    /// Determine the current state of the external flash, as a bootloader
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    pub async fn bl_boot_status<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<BlBootStatus, ManagerError<T::Error>> {
        let hdrs = get_ordered_headers::<T, N>(flash, self.slot_size, scratch).await?;
        let Some(TwoHdrs {
            older: fw,
            newer: pa,
        }) = get_two_newest(&hdrs)
        else {
            return Ok(BlBootStatus::Idle);
        };

        let (Some(f_hdr), Some(p_hdr)) = (fw.hdr.as_ref(), pa.hdr.as_ref()) else {
            return Ok(BlBootStatus::Idle);
        };

        if !((f_hdr.kind == Kind::Firmware) && (p_hdr.kind == Kind::Parity)) {
            return Ok(BlBootStatus::Idle);
        }

        #[allow(clippy::cast_possible_truncation, reason = "Embedded usize is u32")]
        match total_status(f_hdr) {
            TotalStatus::BootloadWriteInProgress => {
                Ok(BlBootStatus::IncompleteInternal { idx: fw.idx as u32 })
            }
            TotalStatus::FirstBootPendingAck => Ok(BlBootStatus::FailedLoad { idx: fw.idx as u32 }),
            // Anything else: Idle
            TotalStatus::BlankSlot
            | TotalStatus::AppWriteInProgress
            | TotalStatus::AppWriteAborted
            | TotalStatus::ConfirmedImage
            | TotalStatus::RejectedImage
            | TotalStatus::InvalidNeedsErase => Ok(BlBootStatus::Idle),
        }
    }

    /// Set the "Write External Status" field of the given index to "Aborted".
    ///
    /// NOTE: Does NOT check the prior contents of the field.
    ///
    /// # Errors
    /// Reports errors for Flash and SPI bus
    pub async fn write_ext_status_aborted<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        idx: usize,
    ) -> Result<(), ManagerError<T::Error>> {
        let address = idx * self.slot_size;
        let mut buf = [0_u8; WriteExtStatus::SIZE];
        WriteExtStatus::Aborted.write_to_bytes(&mut buf)?;
        flash
            .write_from(address + SlotHeader::WRITE_EXT_STATUS_OFFSET, &buf)
            .await?;
        Ok(())
    }

    /// Set the "Write Internal Status" field of the given index to "Complete".
    ///
    /// NOTE: Does NOT check the prior contents of the field.
    ///
    /// # Errors
    /// Reports errors for Flash and SPI bus
    pub async fn write_int_status_complete<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        idx: usize,
    ) -> Result<(), ManagerError<T::Error>> {
        let address = idx * self.slot_size;
        let mut buf = [0_u8; WriteIntStatus::SIZE];
        WriteIntStatus::Complete.write_to_bytes(&mut buf)?;
        flash
            .write_from(address + SlotHeader::WRITE_INT_STATUS_OFFSET, &buf)
            .await?;
        Ok(())
    }

    /// Set the "Boot Outcome Status" field of the given index to "Successful".
    ///
    /// NOTE: Does NOT check the prior contents of the field.
    ///
    /// # Errors
    /// Reports errors for Flash and SPI bus
    pub async fn write_boot_outcome_successful<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        idx: usize,
    ) -> Result<(), ManagerError<T::Error>> {
        let address = idx * self.slot_size;
        let mut buf = [0_u8; BootOutcome::SIZE];
        BootOutcome::Successful.write_to_bytes(&mut buf)?;
        flash
            .write_from(address + SlotHeader::BOOT_OUTCOME_OFFSET, &buf)
            .await?;
        Ok(())
    }

    /// Set the "Boot Outcome Status" field of the given index to "Unsuccessful".
    ///
    /// NOTE: Does NOT check the prior contents of the field.
    ///
    /// # Errors
    /// Reports errors for Flash and SPI bus
    pub async fn write_boot_outcome_unsuccessful<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        idx: usize,
    ) -> Result<(), ManagerError<T::Error>> {
        let address = idx * self.slot_size;
        let mut buf = [0_u8; BootOutcome::SIZE];
        BootOutcome::Unsuccessful.write_to_bytes(&mut buf)?;
        flash
            .write_from(address + SlotHeader::BOOT_OUTCOME_OFFSET, &buf)
            .await?;
        Ok(())
    }

    /// Determine the current state of the external flash, as an application
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    ///
    /// # Panics
    /// Panics if write to buffer fails. This is an unwrap that should be eliminated.
    #[allow(
        clippy::too_many_lines,
        reason = "did not want to refactor the old algorithm"
    )]
    pub async fn app_boot_status<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<AppBootStatus, ManagerError<T::Error>> {
        // The two newest headers should represent the firmware and parity, IF we are
        // currently bootloading.
        let headers = get_ordered_headers::<T, N>(flash, self.slot_size, scratch).await?;
        let two_newest = get_two_newest(&headers);

        let Some(TwoHdrs { older, newer }) = two_newest else {
            self.cancel_all_ext_pending(flash, &headers).await?;
            return Ok(AppBootStatus::Idle);
        };

        let (Some(f_hdr), Some(p_hdr)) = (older.hdr.as_ref(), newer.hdr.as_ref()) else {
            self.cancel_all_ext_pending(flash, &headers).await?;
            return Ok(AppBootStatus::Idle);
        };

        // Firmware sanity checking
        let fw_in_progress = total_status(f_hdr) == TotalStatus::AppWriteInProgress;
        let fw_right_kind = f_hdr.kind == Kind::Firmware;
        self.is_reasonably_sized::<T>(f_hdr.segment_size.0, f_hdr.num_segments.0)?;

        // Parity sanity checking
        let pa_in_progress = total_status(p_hdr) == TotalStatus::AppWriteInProgress;
        let pa_right_kind = p_hdr.kind == Kind::Parity;
        let pa_right_segments = f_hdr.segment_size == p_hdr.segment_size;

        let all_good =
            fw_in_progress && fw_right_kind && pa_in_progress && pa_right_kind && pa_right_segments;

        if !all_good {
            self.cancel_all_ext_pending(flash, &headers).await?;
            return Ok(AppBootStatus::Idle);
        }

        let f_idx = older.idx;
        let p_idx = newer.idx;

        // Now check the "other" items to see if they need remediation
        let now_indexes = [f_idx, p_idx];
        #[allow(
            clippy::pattern_type_mismatch,
            reason = "unclear how to refactor, TODO: eliminate this allow"
        )]
        for IndexedHeader {
            idx,
            hdr: hdr_option,
        } in &headers
        {
            if now_indexes.contains(idx) {
                continue;
            }
            let Some(hdr) = hdr_option else {
                continue;
            };
            match total_status(hdr) {
                TotalStatus::AppWriteInProgress => {
                    self.write_ext_status_aborted(flash, *idx).await?;
                }
                TotalStatus::BootloadWriteInProgress => {
                    // unreachable!("How did we boot to the app?")
                    #[cfg(feature = "defmt")]
                    defmt::info!(
                        "Unreachable app_boot_status with TotalStatus::BootloadWriteInProgress"
                    );
                    self.erase_slot(flash, *idx).await?;
                }
                TotalStatus::InvalidNeedsErase => {
                    self.erase_slot(flash, *idx).await?;
                }
                TotalStatus::BlankSlot
                | TotalStatus::AppWriteAborted
                | TotalStatus::FirstBootPendingAck
                | TotalStatus::ConfirmedImage
                | TotalStatus::RejectedImage => {}
            }
        }

        // TODO, any CRC checking, making sure the remaining parity isn't impossible?

        let total_firmware_segments = f_hdr.num_segments.0 as usize;
        let total_parity_segments = p_hdr.num_segments.0 as usize;

        let data_start_addr = (f_idx * self.slot_size) + WRITTEN_OFFSET;
        let Ok(received_firmware_scratch) = fill_bitcache(
            flash,
            &mut scratch.parity_temp_page,
            &mut scratch.received_firmware_scratch,
            data_start_addr,
            total_firmware_segments,
        )
        .await
        else {
            return self.cancel_all_ext_pending(flash, &headers).await;
        };

        let parity_start_addr = (p_idx * self.slot_size) + WRITTEN_OFFSET;
        let Ok(received_parity_scratch) = fill_bitcache(
            flash,
            &mut scratch.parity_temp_page,
            &mut scratch.received_parity_scratch,
            parity_start_addr,
            total_parity_segments,
        )
        .await
        else {
            return self.cancel_all_ext_pending(flash, &headers).await;
        };

        let data_remain = received_firmware_scratch.fold(0, |acc, b| acc + u32::from(!b));
        let parity_remain = received_parity_scratch.fold(0, |acc, b| acc + u32::from(!b));

        Ok(AppBootStatus::InProgress(ActiveStatus {
            segment_size: f_hdr.segment_size.0 as usize,
            slot_size: self.slot_size,

            firmware_slot_idx: f_idx,
            parity_slot_idx: p_idx,

            total_firmware_segments: f_hdr.num_segments.0,
            remaining_firmware_segments: data_remain,
            total_parity_segments: p_hdr.num_segments.0,
            remaining_parity_segments: parity_remain,
        }))
    }

    /// Find the oldest slot on the external flash
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    async fn find_oldest_slot<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<OldestReport, ManagerError<T::Error>> {
        let hdrs = get_ordered_headers::<T, N>(flash, self.slot_size, scratch).await?;

        let next = get_next_seq_no(&hdrs);
        Ok(OldestReport {
            slot_idx: hdrs[0].idx,
            next_seq_no: next,
        })
    }

    /// Mark any in-progress firmware writes as Aborted. Used for error case recovery.
    async fn cancel_all_ext_pending<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        hdrs: &[IndexedHeader; N],
    ) -> Result<AppBootStatus, ManagerError<T::Error>> {
        for hdr in hdrs {
            if let Some(h) = hdr.hdr.as_ref() {
                if h.write_ext_status == WriteExtStatus::InProgress {
                    self.write_ext_status_aborted(flash, hdr.idx).await?;
                }
            }
        }

        Ok(AppBootStatus::Idle)
    }
    /// Find the oldest slot on the external flash
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    pub async fn cancel_all_ext_pending_from_scratch<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<AppBootStatus, ManagerError<T::Error>> {
        let hdrs = get_ordered_headers::<T, N>(flash, self.slot_size, scratch).await?;
        self.cancel_all_ext_pending(flash, &hdrs).await
    }
}

// struct SlotManager ---------------------------

// struct `ActiveStatus` ---------------------------

/// `ActiveStatus` is the main application interface while a single FUOTA process is active.
///
/// It contains methods for receiving and checking data.
//
// TODO: The contained fields should not be `pub`, but are made so for ease of testing.
// This could be fixed with proper accessor methods.
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[derive(Debug, PartialEq)]
pub struct ActiveStatus {
    pub(crate) slot_size: usize,
    pub(crate) segment_size: usize,

    pub(crate) firmware_slot_idx: usize,
    pub(crate) total_firmware_segments: u32,
    pub(crate) remaining_firmware_segments: u32,

    pub(crate) parity_slot_idx: usize,
    pub(crate) total_parity_segments: u32,
    pub(crate) remaining_parity_segments: u32,
}

impl ActiveStatus {
    /// Is the current FUOTA process complete?
    ///
    /// NOTE: This only checks if all firmware segments have been received
    #[must_use]
    pub fn is_complete(&self) -> bool {
        self.remaining_firmware_segments == 0
    }

    /// Get the remaining number of firmware and parity segments
    #[must_use]
    pub fn remaining(&self) -> (u32, u32) {
        (
            self.remaining_firmware_segments,
            self.remaining_parity_segments,
        )
    }

    /// Attempt to write a received segment
    ///
    /// This may either be a firmware segment, or a parity segment.
    ///
    /// This function DOES handle duplicate reception of a segment, AS LONG AS
    /// the duplicate data matches the previously received data.
    ///
    /// On success, returns a `[WriteSegmentOutcome]` indicating the potential next steps.
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    ///
    /// # Panics
    /// Panics if buffer sizes are not equal.
    pub async fn write_segment<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
        segment_1idx: u32,
        bytes: &[u8],
    ) -> Result<WriteSegmentOutcome, SpiFlashError<T::Error>> {
        self.write_segment_internal(flash, &mut scratch.firmware_rd_scratch, segment_1idx, bytes)
            .await
    }

    // Actual implementation of
    async fn write_segment_internal<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        read_scratch: &mut [u8],
        segment_1idx: u32,
        bytes: &[u8],
    ) -> Result<WriteSegmentOutcome, SpiFlashError<T::Error>> {
        // Determine if this is a firmware segment or a parity segment
        let (slot_idx, segment_0idx) = if segment_1idx == 0 {
            #[cfg(feature = "defmt")]
            defmt::info!(
                "write_segment: Out of bounds segment_1idx {=u32} == 0",
                segment_1idx
            );

            // segments are 1-indexed
            return Err(SpiFlashError::OutOfBounds);
        } else if segment_1idx <= self.total_firmware_segments {
            // firmware segments come first
            (self.firmware_slot_idx, segment_1idx - 1)
        } else if segment_1idx <= (self.total_firmware_segments + self.total_parity_segments) {
            // Then parity segments
            (
                self.parity_slot_idx,
                segment_1idx - 1 - self.total_firmware_segments,
            )
        } else {
            #[cfg(feature = "defmt")]
            defmt::info!(
                "write_segment: Out of bounds segment_1idx {=u32} > (total_firmware_segments {=u32} + total_parity_segments {=u32})",
                segment_1idx,
                self.total_firmware_segments,
                self.total_parity_segments
            );
            return Err(SpiFlashError::OutOfBounds);
        };

        // Do some sanity checking that this is all okay
        let written_in_range = (segment_0idx as usize) < WRITTEN_SIZE;
        let data_in_range = ((segment_0idx + 1) as usize * self.segment_size) <= self.slot_size;
        assert_eq!(self.segment_size, bytes.len());

        if !(written_in_range && data_in_range) {
            #[cfg(feature = "defmt")]
            defmt::info!(
                "write_segment: Out of bounds segment_0idx {=u32}",
                segment_0idx
            );
            #[cfg(feature = "defmt")]
            defmt::info!(
                "  written_in_range {=bool}, idx < WRITTEN_SIZE {=usize}",
                written_in_range,
                WRITTEN_SIZE
            );
            #[cfg(feature = "defmt")]
            defmt::info!(
                "  data_in_range {}, idx * segment_size {} <= slot_size {}",
                data_in_range,
                self.segment_size,
                self.slot_size
            );

            return Err(SpiFlashError::OutOfBounds);
        }

        // Have we previously written this segment before?
        let slot_start = slot_idx * self.slot_size;
        let mut value_buffer = [0xAB_u8; 1];
        let written_addr = slot_start + WRITTEN_OFFSET + (segment_0idx as usize);
        flash.read_to(written_addr, &mut value_buffer).await?;

        // NOTE: When writing chunks, we want to write the entire data region, including
        // the CRC and Signature header.
        let data_start =
            slot_start + DATA_REGION_OFFSET + (segment_0idx as usize * self.segment_size);

        match value_buffer[0] {
            DATA_NOT_WRITTEN => {
                // Not written, we're good
            }
            #[allow(clippy::indexing_slicing, reason = "TODO: eliminate possible panic")]
            DATA_WRITTEN => {
                // Uh oh, was written, does the existing value match the current data?
                flash.read_to(data_start, read_scratch).await?;
                assert_eq!(&read_scratch[..bytes.len()], bytes);
                // All good!
                return Ok(WriteSegmentOutcome::Consumed);
            }
            #[allow(clippy::panic, reason = "TODO: eliminate this panic")]
            _ => panic!(),
        }

        flash.write_from(data_start, bytes).await?;
        value_buffer[0] = DATA_WRITTEN;
        flash.write_from(written_addr, &value_buffer).await?;

        if slot_idx == self.firmware_slot_idx {
            self.remaining_firmware_segments -= 1;
        } else {
            self.remaining_parity_segments -= 1;
        }

        match (
            self.remaining_firmware_segments,
            self.remaining_parity_segments,
        ) {
            // No remaining firmware segments, all done!
            (0, _) => Ok(WriteSegmentOutcome::FirmwareComplete),
            // We got SOMETHING but there are no parity frames, no need to check parity
            // if we have none.
            (_, n) if n == self.total_parity_segments => Ok(WriteSegmentOutcome::Consumed),
            // We got a new firmware OR parity, which means there might be something to
            // check and recover now.
            _ => Ok(WriteSegmentOutcome::ConsumedMaybeParity),
        }
    }

    /// Attempt to perform a repair of a firmware segment, using other firmware and parity segments
    ///
    /// If a firmware segment was recovered, a `Some` will be returned with the repaired segment.
    /// It may be possible to recover further segments at this time
    ///
    /// If no firmware segment was recovered, a `None` will be returned. It will not be possible to
    /// successfully recover another segment until a new firmware or parity segment is received.
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    #[allow(
        clippy::too_many_lines,
        reason = "do not want to refactor old algorithm"
    )]
    pub async fn repair_step<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<Option<usize>, SpiFlashError<T::Error>> {
        // NOTE/TODO: This is a brute force, O(n^2) approach to repairing one
        // data segment at a time. This also means that generally repairing
        // many frames approaches O(n^3) (since this needs to be called once
        // for every frame that needs to be repaired).
        //
        // This uses a lot of scratch memory, and is generally very wasteful.
        // There is likely a fancy matrix math way of determining what parity
        // frames we should skip or ignore completely because they are not
        // currently relevant, or have served their purpose and will not be
        // useful again.

        // If we've received or recovered all firmware segments, nothing to do
        if self.remaining_firmware_segments == 0 {
            #[cfg(feature = "defmt")]
            defmt::info!("SKIP REPAIR: ALREADY DONE");
            return Ok(None);
        }
        // If we don't have any parity segments, nothing to do
        if self.remaining_parity_segments == self.total_parity_segments {
            #[cfg(feature = "defmt")]
            defmt::info!("SKIP REPAIR: NO PARITY");
            return Ok(None);
        }

        #[allow(clippy::indexing_slicing, reason = "TODO: eliminate possible panic")]
        let firmware_rd_scratch = &mut scratch.firmware_rd_scratch[..self.segment_size];
        #[allow(
            clippy::indexing_slicing,
            reason = "TODO: eliminate possible panic, again"
        )]
        let firmware_wr_scratch = &mut scratch.firmware_wr_scratch[..self.segment_size];

        // Read which firmware and parity blocks we have.
        let data_start_addr = (self.firmware_slot_idx * self.slot_size) + WRITTEN_OFFSET;
        let total_firmware_segments = self.total_firmware_segments as usize;
        let received_firmware_scratch = fill_bitcache(
            flash,
            &mut scratch.parity_temp_page,
            &mut scratch.received_firmware_scratch,
            data_start_addr,
            total_firmware_segments,
        )
        .await?;

        let parity_start_addr = (self.parity_slot_idx * self.slot_size) + WRITTEN_OFFSET;
        let total_parity_segments = self.total_parity_segments as usize;
        let received_parity_scratch = fill_bitcache(
            flash,
            &mut scratch.parity_temp_page,
            &mut scratch.received_parity_scratch,
            parity_start_addr,
            total_parity_segments,
        )
        .await?;

        // LAYER 1:
        //
        // For each parity segment in this transaction...
        'parity: for (parity_i, pf) in received_parity_scratch.enumerate() {
            // Have we received this parity segment? If not, nothing to do.
            if !pf {
                continue;
            }

            // If we have, figure out which data segments this parity segment
            // is supposed to apply to. We calculate this into the scratch
            // space.
            //
            // TODO(AJM): Replace this BitCache with a persistent one!
            #[allow(clippy::cast_possible_truncation, reason = "Embedded usize is u32")]
            get_parity_matrix_row(
                parity_i as u32 + 1,
                self.total_firmware_segments,
                &mut scratch.parity_mask_scratch,
            );

            // We are now looking for a case where EXACTLY ONE of the relevant
            // data segments are missing. This means we can recover that segment
            // using the parity.
            let mut missing = None;

            // Zip together:
            //
            // * "does this parity apply to data segment N"
            // * "have we received data segment N"
            let parity_mask_scratch = scratch
                .parity_mask_scratch
                .iter()
                .take(total_firmware_segments);
            let plan = parity_mask_scratch
                .clone()
                .take(self.total_firmware_segments as usize)
                .zip(received_firmware_scratch.clone());

            // LAYER 2: For all data segments, compare against this parity
            // segment for relevance.
            for (firmware_i, (p, f)) in plan.enumerate() {
                // This index is interesting to us if the parity segment
                // DOES cover this segment AND we have not received this
                // data segment.
                let is_missing = *p && !f;

                #[allow(unused_variables, reason = "Used when defmt configuration set")]
                match (is_missing, missing) {
                    (true, None) => {
                        // This IS interesting, AND it is the first missing
                        // interesting segment we've found
                        missing = Some(firmware_i);
                    }
                    (true, Some(old_i)) => {
                        // This IS interesting, BUT it's a second missing
                        // frame, which means we can't recover this.
                        //
                        // Bail on checking this entire parity frame
                        #[cfg(feature = "defmt")]
                        defmt::info!(
                            "{=usize} -> two missing {=usize}, {=usize}",
                            parity_i,
                            old_i,
                            firmware_i
                        );
                        continue 'parity;
                    }
                    (false, _) => {
                        // This is not interesting.
                    }
                }
            }

            // Okay, we checked everything, and found that this parity
            // segment was missing EXACTLY one data segment!
            if let Some(fwi) = missing {
                // Okay it's recovery time.
                //
                // Load the parity frame as the initial contents
                let parity_data_addr = (self.parity_slot_idx * self.slot_size)
                    + DATA_REGION_OFFSET
                    + (parity_i * self.segment_size);
                flash.read_to(parity_data_addr, firmware_wr_scratch).await?;

                // Now XOR the contents of each of the relevant frames
                for (flash_index, relevant) in parity_mask_scratch.clone().enumerate() {
                    // If this ISN'T a relevant frame, skip it.
                    // If it IS relevant BUT is also the missing frame, skip it.
                    if (flash_index == fwi) || !relevant {
                        continue;
                    }

                    let firmware_data_addr = (self.firmware_slot_idx * self.slot_size)
                        + DATA_REGION_OFFSET
                        + (flash_index * self.segment_size);
                    flash
                        .read_to(firmware_data_addr, firmware_rd_scratch)
                        .await?;
                    firmware_wr_scratch
                        .iter_mut()
                        .zip(firmware_rd_scratch.iter())
                        .for_each(|(a, b)| {
                            *a ^= *b;
                        });
                }

                // By now, we should have now recovered the frame, write it using
                // the normal writing process so it is marked as received.
                #[allow(clippy::cast_possible_truncation, reason = "embedded usize is u32")]
                self.write_segment_internal(
                    flash,
                    firmware_rd_scratch,
                    fwi as u32 + 1,
                    firmware_wr_scratch,
                )
                .await?;
                // yay!!!
                return Ok(Some(fwi));
            }
        }
        Ok(None)
    }

    /// Check if the CRC of a given slot checks out.
    /// This will only succeed on a completed Firmware slot.
    ///
    /// # Errors
    /// Reports if `check_crc` operation fails
    async fn check_fw_idx_crc<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<(), ManagerError<T::Error>> {
        let slot_start = self.firmware_slot_idx * self.slot_size;

        check_crc_from_index(
            flash,
            scratch,
            Some(self.segment_size),
            Some(self.total_firmware_segments as usize),
            slot_start,
        )
        .await
    }

    /// Perform a final check that a FUOTA process is complete.
    ///
    /// If it checks out, the firmware and parity slots will be marked as
    /// having a complete ext write status.
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    ///
    /// # Panics
    /// Panics if write to buffer fails. Need to eliminate unwrap.
    pub async fn check_and_mark_done<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<usize, ManagerError<T::Error>> {
        if self.remaining_firmware_segments != 0 {
            return Err(ManagerError::CheckFailNotDone);
        }

        self.check_fw_idx_crc(flash, scratch).await?;

        let fw_slot_start = self.firmware_slot_idx * self.slot_size;
        let pa_slot_start = self.parity_slot_idx * self.slot_size;

        // TODO: should this be a helper method, rather than doing it directly?
        let mut buf = [0_u8; WriteExtStatus::SIZE];
        WriteExtStatus::Complete.write_to_bytes(&mut buf)?;

        flash
            .write_from(fw_slot_start + SlotHeader::WRITE_EXT_STATUS_OFFSET, &buf)
            .await?;
        flash
            .write_from(pa_slot_start + SlotHeader::WRITE_EXT_STATUS_OFFSET, &buf)
            .await?;

        Ok(self.firmware_slot_idx)
    }

    #[must_use]
    pub fn total_firmware_segments(&self) -> u32 {
        self.total_firmware_segments
    }

    #[must_use]
    pub fn remaining_firmware_segments(&self) -> u32 {
        self.remaining_firmware_segments
    }

    #[must_use]
    pub fn received_firmware_segments(&self) -> u32 {
        if self.total_firmware_segments > self.remaining_firmware_segments {
            self.total_firmware_segments - self.remaining_firmware_segments
        } else {
            0
        }
    }
}

/// Fill the given [`BitCache`], from the given flash address
///
/// This loads `len` bytes, starting at `start_addr`, in strides of
/// `<= scratch_bytes.len()`.
///
/// Returns an iterator over the range that was filled
async fn fill_bitcache<'a, T: SpiFlash>(
    flash: &mut T,
    scratch_bytes: &mut [u8],
    bitbuf: &'a mut BitCache,
    start_addr: usize,
    len: usize,
) -> Result<Take<BitIter<'a>>, SpiFlashError<T::Error>> {
    let mut addr = start_addr;
    let mut remain = len;

    while remain != 0 {
        let stride = remain.min(scratch_bytes.len());
        #[allow(
            clippy::indexing_slicing,
            reason = "do not want to refactor old algorithm"
        )]
        let buf = &mut scratch_bytes[..stride];

        flash.read_to(addr, buf).await?;
        let res = bitbuf.fill_from(addr - start_addr, buf);

        if let Err(e) = res {
            return match e {
                // If we tried to overfill the buf, that's a logic error
                FillError::OutOfRange => Err(SpiFlashError::LogicError),
                // If we got bad data: possible SPI flash corruption
                FillError::InvalidByte => Err(SpiFlashError::HardwareFailure),
            };
        }

        addr += stride;
        remain -= stride;
    }

    Ok(bitbuf.iter().take(len))
}

// struct `ActiveStatus` ---------------------------

// struct `ScratchRam` ---------------------------

/// `ScratchRam` is a helper type for obtaining necessary in-memory caches of
/// external flash contents.
///
/// Methods that require "paging in/out" contents from the external flash will
/// generally require an `&mut ScratchRam` rather than stack-allocating temporary
/// buffers.
///
/// `[ScratchRam]` is (relatively) extremely large, in the order of ~10KiB. Care must
/// be taken NOT to pass it by value up or down the stack, which could easily
/// lead to stack overflow cases.
///
/// In embedded systems applications, it is recommended to create a statically
/// allocated singleton, e.g.:
///
/// ```rust,ignore
/// use cortex_m::singleton;
/// fn main() {
///     let scratch: &'static mut ScratchRam = singleton!(: ScratchRam = ScratchRam::new()).unwrap();
/// }
/// ```
//
// NOTE: These probably *shouldn't* be pub, as they are an internal implementation
// detail, however we use them in integration tests, so we cannot mark them as private
// or `pub(crate)`. The tests COULD be modified to provide their own buffers, if necessary.
#[allow(
    clippy::partial_pub_fields,
    reason = "Do not want to change deployed API"
)]
pub struct ScratchRam {
    /// One array to hold the list of all received firmware segments
    pub(crate) received_firmware_scratch: BitCache,
    /// One array to hold the list of all received parity segments
    pub(crate) received_parity_scratch: BitCache,
    /// One array to hold a single "does this parity segment correspond
    ///   to a given data segment" row.
    pub(crate) parity_mask_scratch: BitArray<[u8; MAX_SEGMENTS / 8]>,

    /// An array to page-in parity data when loading a [`BitCache`]
    /// from flash data
    pub(crate) parity_temp_page: [u8; Self::PARITY_TEMP_LEN],

    /// A single read segment
    pub firmware_rd_scratch: [u8; MAX_SEGMENT_SIZE],
    /// A single write segment
    pub(crate) firmware_wr_scratch: [u8; MAX_SEGMENT_SIZE],

    /// For one header
    pub(crate) header_scratch: [u8; SlotHeader::SIZE],
}

impl Default for ScratchRam {
    fn default() -> Self {
        Self::new()
    }
}

impl ScratchRam {
    /// Number of bytes per stride when loading [`BitCaches`] from Flash
    const PARITY_TEMP_LEN: usize = 128;

    /// Create a new, empty (zero-filled) `ScratchRam`
    #[must_use]
    pub const fn new() -> Self {
        Self {
            received_firmware_scratch: BitCache::new(),
            received_parity_scratch: BitCache::new(),
            parity_mask_scratch: BitArray::ZERO,
            firmware_rd_scratch: [0_u8; MAX_SEGMENT_SIZE],
            firmware_wr_scratch: [0_u8; MAX_SEGMENT_SIZE],
            header_scratch: [0_u8; SlotHeader::SIZE],
            parity_temp_page: [0_u8; Self::PARITY_TEMP_LEN],
        }
    }
}

// struct ScratchRam ---------------------------

/// The outcome of a call to `[ActiveStatus::write_segment]`
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[derive(Debug, PartialEq)]
pub enum WriteSegmentOutcome {
    /// Frame was consumed, and no parity check is recommended at the moment.
    ///
    /// This typically means either:
    ///
    /// * We haven't finished receiving all firmware frames, AND there are no
    ///   parity frames
    /// * This was a complete duplicate frame (for example if this is a retry).
    Consumed,
    /// Frame was consumed, and it might be worth checking if we can recover
    /// any frames. This typically means:
    ///
    /// * We are missing some firmware frames, and we just got a parity frame
    /// * We got a firmware frame AND have some parity frames, so this might be
    ///   useful
    ConsumedMaybeParity,
    /// We have no missing firmware slots
    ///
    /// TODO: do we wait to verify with any parity frames? Or bail immediately?
    FirmwareComplete,
}

/// Outcome of a call to `[SlotManager::app_boot_status]`
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[derive(Debug, PartialEq)]
pub enum AppBootStatus {
    /// No in-progress application writes to external flash
    Idle,
    /// An in-progress application write exists
    InProgress(ActiveStatus),
}

/// Outcome of a call to `[SlotManager::bl_boot_status]`
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[derive(Debug, PartialEq)]
pub enum BlBootStatus {
    /// No in-progress external to internal writes are active
    Idle,
    /// An external-to-internal write is in-progress and seems to have
    /// been interrupted
    IncompleteInternal { idx: u32 },
    /// An external-to-internal write occurred and was completed, but
    /// we rebooted without the (new) application marking it as completed.
    FailedLoad { idx: u32 },
}

/// Outcome of a call to `[SlotManager::validate_firmware_slot]`
pub struct Validated {
    /// The start of the FIRMWARE of the given slot.
    ///
    /// NOTE: This skips the crc32 and signature area header!
    pub data_start: usize,
    /// The length in bytes of the data segment of the given slot
    pub data_len: usize,
}

/// Read the header of a given slot
///
/// # Errors
/// Reports if SPI Flash activities fail
pub async fn read_header_from_slot<T: SpiFlash>(
    flash: &mut T,
    scratch: &mut ScratchRam,
    slot_start: usize,
) -> Result<Option<SlotHeader>, ManagerError<T::Error>> {
    flash
        .read_to(slot_start, &mut scratch.header_scratch)
        .await?;
    Ok(SlotHeader::take_from_bytes(&scratch.header_scratch).map(|(h, _r)| h))
}

/// Check the CRC of a given slot
///
/// # Errors
/// Reports if SPI Flash activities fail
pub async fn check_crc_from_index<T: SpiFlash>(
    flash: &mut T,
    scratch: &mut ScratchRam,
    segment_size_option: Option<usize>,
    segments_option: Option<usize>,
    slot_start: usize,
) -> Result<(), ManagerError<T::Error>> {
    let calc_crc = Crc::<u32>::new(&CRC_32_CKSUM);
    let mut digest = calc_crc.digest();

    // Read header
    let hdr = read_header_from_slot(flash, scratch, slot_start)
        .await?
        .ok_or(ManagerError::UnexpectedMissingHeader)?;

    // Now read the payload header
    let mut data_hdr = [0_u8; Crc32::SIZE + Signature::SIZE];
    flash
        .read_to(slot_start + DATA_REGION_OFFSET, &mut data_hdr)
        .await?;
    let (expected_crc32, later) = Crc32::take_from_bytes(&data_hdr).ok_or(ManagerError::Fatal)?;
    let (_expected_sig, _later) = Signature::take_from_bytes(later).ok_or(ManagerError::Fatal)?;

    let segments = match segments_option {
        #[allow(clippy::cast_possible_truncation, reason = "embedded usize is u32")]
        Some(s) if s as u32 == hdr.num_segments.0 => s,
        Some(_) => return Err(ManagerError::SegmentCountMismatch),
        None => hdr.num_segments.0 as usize,
    };
    let segment_size = match segment_size_option {
        #[allow(
            clippy::cast_possible_truncation,
            reason = "embedded usize is u32, again"
        )]
        Some(s) if s as u32 == hdr.segment_size.0 => s,
        Some(_) => return Err(ManagerError::SegmentSizeMismatch),
        None => hdr.segment_size.0 as usize,
    };

    if segments > MAX_SEGMENTS {
        return Err(ManagerError::TooManySegments);
    }
    if segment_size > MAX_SEGMENT_SIZE {
        return Err(ManagerError::SegmentsTooLarge);
    }

    // When calculating the CRC32, we need to skip the prepended
    // CRC32 and signature for calculations.
    //
    // TODO: Phil - you'll need to calculate the ed25519 signature over
    // the same payload bytes as the CRC32 here, and check it against
    // the `_expected_sig` variable we decoded above!
    let mut skip_remain = Some(Crc32::SIZE + Signature::SIZE);

    // Take a properly sized scratch buffer for our segment sizes
    #[allow(
        clippy::indexing_slicing,
        reason = "do not want to change old algorithm"
    )]
    let segment_buf = &mut scratch.firmware_rd_scratch[..segment_size];

    for idx in 0..segments {
        // Decide if any header bytes still need to be skipped
        let to_skip = match skip_remain.take() {
            // Nope, no remaining header bytes!
            None => 0,
            // Yes, AND it's larger than a whole segment, so just skip
            // reading the whole segment, and decrement by the number
            // to skip by a whole segment
            Some(n) if n >= segment_size => {
                skip_remain = Some(n - segment_size);
                continue;
            }
            // Yes, and it's NOT larger than a whole segment, so just
            // skip the remaining bytes (and don't store back any
            // more to be skipped). It's okay if `n == 0` at this point.
            Some(n) => n,
        };

        let data_start = slot_start + DATA_REGION_OFFSET + (idx * segment_size);

        flash.read_to(data_start, segment_buf).await?;
        #[allow(
            clippy::indexing_slicing,
            reason = "do not want to change old algorithm, again"
        )]
        digest.update(&segment_buf[to_skip..]);
    }

    let calc_crc32 = digest.finalize();

    if expected_crc32.0 == calc_crc32 {
        Ok(())
    } else {
        #[cfg(feature = "defmt")]
        defmt::error!(
            "Crc32Mismatch expected {=u32:x} calc {=u32:x}",
            expected_crc32.0,
            calc_crc32
        );

        Err(ManagerError::Crc32Mismatch)
    }
}

/// Calculate the NEXT sequence number, avoiding the invalid
/// value of `u32::MAX`, and handling wraparound.
#[must_use]
pub(crate) fn next_seq(cur: u32) -> u32 {
    match cur.wrapping_add(1) {
        u32::MAX => 0,
        other => other,
    }
}

// struct OldestReport ---------------------------

/// A structure that contains the oldest slot index, and the next sequence number
#[derive(Debug, PartialEq)]
struct OldestReport {
    slot_idx: usize,
    next_seq_no: u32,
}

#[allow(
    clippy::cast_possible_truncation,
    clippy::cast_lossless,
    clippy::items_after_statements,
    clippy::match_same_arms,
    clippy::print_stdout,
    clippy::shadow_reuse,
    clippy::shadow_unrelated,
    clippy::trivially_copy_pass_by_ref,
    clippy::uninlined_format_args,
    clippy::unreachable,
    clippy::unwrap_used,
    clippy::use_debug
)]
#[cfg(test)]
mod test {
    extern crate std;
    use std::{prelude::rust_2021::*, print, println};

    use crate::testutils::{heap_flash::Flash, protocol_test_support::make_a_slot};

    use super::*;
    use insta::assert_snapshot;

    #[test]
    fn check_layout_constants() {
        // This test should not be modified or fail, it ensures
        // our memory layout remains compatible with existing bootloaders

        assert_eq!(DATA_PAYLOAD_OFFSET, 0x4444);
        assert_eq!(MAX_SEGMENT_SIZE, 0x100);
    }

    #[test]
    #[allow(clippy::assertions_on_constants)]
    fn size_checker() {
        // Scratch RAM last updated: 2024-01-02
        // assert_eq!(core::mem::size_of::<ScratchRam>(), 1_436); // BEFORE 2K -> 16K
        assert_eq!(core::mem::size_of::<ScratchRam>(), 6_812); // AFTER  2K -> 16K

        // Will the SlotHeader fit in the Header Size?
        assert!(SlotHeader::SIZE <= HEADER_SIZE);
    }

    #[test]
    fn max_data() {
        let manager = SlotManager::<4>::new(256 * 1024);
        let max_data = manager.max_data_size();

        // Magic number: 256K - HEADER_SIZE (1K) - MAX_SEGMENTS (16K)
        assert_eq!(max_data, (256 - 1 - 16) * 1024);
    }

    #[test]
    #[should_panic(expected = "assertion failed: slot_size > baseline")]
    fn bad_data() {
        let _manager = SlotManager::<4>::new(0);
    }

    #[tokio::test]
    async fn empty_flash() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();
        let mut flash = Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        assert_eq!(
            OldestReport {
                slot_idx: 0,
                next_seq_no: 0
            },
            mgr.find_oldest_slot(&mut flash, &mut scratch)
                .await
                .unwrap()
        );
    }

    #[tokio::test]
    async fn partial_empty() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();
        let mut flash = Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let mut scr = [0_u8; 1024];
        // 0, 1, 2, X
        for idx in 0..3 {
            #[allow(clippy::cast_possible_truncation)]
            make_a_slot(idx as u32, &mut scr);
            flash
                .write_from(SLOT_SIZE * idx, &scr[..SlotHeader::SIZE])
                .await
                .unwrap();

            let next = mgr
                .find_oldest_slot(&mut flash, &mut scratch)
                .await
                .unwrap();

            assert_eq!(
                next,
                OldestReport {
                    slot_idx: idx + 1,
                    next_seq_no: idx as u32 + 1
                }
            );
        }

        // 0, 1, 2, 3
        make_a_slot(3, &mut scr);
        flash
            .write_from(SLOT_SIZE * 3, &scr[..SlotHeader::SIZE])
            .await
            .unwrap();

        let next = mgr
            .find_oldest_slot(&mut flash, &mut scratch)
            .await
            .unwrap();
        assert_eq!(
            next,
            OldestReport {
                slot_idx: 0,
                next_seq_no: 4
            }
        );

        assert_snapshot!(flash.dump_to_string());
    }

    #[tokio::test]
    async fn basic_start() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();
        let mut flash = Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let init = mgr.app_boot_status(&mut flash, &mut scratch).await.unwrap();
        assert_eq!(init, AppBootStatus::Idle);

        let start = mgr.start(&mut flash, &mut scratch, 128, 64).await.unwrap();

        assert_eq!(
            start,
            ActiveStatus {
                segment_size: 128,
                slot_size: SLOT_SIZE,
                firmware_slot_idx: 0,
                total_firmware_segments: 64,
                remaining_firmware_segments: 64,
                parity_slot_idx: 1,
                total_parity_segments: 16384,
                remaining_parity_segments: 16384,
            }
        );

        let blank_to_start = flash.dump_to_string();
        assert_snapshot!(blank_to_start);
    }

    #[tokio::test]
    async fn start_write_all() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();
        let mut flash = Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let init = mgr.app_boot_status(&mut flash, &mut scratch).await.unwrap();
        assert_eq!(init, AppBootStatus::Idle);

        let mut start = mgr.start(&mut flash, &mut scratch, 128, 64).await.unwrap();

        assert_eq!(
            start,
            ActiveStatus {
                segment_size: 128,
                slot_size: SLOT_SIZE,
                firmware_slot_idx: 0,
                total_firmware_segments: 64,
                remaining_firmware_segments: 64,
                parity_slot_idx: 1,
                total_parity_segments: 16384,
                remaining_parity_segments: 16384,
            }
        );

        let blank_to_start = flash.dump_to_string();
        assert_snapshot!(blank_to_start);

        let mut rand_val = 1_234_567_890_u32;
        let mut bad_rand = || {
            let a = rand_val.wrapping_mul(31421);
            let b = a.wrapping_add(6927);
            let c = b.rotate_left(7);
            rand_val = c;
            b
        };

        let mut buf = [0_u8; 128];

        for i in 0..(64 + 32) {
            buf.iter_mut().for_each(|b| *b = bad_rand() as u8);
            start
                .write_segment(&mut flash, &mut scratch, i + 1, &buf)
                .await
                .unwrap();
        }

        let wrote_all = flash.dump_to_string();
        assert_snapshot!(wrote_all.clone());

        // Reset RNG - write the SAME values to everything, make sure we're still
        // okay
        rand_val = 1_234_567_890_u32;
        let mut bad_rand = || {
            let a = rand_val.wrapping_mul(31421);
            let b = a.wrapping_add(6927);
            let c = b.rotate_left(7);
            rand_val = c;
            b
        };
        for i in 0..(64 + 32) {
            buf.iter_mut().for_each(|b| *b = bad_rand() as u8);
            start
                .write_segment(&mut flash, &mut scratch, i + 1, &buf)
                .await
                .unwrap();
        }

        let wrote_all_2 = flash.dump_to_string();
        assert_eq!(wrote_all, wrote_all_2);

        // Writing more data fails
        let res = start
            .write_segment(&mut flash, &mut scratch, 1 + (64 + 16384), &buf)
            .await;
        assert_eq!(res, Err(SpiFlashError::OutOfBounds));
    }

    #[allow(clippy::too_many_lines)]
    #[tokio::test]
    async fn right_boot_status() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();

        // Sequence Number: 0-9/a-z like above
        // Firmware or Parity: f or p
        // status: `.`: complete, `~`: in progress, `!`: aborted
        let cases = [
            // Happy path "in progress" case
            (
                "0f. 1f~ 2p~ yf!", // Start
                "0f. 1f~ 2p~ yf!", // After
                // Status
                AppBootStatus::InProgress(ActiveStatus {
                    segment_size: 128,
                    slot_size: SLOT_SIZE,
                    firmware_slot_idx: 1,
                    parity_slot_idx: 2,
                    // TODO!!!!!
                    total_firmware_segments: 456,
                    remaining_firmware_segments: 456,
                    total_parity_segments: 456,
                    remaining_parity_segments: 456,
                }),
            ),
            // In progress around a wraparound
            (
                "2p~ yf! 0f. 1f~", // Start
                "2p~ yf! 0f. 1f~", // After
                // Status
                AppBootStatus::InProgress(ActiveStatus {
                    segment_size: 128,
                    slot_size: SLOT_SIZE,
                    firmware_slot_idx: 3,
                    parity_slot_idx: 0,
                    // TODO!!!!!
                    total_firmware_segments: 456,
                    remaining_firmware_segments: 456,
                    total_parity_segments: 456,
                    remaining_parity_segments: 456,
                }),
            ),
            // In progress with a seq num wraparound
            (
                "yf. 0f~ 1p~ xf!", // Start
                "yf. 0f~ 1p~ xf!", // After
                // Status
                AppBootStatus::InProgress(ActiveStatus {
                    segment_size: 128,
                    slot_size: SLOT_SIZE,
                    firmware_slot_idx: 1,
                    parity_slot_idx: 2,
                    // TODO!!!!!
                    total_firmware_segments: 456,
                    remaining_firmware_segments: 456,
                    total_parity_segments: 456,
                    remaining_parity_segments: 456,
                }),
            ),
            // Bad case (aliased seq no), DOES abort in progress
            // items
            (
                "0f. 1f~ 1p~ xf!", // Start
                "0f. 1f! 1p! xf!", // After
                // Status
                AppBootStatus::Idle,
            ),
        ];

        fn txt2arr(s: &str) -> [Option<SlotHeader>; 4] {
            const ONE: Option<SlotHeader> = None;
            let mut out = [ONE; 4];
            let s = s.split_whitespace().collect::<Vec<_>>();

            for (o, s) in out.iter_mut().zip(s) {
                let &[seq, kind, stat] = s.as_bytes() else {
                    assert_eq!(s, "N");
                    *o = None;
                    continue;
                };
                let seq_no = match seq {
                    v @ b'0'..=b'9' => {
                        // Treat single-digit numbers as u32s.
                        (v - b'0') as u32
                    }
                    v @ b'a'..=b'z' => {
                        // Treat lowercase letters as the last values before wraparound,
                        // so `z` == u32::MAX
                        let n_back = (b'z' - v) as u32;
                        u32::MAX - n_back
                    }
                    _ => unreachable!(),
                };
                let seq_no = SequenceNumber(seq_no);
                let kind = match kind {
                    b'f' => Kind::Firmware,
                    b'p' => Kind::Parity,
                    _ => unreachable!(),
                };
                let ext_status = match stat {
                    b'.' => WriteExtStatus::Complete,
                    b'!' => WriteExtStatus::Aborted,
                    b'~' => WriteExtStatus::InProgress,
                    _ => unreachable!(),
                };

                let int_status = match ext_status {
                    WriteExtStatus::InProgress => WriteIntStatus::InProgress,
                    WriteExtStatus::Aborted => WriteIntStatus::InProgress,
                    WriteExtStatus::Complete => WriteIntStatus::Complete,
                };

                let bootstat = match ext_status {
                    WriteExtStatus::InProgress => BootOutcome::Untested,
                    WriteExtStatus::Aborted => BootOutcome::Untested,
                    WriteExtStatus::Complete => BootOutcome::Successful,
                };

                *o = Some(SlotHeader {
                    kind,
                    seq_no,
                    segment_size: SegmentSize(128),
                    num_segments: NumberOfSegments(456),
                    write_ext_status: ext_status,
                    write_int_status: int_status,
                    boot_outcome: bootstat,
                });
            }

            out
        }

        // let mut scratch = Box::leak(Box::new([0_u8; 1024]));
        let mut flash = Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let mut scr = [0_u8; 1024];

        for (init, after, outcome) in cases {
            // println!("{init}");
            let init_cmp = txt2arr(init);

            flash.erase_all().await.unwrap();
            for (i, s) in init_cmp
                .iter()
                .enumerate()
                .filter_map(|(i, s)| s.as_ref().map(|s| (i, s)))
            {
                s.write_to_bytes(&mut scr).unwrap();
                flash
                    .write_from(SLOT_SIZE * i, &scr[..SlotHeader::SIZE])
                    .await
                    .unwrap();
            }

            let bs = mgr.app_boot_status(&mut flash, &mut scratch).await.unwrap();
            println!("{:?} {:?} {:?} {:?}", init, after, outcome, bs);
            assert_eq!(bs, outcome);

            // Now read the headers from flash, and see if they match the expected "after"
            let mut hdrs2: [Option<SlotHeader>; 4] = [None, None, None, None];

            for (i, slot) in hdrs2.iter_mut().enumerate() {
                let address = i * SLOT_SIZE;
                *slot = read_header_from_slot(&mut flash, &mut scratch, address)
                    .await
                    .unwrap();
            }

            let after = txt2arr(after);
            assert_eq!(hdrs2, after);
        }
    }

    #[tokio::test]
    async fn right_placement() {
        const SLOT_SIZE: usize = 256 * 1024;
        let mut scratch = ScratchRam::new();

        // CSpell:disable
        // (Current Status, Next Position, Next Sequence Number)
        let cases = [
            // Some (or all) slots are empty - the first empty slot is used
            (b"NNNN", 0, 0),
            (b"0NNN", 1, 1),
            (b"01NN", 2, 2),
            (b"012N", 3, 3),
            // Works the same whether you have/haven't erased the desired page
            (b"0123", 0, 4),
            (b"N123", 0, 4),
            (b"4123", 1, 5),
            (b"4N23", 1, 5),
            (b"4523", 2, 6),
            (b"45N3", 2, 6),
            (b"4563", 3, 7),
            (b"456N", 3, 7),
            (b"4567", 0, 8),
            (b"N567", 0, 8),
            // Getting close to the seq_no wrapping...
            (b"uvwx", 0, u32::MAX - 1),
            // There it goes!
            (b"yvwx", 1, 0),
            (b"y0wx", 2, 1),
            (b"y01x", 3, 2),
            (b"vwxy", 0, 0),
            // Also, we treat u32::MAX the same as "None"
            (b"zzzz", 0, 0),
            // What if there are two weird blank spots?
            // Show the "healing" progression
            (b"5NaN", 1, 6),
            (b"56aN", 2, 7),
            (b"567N", 3, 8),
            (b"5678", 0, 9),
            (b"9678", 1, 10),
            // Additional weirdo cases
            (b"5NNN", 1, 6),
            (b"N5NN", 2, 6),
            (b"NN5N", 3, 6),
            (b"NNN5", 0, 6),
            // Dupe index
            (b"233N", 2, 4),
        ];
        // CSpell:enable

        // Use text instead of `[None, None, None, Some(4)]` or so to make
        // it more readable
        fn txt2arr(s: &[u8; 4]) -> [Option<u32>; 4] {
            let mut out = [None; 4];
            s.iter().zip(out.iter_mut()).for_each(|(s, o)| {
                *o = match *s {
                    b'N' => {
                        // N => None, same as a totally erased flash page
                        None
                    }
                    v @ b'0'..=b'9' => {
                        // Treat single-digit numbers as u32s.
                        Some((v - b'0') as u32)
                    }
                    v @ b'a'..=b'z' => {
                        // Treat lowercase letters as the last values before wraparound,
                        // so `z` == u32::MAX
                        let n_back = (b'z' - v) as u32;
                        let idx = u32::MAX - n_back;
                        Some(idx)
                    }
                    _ => unreachable!(),
                }
            });
            out
        }

        let mut flash = Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let mut scr = [0_u8; 1024];

        for (state, next_idx, next_seq) in cases {
            print!("{} - ", core::str::from_utf8(state).unwrap());

            let arr = txt2arr(state);
            // println!("{:08X?}", arr);
            flash.erase_all().await.unwrap();
            for (i, s) in arr
                .into_iter()
                .enumerate()
                .filter_map(|(i, s)| s.map(|s| (i, s)))
            {
                make_a_slot(s, &mut scr);
                flash
                    .write_from(SLOT_SIZE * i, &scr[..SlotHeader::SIZE])
                    .await
                    .unwrap();
            }

            let rpt = mgr
                .find_oldest_slot(&mut flash, &mut scratch)
                .await
                .unwrap();
            // println!("{rpt:?}");
            assert_eq!(
                rpt,
                OldestReport {
                    slot_idx: next_idx,
                    next_seq_no: next_seq
                }
            );
        }
    }
}
