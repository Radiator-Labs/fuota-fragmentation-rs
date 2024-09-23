use core::num::NonZeroU32;

#[cfg(not(feature = "matrixreconstructor"))]
use bitvec::array::BitArray;
use layout::{
    segment_status_table::{MAX_SEGMENTS, MAX_SEGMENT_SIZE},
    total_status, BootOutcome, FlashRepr, FlashReprError, Kind, SlotHeader, WriteExtStatus,
    WriteIntStatus, HEADER_SIZE,
};

use crate::spi_flash::{SpiFlash, SpiFlashError};

#[cfg(not(feature = "matrixreconstructor"))]
use crate::bitcache::BitCache;

mod firmware;
mod fs;
pub(crate) mod layout;
pub(crate) use fs::indexed_headers;

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
pub struct ScratchRam {
    /// One array to hold the list of all received firmware segments
    #[cfg(not(feature = "matrixreconstructor"))]
    pub(crate) received_firmware_scratch: BitCache,
    /// One array to hold the list of all received parity segments
    #[cfg(not(feature = "matrixreconstructor"))]
    pub(crate) received_parity_scratch: BitCache,
    /// One array to hold a single "does this parity segment correspond
    ///   to a given data segment" row.
    #[cfg(not(feature = "matrixreconstructor"))]
    pub(crate) parity_mask_scratch: BitArray<[u8; MAX_SEGMENTS / 8]>,

    /// An array to page-in parity data when loading a [`BitCache`]
    /// from flash data
    #[cfg(not(feature = "matrixreconstructor"))]
    pub(crate) parity_temp_page: [u8; Self::PARITY_TEMP_LEN],

    /// A single read segment
    pub(crate) firmware_rd_scratch: [u8; MAX_SEGMENT_SIZE],
    /// A single write segment
    #[cfg(not(feature = "matrixreconstructor"))]
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
    #[cfg(not(feature = "matrixreconstructor"))]
    const PARITY_TEMP_LEN: usize = 128;

    /// Create a new, empty (zero-filled) `ScratchRam`
    #[must_use]
    pub const fn new() -> Self {
        Self {
            #[cfg(not(feature = "matrixreconstructor"))]
            received_firmware_scratch: BitCache::new(),
            #[cfg(not(feature = "matrixreconstructor"))]
            received_parity_scratch: BitCache::new(),
            #[cfg(not(feature = "matrixreconstructor"))]
            parity_mask_scratch: BitArray::ZERO,
            firmware_rd_scratch: [0_u8; MAX_SEGMENT_SIZE],
            #[cfg(not(feature = "matrixreconstructor"))]
            firmware_wr_scratch: [0_u8; MAX_SEGMENT_SIZE],
            header_scratch: [0_u8; SlotHeader::SIZE],
            #[cfg(not(feature = "matrixreconstructor"))]
            parity_temp_page: [0_u8; Self::PARITY_TEMP_LEN],
        }
    }
}

/// Error type used by [`SlotManager`] and [`ActiveStatus`]
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

    /// We attempted to check for firmware, but slot contained something else
    CheckFailNotFirmware,
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

/// Outcome of a call to `[SlotManager::bl_boot_status]`
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[derive(Debug, PartialEq, Eq)]
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

/// The `SlotManager` is the primary interface used to query the state of the flash contents
///
/// The `SlotManager` is used in both application and bootloader.
pub struct SlotManager<const N: usize> {
    pub(crate) slot_size: usize,
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

    /// Determine the current state of the external flash, as a bootloader
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    pub async fn bl_boot_status<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<BlBootStatus, ManagerError<T::Error>> {
        let headers = self.load_headers(flash, scratch).await?;
        for (idx, header) in indexed_headers(&headers) {
            if header.kind != Kind::Firmware {
                continue;
            }

            match total_status(header) {
                layout::TotalStatus::BootloadWriteInProgress => {
                    return Ok(BlBootStatus::IncompleteInternal { idx: idx as u32 })
                }
                layout::TotalStatus::FirstBootPendingAck => {
                    return Ok(BlBootStatus::FailedLoad { idx: idx as u32 })
                }
                layout::TotalStatus::ConfirmedImage
                | layout::TotalStatus::RejectedImage
                | layout::TotalStatus::InvalidNeedsErase
                | layout::TotalStatus::BlankSlot
                | layout::TotalStatus::AppWriteInProgress
                | layout::TotalStatus::AppWriteAborted => {}
            }
        }

        Ok(BlBootStatus::Idle)
    }
}

pub struct Slot {
    idx: usize,
    size: usize,
    segment_size: Option<NonZeroU32>,
}

impl Slot {
    pub(crate) fn index(&self) -> usize {
        self.idx
    }

    /// Set the "Write External Status" field of the given index to "Aborted".
    ///
    /// NOTE: Does NOT check the prior contents of the field.
    ///
    /// # Errors
    /// Reports errors for Flash and SPI bus
    pub async fn mark_ext_status_aborted<T: SpiFlash>(
        &mut self,
        flash: &mut T,
    ) -> Result<(), ManagerError<T::Error>> {
        let address = self.idx * self.size;
        let mut buf = [0_u8; WriteExtStatus::SIZE];
        WriteExtStatus::Aborted.write_to_bytes(&mut buf)?;
        flash
            .write_from(address + SlotHeader::WRITE_EXT_STATUS_OFFSET, &buf)
            .await?;
        Ok(())
    }

    /// Set the "Write External Status" field of the given index to "Complete".
    ///
    /// NOTE: Does NOT check the prior contents of the field.
    ///
    /// # Errors
    /// Reports errors for Flash and SPI bus
    pub async fn mark_ext_status_complete<T: SpiFlash>(
        &mut self,
        flash: &mut T,
    ) -> Result<(), ManagerError<T::Error>> {
        let address = self.idx * self.size;
        let mut buf = [0_u8; WriteExtStatus::SIZE];
        WriteExtStatus::Complete.write_to_bytes(&mut buf)?;
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
    pub async fn mark_int_status_complete<T: SpiFlash>(
        &mut self,
        flash: &mut T,
    ) -> Result<(), ManagerError<T::Error>> {
        let address = self.idx * self.size;
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
    pub async fn mark_boot_outcome_successful<T: SpiFlash>(
        &mut self,
        flash: &mut T,
    ) -> Result<(), ManagerError<T::Error>> {
        let address = self.idx * self.size;
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
    pub async fn mark_boot_outcome_unsuccessful<T: SpiFlash>(
        &mut self,
        flash: &mut T,
    ) -> Result<(), ManagerError<T::Error>> {
        let address = self.idx * self.size;
        let mut buf = [0_u8; BootOutcome::SIZE];
        BootOutcome::Unsuccessful.write_to_bytes(&mut buf)?;
        flash
            .write_from(address + SlotHeader::BOOT_OUTCOME_OFFSET, &buf)
            .await?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    extern crate std;
    use std::prelude::rust_2021::*;

    use super::*;

    #[test]
    #[should_panic(expected = "assertion failed: slot_size > baseline")]
    fn bad_data() {
        let _manager = SlotManager::<4>::new(0);
    }
}
