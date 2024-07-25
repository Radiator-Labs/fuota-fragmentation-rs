use crate::{
    fragmentation::get_parity_matrix_row,
    layout::{total_status, Kind, NumberOfSegments, SegmentSize, TotalStatus, DATA_REGION_OFFSET},
    manager::{
        indexed_headers,
        layout::segment_status_table::{DATA_NOT_WRITTEN, DATA_WRITTEN},
        ManagerError, ScratchRam, Slot, SlotManager,
    },
    spi_flash::{SpiFlash, SpiFlashError},
};

use super::SegmentOutcome;

/// The outcome of a call to [`Updater::write_segment_internal`]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[derive(Debug, PartialEq, Eq, Hash)]
enum WriteSegmentOutcome {
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

/// [`Updater`] is the main application interface while a single FUOTA process is active.
///
/// It contains methods for receiving and checking data.
pub struct Updater {
    firmware_slot: Slot,
    total_firmware_segments: u32,
    remaining_firmware_segments: u32,

    parity_slot: Slot,
    total_parity_segments: u32,
    remaining_parity_segments: u32,
}

impl Updater {
    /// Is the current FUOTA process complete?
    ///
    /// NOTE: This only checks if all firmware segments have been received
    #[must_use]
    pub fn is_complete(&self) -> bool {
        self.remaining_firmware_segments == 0
    }

    /// Attempt to write a received segment
    ///
    /// This may either be a firmware segment, or a parity segment.
    ///
    /// This function DOES handle duplicate reception of a segment, AS LONG AS
    /// the duplicate data matches the previously received data.
    ///
    /// On success, returns a `[SegmentOutcome]` indicating the potential next steps.
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    ///
    /// # Panics
    /// Panics if buffer sizes are not equal.
    pub async fn handle_segment<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
        segment_1idx: u32,
        bytes: &[u8],
    ) -> Result<SegmentOutcome, ManagerError<T::Error>> {
        let outcome = self
            .write_segment_internal(flash, &mut scratch.firmware_rd_scratch, segment_1idx, bytes)
            .await?;

        match outcome {
            WriteSegmentOutcome::Consumed => Ok(SegmentOutcome::Consumed),
            WriteSegmentOutcome::ConsumedMaybeParity => {
                while self.repair_step(flash, scratch).await?.is_some() {}
                if self.is_complete() {
                    Ok(SegmentOutcome::FirmwareComplete)
                } else {
                    Ok(SegmentOutcome::Consumed)
                }
            }
            WriteSegmentOutcome::FirmwareComplete => Ok(SegmentOutcome::FirmwareComplete),
        }
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
        let (slot, segment_0idx) = if segment_1idx == 0 {
            #[cfg(feature = "rtt_target")]
            rprintln!(
                "write_segment: Out of bounds segment_1idx {} == 0",
                segment_1idx
            );

            // segments are 1-indexed
            return Err(SpiFlashError::OutOfBounds);
        } else if segment_1idx <= self.total_firmware_segments {
            // firmware segments come first
            (&mut self.firmware_slot, segment_1idx - 1)
        } else if segment_1idx <= (self.total_firmware_segments + self.total_parity_segments) {
            // Then parity segments
            (
                &mut self.parity_slot,
                segment_1idx - 1 - self.total_firmware_segments,
            )
        } else {
            #[cfg(feature = "rtt_target")]
            rprintln!(
                "write_segment: Out of bounds segment_1idx {} > (total_firmware_segments {} + total_parity_segments {})",
                segment_1idx,
                self.total_firmware_segments,
                self.total_parity_segments
            );
            return Err(SpiFlashError::OutOfBounds);
        };

        // Have we previously written this segment before?
        let segment_status = slot.segment_status(segment_0idx as usize, flash).await?;

        match segment_status {
            DATA_NOT_WRITTEN => {
                // Not written, we're good
            }
            #[allow(clippy::indexing_slicing)] // TODO: eliminate possible panic
            DATA_WRITTEN => {
                // Uh oh, was written, does the existing value match the current data?
                slot.read_segment(segment_0idx as usize, read_scratch, flash)
                    .await?;
                assert_eq!(&read_scratch[..bytes.len()], bytes);
                // All good!
                return Ok(WriteSegmentOutcome::Consumed);
            }
            #[allow(clippy::panic)] // TODO: eliminate this panic
            _ => panic!(),
        }

        slot.write_segment(segment_0idx as usize, bytes, flash)
            .await?;

        if core::ptr::eq(slot, &self.firmware_slot) {
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
    #[allow(clippy::too_many_lines)]
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
            #[cfg(feature = "rtt_target")]
            rprintln!("SKIP REPAIR: ALREADY DONE");
            return Ok(None);
        }
        // If we don't have any parity segments, nothing to do
        if self.remaining_parity_segments == self.total_parity_segments {
            #[cfg(feature = "rtt_target")]
            rprintln!("SKIP REPAIR: NO PARITY");
            return Ok(None);
        }

        let segment_size = self.firmware_slot.segment_size(flash).await?;
        #[allow(clippy::indexing_slicing)] // TODO: eliminate possible panic
        let firmware_rd_scratch = &mut scratch.firmware_rd_scratch[..segment_size];
        #[allow(clippy::indexing_slicing)] // TODO: eliminate possible panic
        let firmware_wr_scratch = &mut scratch.firmware_wr_scratch[..segment_size];

        // Read which firmware and parity blocks we have.
        let received_firmware_scratch = self
            .firmware_slot
            .load_status_array(
                &mut scratch.received_firmware_scratch,
                &mut scratch.parity_temp_page,
                flash,
            )
            .await?;
        let received_parity_scratch = self
            .parity_slot
            .load_status_array(
                &mut scratch.received_parity_scratch,
                &mut scratch.parity_temp_page,
                flash,
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
            #[allow(clippy::cast_possible_truncation)]
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
                .take(self.total_firmware_segments as _);
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

                #[allow(unused_variables)]
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
                        #[cfg(feature = "rtt_target")]
                        rprintln!("{} -> two missing {}, {}", parity_i, old_i, firmware_i);
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
                self.parity_slot
                    .read_segment(parity_i, firmware_wr_scratch, flash)
                    .await?;

                // Now XOR the contents of each of the relevant frames
                for (flash_index, relevant) in parity_mask_scratch.clone().enumerate() {
                    // If this ISN'T a relevant frame, skip it.
                    // If it IS relevant BUT is also the missing frame, skip it.
                    if (flash_index == fwi) || !relevant {
                        continue;
                    }

                    self.firmware_slot
                        .read_segment(flash_index, firmware_rd_scratch, flash)
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
                #[allow(clippy::cast_possible_truncation)]
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

    /// Perform a final check that a FUOTA process is complete.
    ///
    /// If it checks out, the firmware and parity slots will be marked as
    /// having a complete ext write status.
    ///
    /// Returns the slot of the completed firmware
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    ///
    /// # Panics
    /// Panics if write to buffer fails. Need to eliminate unwrap.
    pub async fn check_and_mark_done<T: SpiFlash>(
        mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<usize, ManagerError<T::Error>> {
        if self.remaining_firmware_segments != 0 {
            return Err(ManagerError::CheckFailNotDone);
        }

        let header = self
            .firmware_slot
            .load_header(flash, scratch)
            .await?
            .ok_or(ManagerError::UnexpectedMissingHeader)?;
        self.firmware_slot
            .crc_valid(&header, flash, scratch)
            .await?;

        self.firmware_slot.mark_ext_status_complete(flash).await?;
        self.parity_slot.mark_ext_status_complete(flash).await?;

        Ok(self.firmware_slot.index())
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
            self.session.total_firmware_segments - self.remaining_firmware_segments
        } else {
            0
        }
    }
}

impl<const N: usize> SlotManager<N> {
    pub async fn start_update<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
        segment_size: u32,
        firmware_segments: u32,
    ) -> Result<Updater, ManagerError<T::Error>> {
        self.is_reasonably_sized(segment_size, firmware_segments)?;

        let parity_segments =
            (self.slot_size.saturating_sub(DATA_REGION_OFFSET) as u32) / segment_size;

        let (mut firmware_slot, mut parity_slot) = self.alloc_slotpair(flash, scratch).await?;
        firmware_slot.set_kind(Kind::Firmware, flash).await?;
        firmware_slot
            .set_layout(
                NumberOfSegments(firmware_segments),
                SegmentSize(segment_size),
                flash,
            )
            .await?;
        parity_slot.set_kind(Kind::Parity, flash).await?;
        parity_slot
            .set_layout(
                NumberOfSegments(parity_segments),
                SegmentSize(segment_size),
                flash,
            )
            .await?;

        Ok(Updater {
            firmware_slot,
            total_firmware_segments: firmware_segments,
            remaining_firmware_segments: firmware_segments,
            parity_slot,
            total_parity_segments: parity_segments,
            remaining_parity_segments: parity_segments,
        })
    }

    pub(crate) async fn try_recover_inner<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<Option<Updater>, ManagerError<T::Error>> {
        let headers = self.load_headers(flash, scratch).await?;

        let mut newest: Option<(usize, &crate::manager::layout::SlotHeader)> = None;
        let mut second_newest = None;

        for (i, hdr) in indexed_headers(&headers) {
            if let Some(newest_inner) = newest {
                if newest_inner.1.seq_no < hdr.seq_no {
                    second_newest = newest;
                    newest = Some((i, hdr));
                } else if let Some(second_newest_inner) = second_newest {
                    if second_newest_inner.1.seq_no < hdr.seq_no {
                        second_newest = Some((i, hdr));
                    }
                } else {
                    second_newest = Some((i, hdr));
                }
            } else {
                newest = Some((i, hdr));
            }
        }

        let Some(newest) = newest else {
            return Ok::<_, ManagerError<T::Error>>(None);
        };
        let Some(second_newest) = second_newest else {
            return Ok(None);
        };

        // Basic sanity checks
        if total_status(newest.1) != TotalStatus::AppWriteInProgress {
            return Ok(None);
        }
        if newest.1.kind != Kind::Parity {
            return Ok(None);
        }
        if total_status(second_newest.1) != TotalStatus::AppWriteInProgress {
            return Ok(None);
        }
        if second_newest.1.kind != Kind::Firmware {
            return Ok(None);
        }
        if newest.1.segment_size != second_newest.1.segment_size {
            return Ok(None);
        }
        if self
            .is_reasonably_sized::<T::Error>(
                second_newest.1.segment_size.0,
                second_newest.1.num_segments.0,
            )
            .is_err()
        {
            return Ok(None);
        }

        // Now check the "other" items to see if they need remediation
        for (i, hdr) in indexed_headers(&headers) {
            if i == newest.0 || i == second_newest.0 {
                continue;
            }

            match total_status(hdr) {
                TotalStatus::AppWriteInProgress => {
                    let mut slot = self.open(i);
                    slot.mark_ext_status_aborted(flash).await?;
                }
                TotalStatus::BootloadWriteInProgress | TotalStatus::InvalidNeedsErase => {
                    let mut slot = self.open(i);
                    slot.clear(flash).await?;
                }
                TotalStatus::BlankSlot
                | TotalStatus::AppWriteAborted
                | TotalStatus::FirstBootPendingAck
                | TotalStatus::ConfirmedImage
                | TotalStatus::RejectedImage => {}
            }
        }

        let mut firmware_slot = self.open(second_newest.0);
        let mut parity_slot = self.open(newest.0);

        let firmware_status = firmware_slot
            .load_status_array(
                &mut scratch.received_firmware_scratch,
                &mut scratch.parity_temp_page,
                flash,
            )
            .await?;
        let firmware_done = firmware_status.filter(|v| *v).count() as u32;
        let parity_status = parity_slot
            .load_status_array(
                &mut scratch.received_parity_scratch,
                &mut scratch.parity_temp_page,
                flash,
            )
            .await?;
        let parity_done = parity_status.filter(|v| *v).count() as u32;

        Ok(Some(Updater {
            firmware_slot,
            total_firmware_segments: second_newest.1.num_segments.0,
            remaining_firmware_segments: second_newest.1.num_segments.0 - firmware_done,
            parity_slot,
            total_parity_segments: newest.1.num_segments.0,
            remaining_parity_segments: newest.1.num_segments.0 - parity_done,
        }))
    }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use std::prelude::rust_2021::*;

    use crate::{manager::SlotManager, spi_flash::SpiFlashError, testutils::heap_flash::Flash};

    use super::*;
    use insta::assert_snapshot;

    #[tokio::test]
    async fn basic_start() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();
        let mut flash = Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let init = mgr.try_recover(&mut flash, &mut scratch).await.unwrap();
        assert!(init.is_none());

        let start = mgr
            .start_update(&mut flash, &mut scratch, 128, 64)
            .await
            .unwrap();

        assert_eq!(start.firmware_slot.index(), 0);
        assert_eq!(start.parity_slot.index(), 1);
        assert_eq!(start.total_firmware_segments, 64);
        assert_eq!(start.total_parity_segments, 1912);
        assert_eq!(
            start.total_firmware_segments,
            start.remaining_firmware_segments
        );
        assert_eq!(start.total_parity_segments, start.remaining_parity_segments);

        let blank_to_start = flash.dump_to_string();
        assert_snapshot!(blank_to_start);
    }

    #[tokio::test]
    async fn start_write_all() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();
        let mut flash = Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let init = mgr.try_recover(&mut flash, &mut scratch).await.unwrap();
        assert!(init.is_none());

        let mut start = mgr
            .start_update(&mut flash, &mut scratch, 128, 64)
            .await
            .unwrap();

        assert_eq!(start.firmware_slot.index(), 0);
        assert_eq!(start.parity_slot.index(), 1);
        assert_eq!(start.total_firmware_segments, 64);
        assert_eq!(start.total_parity_segments, 1912);
        assert_eq!(
            start.total_firmware_segments,
            start.remaining_firmware_segments
        );
        assert_eq!(start.total_parity_segments, start.remaining_parity_segments);

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
                .handle_segment(&mut flash, &mut scratch, i + 1, &buf)
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
                .handle_segment(&mut flash, &mut scratch, i + 1, &buf)
                .await
                .unwrap();
        }

        let wrote_all_2 = flash.dump_to_string();
        assert_eq!(wrote_all, wrote_all_2);

        // Writing more data fails
        let res = start
            .handle_segment(&mut flash, &mut scratch, 1 + (64 + 16384), &buf)
            .await;
        assert_eq!(res, Err(ManagerError::Spi(SpiFlashError::OutOfBounds)));
    }
}
