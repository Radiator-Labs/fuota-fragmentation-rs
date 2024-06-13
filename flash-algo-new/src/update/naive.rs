use crate::{manager::{layout::{segment_status_table::DATA_WRITTEN, DATA_REGION_OFFSET, WRITTEN_OFFSET, WRITTEN_SIZE}, ScratchRam, Slot}, spi_flash::{SpiFlash, SpiFlashError}};

/// The outcome of a call to [`Updater::write_segment_internal`]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[derive(Debug, PartialEq)]
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
struct Updater {
    pub(crate) slot_size: usize,
    pub(crate) segment_size: usize,

    //pub(crate) firmware_slot_idx: usize,
    pub(crate) firmware_slot: Slot,
    pub(crate) total_firmware_segments: u32,
    pub(crate) remaining_firmware_segments: u32,

    //pub(crate) parity_slot_idx: usize,
    pub(crate) parity_slot: Slot,
    pub(crate) total_parity_segments: u32,
    pub(crate) remaining_parity_segments: u32,
}

impl Updater {
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
    pub async fn handle_segment<T: SpiFlash>(
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
            (self.firmware_slot, segment_1idx - 1)
        } else if segment_1idx <= (self.total_firmware_segments + self.total_parity_segments) {
            // Then parity segments
            (
                self.parity_slot,
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

        // Do some sanity checking that this is all okay
        let written_in_range = (segment_0idx as usize) < WRITTEN_SIZE;
        let data_in_range = ((segment_0idx + 1) as usize * self.segment_size) <= self.slot_size;
        assert_eq!(self.segment_size, bytes.len());

        if !(written_in_range && data_in_range) {
            #[cfg(feature = "rtt_target")]
            rprintln!("write_segment: Out of bounds segment_0idx {}", segment_0idx);
            #[cfg(feature = "rtt_target")]
            rprintln!(
                "  written_in_range {}, idx < WRITTEN_SIZE {}",
                written_in_range,
                WRITTEN_SIZE
            );
            #[cfg(feature = "rtt_target")]
            rprintln!(
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
            #[allow(clippy::indexing_slicing)] // TODO: eliminate possible panic
            DATA_WRITTEN => {
                // Uh oh, was written, does the existing value match the current data?
                flash.read_to(data_start, read_scratch).await?;
                assert_eq!(&read_scratch[..bytes.len()], bytes);
                // All good!
                return Ok(WriteSegmentOutcome::Consumed);
            }
            #[allow(clippy::panic)] // TODO: eliminate this panic
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

        #[allow(clippy::indexing_slicing)] // TODO: eliminate possible panic
        let firmware_rd_scratch = &mut scratch.firmware_rd_scratch[..self.segment_size];
        #[allow(clippy::indexing_slicing)] // TODO: eliminate possible panic
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
