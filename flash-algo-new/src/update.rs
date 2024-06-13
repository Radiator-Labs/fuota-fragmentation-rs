pub use naive::{SegmentOutcome, Updater};

use crate::{
    manager::{
        indexed_headers,
        layout::{
            segment_status_table::MAX_SEGMENT_SIZE, total_status, Kind, NumberOfSegments,
            SegmentSize, TotalStatus, WriteExtStatus, DATA_REGION_OFFSET,
        },
        ManagerError, ScratchRam, SlotManager,
    },
    spi_flash::SpiFlash,
};

mod naive;

impl<const N: usize> SlotManager<N> {
    fn is_reasonably_sized<E>(
        &self,
        segment_size: u32,
        firmware_segments: u32,
    ) -> Result<(), ManagerError<E>> {
        if segment_size > MAX_SEGMENT_SIZE as _ {
            return Err(ManagerError::SegmentsTooLarge);
        }
        if segment_size.saturating_mul(firmware_segments)
            > self.slot_size.saturating_sub(DATA_REGION_OFFSET) as _
        {
            return Err(ManagerError::TooManySegments);
        }
        Ok(())
    }

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

    /// Mark any in-progress firmware writes as Aborted. Used for error case recovery.
    async fn cancel_all_ext_pending<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<(), ManagerError<T::Error>> {
        let headers = self.load_headers(flash, scratch).await?;

        for (i, hdr) in indexed_headers(&headers) {
            if hdr.write_ext_status == WriteExtStatus::InProgress {
                let mut slot = self.open(i);
                slot.mark_ext_status_aborted(flash).await?;
            }
        }

        Ok(())
    }

    pub async fn try_recover<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<Option<Updater>, ManagerError<T::Error>> {
        let result = self.try_recover_inner(flash, scratch).await?;
        if result.is_none() {
            self.cancel_all_ext_pending(flash, scratch).await?;
        }
        Ok(result)
    }

    async fn try_recover_inner<T: SpiFlash>(
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
