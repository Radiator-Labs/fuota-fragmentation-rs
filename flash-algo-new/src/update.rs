use crate::{
    manager::{
        indexed_headers,
        layout::{segment_status_table::MAX_SEGMENT_SIZE, WriteExtStatus, DATA_REGION_OFFSET},
        ManagerError, ScratchRam, SlotManager,
    },
    spi_flash::SpiFlash,
};

#[cfg(not(feature = "matrixreconstructor"))]
mod naive;
#[cfg(not(feature = "matrixreconstructor"))]
pub use naive::Updater;

#[cfg(feature = "matrixreconstructor")]
mod matrix;
#[cfg(feature = "matrixreconstructor")]
pub use matrix::Updater;

#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum SegmentOutcome {
    /// Frame was consumed, and no parity check is recommended at the moment.
    ///
    /// This typically means either:
    ///
    /// * We haven't finished receiving all firmware frames, AND there are no
    ///   parity frames
    /// * This was a complete duplicate frame (for example if this is a retry).
    Consumed,
    /// Firmware is complete, can be checked and finished.
    FirmwareComplete,
}

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
}
