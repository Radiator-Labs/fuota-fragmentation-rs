use core::cell::RefCell;

use bitvec::array::BitArray;
use parity_reconstruct::{
    BlockResult, DataStorage, MatrixStorage, ParityMatrix, ParityStorage, Reconstructor,
    ReconstructorData,
};

use crate::{
    bitcache::BitCache,
    fragmentation::get_parity_matrix_row,
    layout::{
        segment_status_table::{MAX_SEGMENTS, MAX_SEGMENT_SIZE},
        total_status, Kind, NumberOfSegments, SegmentSize, TotalStatus, DATA_REGION_OFFSET,
    },
    manager::{indexed_headers, ManagerError, ScratchRam, Slot, SlotManager},
    spi_flash::{SpiFlash, SpiFlashError},
};

use super::SegmentOutcome;

struct UpdaterMatrix {
    num_blocks: usize,
}

impl ParityMatrix<[u8; MAX_SEGMENTS / 8]> for UpdaterMatrix {
    fn row(&self, m: usize) -> BitArray<[u8; MAX_SEGMENTS / 8]> {
        if m < self.num_blocks {
            let mut result = BitArray::ZERO;
            result.set(m, true);
            result
        } else {
            let mut result = BitArray::ZERO;
            get_parity_matrix_row(
                (m - self.num_blocks + 1) as _,
                self.num_blocks as u32,
                &mut result,
            );
            result
        }
    }
}

struct UpdaterMatrixStorage<'a, 'b, T> {
    parity_slot: &'a RefCell<Slot>,
    offset: usize,
    max_l: usize,
    flash: &'a RefCell<&'b mut T>,
}

fn matrix_row_offset(i: usize) -> usize {
    let completes = i / 8;
    let partials = i % 8;

    completes * (completes + 1) * 4 + partials * (completes + 1)
}

impl<'a, 'b, const N: usize, T: SpiFlash> MatrixStorage<[u8; N]>
    for UpdaterMatrixStorage<'a, 'b, T>
{
    type Error = SpiFlashError<T::Error>;

    #[allow(clippy::await_holding_refcell_ref)]
    async fn set_row(
        &mut self,
        m: usize,
        mut data: bitvec::prelude::BitArray<[u8; N]>,
    ) -> Result<(), Self::Error> {
        assert!(m < self.max_l);
        let offset = self.offset + matrix_row_offset(m);
        let data_size = (m / 8) + 1;
        assert!(data_size <= N);

        // Inverting the diagonal makes it usable for reconstructing
        // the used array, by having 1 indicate not used and 0 used.
        // (the flash after clearing is 1)
        let bit = !data[m];
        data.set(m, bit);

        let mut flash = self.flash.borrow_mut();
        self.parity_slot
            .borrow_mut()
            .write_raw(offset, &data.as_raw_slice()[..data_size], *flash)
            .await?;
        Ok(())
    }

    #[allow(clippy::await_holding_refcell_ref)]
    async fn row(&mut self, m: usize) -> Result<bitvec::prelude::BitArray<[u8; N]>, Self::Error> {
        assert!(m < self.max_l);
        let offset = self.offset + matrix_row_offset(m);
        let data_size = (m / 8) + 1;
        assert!(data_size <= N);

        let mut result = BitArray::<[u8; N]>::ZERO;

        let mut flash = self.flash.borrow_mut();
        self.parity_slot
            .borrow_mut()
            .read_raw(offset, &mut result.as_raw_mut_slice()[..data_size], *flash)
            .await?;

        // Undo the inversion applied during writing
        let bit = !result[m];
        result.set(m, bit);
        Ok(result)
    }

    fn num_rows(&self) -> usize {
        self.max_l
    }
}

struct UpdaterParityStorage<'a, 'b, T> {
    parity_slot: &'a RefCell<Slot>,
    max_l: usize,
    flash: &'a RefCell<&'b mut T>,
}

impl<'a, 'b, T: SpiFlash> ParityStorage for UpdaterParityStorage<'a, 'b, T> {
    type Error = SpiFlashError<T::Error>;

    #[allow(clippy::await_holding_refcell_ref)]
    async fn store(&mut self, m: usize, data: &[u8]) -> Result<(), Self::Error> {
        assert!(m < self.max_l);

        let offset = m * data.len();
        let mut flash = self.flash.borrow_mut();
        self.parity_slot
            .borrow_mut()
            .write_raw(offset, data, *flash)
            .await?;

        Ok(())
    }

    #[allow(clippy::await_holding_refcell_ref)]
    async fn get(&mut self, m: usize, buf: &mut [u8]) -> Result<(), Self::Error> {
        assert!(m < self.max_l);

        let offset = m * buf.len();
        let mut flash = self.flash.borrow_mut();
        self.parity_slot
            .borrow_mut()
            .read_raw(offset, buf, *flash)
            .await?;

        Ok(())
    }
}

struct UpdaterDataStorage<'a, 'b, T> {
    firmware_slot: &'a mut Slot,
    flash: &'a RefCell<&'b mut T>,
}

impl<'a, 'b, T: SpiFlash> DataStorage for UpdaterDataStorage<'a, 'b, T> {
    type Error = SpiFlashError<T::Error>;

    #[allow(clippy::await_holding_refcell_ref)]
    async fn store(&mut self, m: usize, data: &[u8]) -> Result<(), Self::Error> {
        let mut flash = self.flash.borrow_mut();
        self.firmware_slot.write_segment(m, data, *flash).await?;
        Ok(())
    }

    #[allow(clippy::await_holding_refcell_ref)]
    async fn get(&mut self, m: usize, buf: &mut [u8]) -> Result<(), Self::Error> {
        let mut flash = self.flash.borrow_mut();
        self.firmware_slot.read_segment(m, buf, *flash).await?;
        Ok(())
    }
}

pub struct Updater {
    firmware_slot: Slot,
    parity_slot: RefCell<Slot>,
    reconstruction_state: ReconstructorData<[u8; MAX_SEGMENTS / 8], [u8; 256]>,
    max_l: usize,
    matrix_offset: usize,
    complete: bool,
}

impl Updater {
    #[must_use]
    pub fn is_complete(&self) -> bool {
        self.complete
    }

    pub async fn handle_segment<T: SpiFlash>(
        &mut self,
        flash: &mut T,
        _scratch: &mut ScratchRam,
        segment_1idx: u32,
        bytes: &[u8],
    ) -> Result<SegmentOutcome, ManagerError<T::Error>> {
        let flash = RefCell::new(flash);
        let parity = UpdaterMatrix {
            num_blocks: self.reconstruction_state.n,
        };
        let matrix = UpdaterMatrixStorage {
            parity_slot: &self.parity_slot,
            offset: self.matrix_offset,
            max_l: self.max_l,
            flash: &flash,
        };
        let parityblocks = UpdaterParityStorage {
            parity_slot: &self.parity_slot,
            max_l: self.max_l,
            flash: &flash,
        };
        let datablocks = UpdaterDataStorage {
            firmware_slot: &mut self.firmware_slot,
            flash: &flash,
        };

        let mut reconstructor: Reconstructor<_, _, _, _, MAX_SEGMENT_SIZE, _, _> = self
            .reconstruction_state
            .hydrate(parity, matrix, parityblocks, datablocks);

        match reconstructor
            .handle_block((segment_1idx - 1) as usize, bytes)
            .await
        {
            Ok(BlockResult::Done(_)) => {
                self.complete = true;
                Ok(SegmentOutcome::FirmwareComplete)
            }
            Ok(BlockResult::TooManyMissing) => {
                // TODO: Discuss whether this should throw some sort of error
                Ok(SegmentOutcome::Consumed)
            }
            Ok(BlockResult::NeedMore) => Ok(SegmentOutcome::Consumed),
            Err(parity_reconstruct::Error::DataStorageError(e))
            | Err(parity_reconstruct::Error::MatrixStorageError(e))
            | Err(parity_reconstruct::Error::ParityStorageError(e)) => Err(e.into()),
        }
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
    #[allow(clippy::await_holding_refcell_ref)]
    pub async fn check_and_mark_done<T: SpiFlash>(
        mut self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<usize, ManagerError<T::Error>> {
        if !self.complete {
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
        self.parity_slot
            .borrow_mut()
            .mark_ext_status_complete(flash)
            .await?;

        Ok(self.firmware_slot.index())
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

        // binary search for the maximum number of missing segments
        // we can handle given our size constraints
        let parity_size = self.slot_size.saturating_sub(DATA_REGION_OFFSET);
        let mut high = 2048;
        let mut low = 0;
        while (high - low) > 1 {
            let mid = (high + low) / 2;
            let mid_size = matrix_row_offset(mid) + mid * (segment_size as usize);
            if mid_size > parity_size {
                high = mid;
            } else {
                low = mid;
            }
        }
        let max_l = low;

        // Allocate slots and format them
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
                NumberOfSegments(max_l as u32),
                SegmentSize(segment_size),
                flash,
            )
            .await?;

        Ok(Updater {
            firmware_slot,
            parity_slot: RefCell::new(parity_slot),
            reconstruction_state: ReconstructorData::new(
                firmware_segments as usize,
                segment_size as usize,
            ),
            max_l,
            matrix_offset: max_l * (segment_size as usize),
            complete: false,
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

        // Load basic parameters
        let n = second_newest.1.num_segments.0 as usize;
        let blocksize = second_newest.1.segment_size.0 as usize;
        let max_l = newest.1.num_segments.0 as usize;
        let matrix_offset = max_l * blocksize;

        // Load done array
        let mut done = BitCache::new();
        let _ = firmware_slot
            .load_status_array(&mut done, &mut [0u8; MAX_SEGMENT_SIZE], flash)
            .await?;
        let done = done.buf;

        // Reconstruct used from the matrix
        let mut used = BitArray::ZERO;
        for i in 0..max_l {
            let offset = matrix_row_offset(i) + i / 8;
            let mut buf = [0u8; 1];
            parity_slot
                .read_raw(matrix_offset + offset, &mut buf, flash)
                .await?;
            // Any zero bits indicates a write, which by the
            // guarantees on the matrix means the diagonal is zero.
            used.set(i, buf[0] != 0xFF);
        }

        let l = if used.any() { n - done.count_ones() } else { 0 };

        let complete = done.count_ones() == n;

        Ok(Some(Updater {
            firmware_slot,
            parity_slot: RefCell::new(parity_slot),
            reconstruction_state: ReconstructorData {
                n,
                l,
                blocksize,
                done,
                used,
            },
            max_l,
            matrix_offset,
            complete,
        }))
    }
}

#[cfg(test)]
mod tests {
    use core::cell::RefCell;

    use bitvec::array::BitArray;
    use parity_reconstruct::MatrixStorage;

    use crate::{
        manager::{ScratchRam, SlotManager},
        testutils::heap_flash::Flash,
        update::matrix::UpdaterMatrixStorage,
    };

    #[tokio::test]
    async fn test_matrix_storage_all_ones() {
        const SLOT_SIZE: usize = 256 * 1024;
        let mut scratch = ScratchRam::new();
        let mut flash = Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let init = mgr.try_recover(&mut flash, &mut scratch).await.unwrap();
        assert!(init.is_none());

        let start = mgr
            .start_update(&mut flash, &mut scratch, 40, 658)
            .await
            .unwrap();

        let flash_refcell = RefCell::new(&mut flash);
        let mut matrix = UpdaterMatrixStorage {
            parity_slot: &start.parity_slot,
            offset: start.matrix_offset,
            max_l: start.max_l,
            flash: &flash_refcell,
        };

        for l in 0..start.max_l {
            let mut testdata: BitArray<[u8; 256]> = BitArray::ZERO;
            for i in 0..=l {
                testdata.set(i, true);
            }

            matrix.set_row(l, testdata.clone()).await.unwrap();

            let stored: BitArray<[u8; 256]> = matrix.row(l).await.unwrap();
            assert_eq!(stored, testdata);
        }
    }

    #[tokio::test]
    async fn test_matrix_storage_all_zeros() {
        const SLOT_SIZE: usize = 256 * 1024;
        let mut scratch = ScratchRam::new();
        let mut flash = Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let init = mgr.try_recover(&mut flash, &mut scratch).await.unwrap();
        assert!(init.is_none());

        let start = mgr
            .start_update(&mut flash, &mut scratch, 40, 658)
            .await
            .unwrap();

        let flash_refcell = RefCell::new(&mut flash);
        let mut matrix = UpdaterMatrixStorage {
            parity_slot: &start.parity_slot,
            offset: start.matrix_offset,
            max_l: start.max_l,
            flash: &flash_refcell,
        };

        for l in 0..start.max_l {
            let mut testdata: BitArray<[u8; 256]> = BitArray::ZERO;
            testdata.set(l, true); //required to uphold contract

            matrix.set_row(l, testdata.clone()).await.unwrap();

            let stored: BitArray<[u8; 256]> = matrix.row(l).await.unwrap();
            assert_eq!(stored, testdata);
        }
    }
}
