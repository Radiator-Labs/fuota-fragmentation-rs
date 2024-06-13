//! We need a module that does "filesystem" like interactions on
//! our ring of slots.
//!
//! The aim for this file is to reduce complexity elsewhere.

use crate::{
    manager::layout::SequenceNumber,
    spi_flash::{SpiFlash, SpiFlashError},
};

use super::{
    layout::{total_status, FlashRepr, Kind, SlotHeader, TotalStatus},
    ManagerError, ScratchRam, Slot, SlotManager,
};

impl<const N: usize> SlotManager<N> {
    // Load all slot headers from the flash
    pub(crate) async fn load_headers<T: SpiFlash>(
        &self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<[Option<SlotHeader>; N], SpiFlashError<T::Error>> {
        let mut headers = core::array::from_fn(|_| None);

        for (i, slot) in headers.iter_mut().enumerate() {
            let address = i * self.slot_size;
            flash.read_to(address, &mut scratch.header_scratch).await?;
            *slot = SlotHeader::take_from_bytes(&scratch.header_scratch).map(|x| x.0);
        }

        Ok(headers)
    }

    // Allocate two new slots. Due to requirements in existing bootloaders, the two slots for an update
    // need to be allocated simultaneously, to allow fallback firmware to be kept in tact whilst
    // keeping slot numbering sequential.
    async fn alloc_slotpair<T: SpiFlash>(
        &self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<(Slot, Slot), ManagerError<T::Error>> {
        let headers = self.load_headers(flash, scratch).await?;

        let mut low = None;
        let mut high = None;
        for (idx, header) in indexed_headers(&headers) {
            if low
                .and_then(|i| (&headers[i] as &Option<SlotHeader>).as_ref())
                .map(|h| h.seq_no > header.seq_no)
                .unwrap_or(true)
            {
                low = Some(idx);
            }
            if high
                .and_then(|i| (&headers[i] as &Option<SlotHeader>).as_ref())
                .map(|h| h.seq_no < header.seq_no)
                .unwrap_or(true)
            {
                high = Some(idx);
            }
        }

        // select the slots for the new update
        let (first, second, first_seq_no, second_seq_no) =
            if let (Some(low), Some(high)) = (low, high) {
                // unwrap is ok here since this is guaranteed to contain something by earlier code.
                let high_seq_no = headers[high].as_ref().unwrap().seq_no;
                if (high + N - low) % N <= N - 2 {
                    // Just use the empty slots, no need to be fancy
                    let first = (high + 1) % N;
                    let second = (high + 2) % N;
                    let first_seq_no = high_seq_no.next();
                    let second_seq_no = first_seq_no.next();
                    (first, second, first_seq_no, second_seq_no)
                } else if (high + N - low) % N == N - 1 {
                    // One empty slot, we will always use that
                    let firmware = fallback_firmware_slot(&headers).unwrap_or(low);

                    // If firmware is in the oldest slot, cant use that so overlap with newest slot
                    if firmware == low {
                        let first = high;
                        let second = (high + 1) % N;
                        let second_seq_no = high_seq_no.next();
                        (first, second, high_seq_no, second_seq_no)
                    } else {
                        let first = (high + 1) % N;
                        let second = (high + 2) % N;
                        let first_seq_no = high_seq_no.next();
                        let second_seq_no = first_seq_no.next();
                        (first, second, first_seq_no, second_seq_no)
                    }
                } else {
                    let firmware = fallback_firmware_slot(&headers).unwrap_or(low);

                    // If firmware is in one of the two candidate slots, instead reuse last
                    if (high + 1) % N == firmware || (high + 2) % N == firmware {
                        let first = (high + N - 1) % N;
                        let second = high;
                        // unwrap will not fail since it is a used slot
                        let first_seq_no = headers[first].as_ref().unwrap().seq_no;
                        (first, second, first_seq_no, high_seq_no)
                    } else {
                        let first = (high + 1) % N;
                        let second = (high + 2) % N;
                        let first_seq_no = high_seq_no.next();
                        let second_seq_no = first_seq_no.next();
                        (first, second, first_seq_no, second_seq_no)
                    }
                }
            } else {
                // No slots yet in use, use the first ones
                (0, 1, SequenceNumber(0), SequenceNumber(0))
            };

        let first = Slot {
            idx: first,
            size: self.slot_size,
        };
        let second = Slot {
            idx: second,
            size: self.slot_size,
        };

        first.clear(flash).await?;
        second.clear(flash).await?;
        first.write_seq_no(first_seq_no, flash).await?;
        second.write_seq_no(second_seq_no, flash).await?;

        Ok((first, second))
    }

    // Access a slot for reading/writing
    //
    // Panics if idx is out of range
    pub fn open(&self, idx: usize) -> Slot {
        assert!(idx < N);

        Slot {
            idx,
            size: self.slot_size,
        }
    }

    pub async fn fallback_firmware<T: SpiFlash>(
        &self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<Option<Slot>, ManagerError<T::Error>> {
        let headers = self.load_headers(flash, scratch).await?;
        let firmware = fallback_firmware_slot(&headers);
        Ok(firmware.map(|idx| Slot {
            idx,
            size: self.slot_size,
        }))
    }
}

impl Slot {
    pub(crate) async fn load_header<T: SpiFlash>(&self, flash: &mut T, scratch: &mut ScratchRam) -> Result<Option<SlotHeader>, SpiFlashError<T::Error>> {
        let offset = self.idx * self.size;
        flash.read_to(offset, &mut scratch.header_scratch).await?;
        Ok(SlotHeader::take_from_bytes(&scratch.header_scratch).map(|v| v.0))
    }

    async fn clear<T: SpiFlash>(&self, flash: &mut T) -> Result<(), SpiFlashError<T::Error>> {
        let firm_start = self.idx * self.size;
        let block_size = flash.block_size();
        let end = firm_start + self.size;
        assert_eq!(self.size % block_size, 0);
        let mut cur: usize = firm_start;
        while cur < end {
            flash.erase_block(cur).await?;
            cur += block_size;
        }
        Ok(())
    }

    async fn write_seq_no<T: SpiFlash>(
        &self,
        seq_no: SequenceNumber,
        flash: &mut T,
    ) -> Result<(), ManagerError<T::Error>> {
        let offset = self.idx * self.size + SlotHeader::SEQUENCE_NUMBER_OFFSET;
        let mut buf = [0; SequenceNumber::SIZE];
        seq_no.write_to_bytes(&mut buf)?;
        flash.write_from(offset, &buf).await?;
        Ok(())
    }

    pub(crate) async fn write_kind<T: SpiFlash>(
        &self,
        kind: Kind,
        flash: &mut T,
    ) -> Result<(), ManagerError<T::Error>> {
        let offset = self.idx * self.size + SlotHeader::KIND_OFFSET;
        let mut buf = [0; Kind::SIZE];
        kind.write_to_bytes(&mut buf)?;
        flash.write_from(offset, &buf).await?;
        Ok(())
    }

    pub(crate) async fn segment_status<T: SpiFlash>(&self, idx: usize, )
}

pub(crate) fn indexed_headers<const N: usize>(
    headers: &[Option<SlotHeader>; N],
) -> impl Iterator<Item = (usize, &SlotHeader)> {
    headers
        .iter()
        .enumerate()
        .flat_map(|(i, v)| v.as_ref().map(|u| (i, u)))
}

fn fallback_firmware_slot<const N: usize>(headers: &[Option<SlotHeader>; N]) -> Option<usize> {
    let mut result = None;
    let mut result_seqno = None;
    for (idx, header) in indexed_headers(headers) {
        match (total_status(header), result_seqno) {
            (TotalStatus::ConfirmedImage, None) => {
                result = Some(idx);
                result_seqno = Some(header.seq_no);
            }
            (TotalStatus::ConfirmedImage, Some(old_seq_no)) if old_seq_no < header.seq_no => {
                result = Some(idx);
                result_seqno = Some(header.seq_no);
            }
            _ => {}
        }
    }

    result
}
