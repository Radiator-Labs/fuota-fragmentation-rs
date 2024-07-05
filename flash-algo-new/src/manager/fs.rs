//! We need a module that does "filesystem" like interactions on
//! our ring of slots.
//!
//! The aim for this file is to reduce complexity elsewhere.

use core::{iter::Take, num::NonZeroU32};

use crate::{
    bitcache::{BitCache, BitIter, FillError},
    manager::layout::{NumberOfSegments, SequenceNumber},
    spi_flash::{SpiFlash, SpiFlashError},
};

#[cfg(feature = "matrixreconstructor")]
use crate::layout::HEADER_SIZE;

use super::{
    layout::{
        segment_status_table::{DATA_WRITTEN, MAX_SEGMENTS},
        total_status, FlashRepr, Kind, SegmentSize, SlotHeader, TotalStatus, DATA_REGION_OFFSET,
        WRITTEN_OFFSET,
    },
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
    pub(crate) async fn alloc_slotpair<T: SpiFlash>(
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
                (0, 1, SequenceNumber(0), SequenceNumber(1))
            };

        let mut first = Slot {
            idx: first,
            size: self.slot_size,
            segment_size: None,
        };
        let mut second = Slot {
            idx: second,
            size: self.slot_size,
            segment_size: None,
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
            segment_size: None,
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
            segment_size: None,
        }))
    }
}

impl Slot {
    pub(crate) async fn load_header<T: SpiFlash>(
        &self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<Option<SlotHeader>, SpiFlashError<T::Error>> {
        let offset = self.idx * self.size;
        flash.read_to(offset, &mut scratch.header_scratch).await?;
        Ok(SlotHeader::take_from_bytes(&scratch.header_scratch).map(|v| v.0))
    }

    pub(crate) async fn clear<T: SpiFlash>(
        &mut self,
        flash: &mut T,
    ) -> Result<(), SpiFlashError<T::Error>> {
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
        &mut self,
        seq_no: SequenceNumber,
        flash: &mut T,
    ) -> Result<(), ManagerError<T::Error>> {
        let offset = self.idx * self.size + SlotHeader::SEQUENCE_NUMBER_OFFSET;
        let mut buf = [0; SequenceNumber::SIZE];
        seq_no.write_to_bytes(&mut buf)?;
        flash.write_from(offset, &buf).await?;
        Ok(())
    }

    #[cfg(not(feature = "matrixreconstructor"))]
    pub(crate) async fn segment_status<T: SpiFlash>(
        &self,
        idx: usize,
        flash: &mut T,
    ) -> Result<u8, SpiFlashError<T::Error>> {
        if idx > MAX_SEGMENTS {
            return Err(SpiFlashError::OutOfBounds);
        }
        let offset = WRITTEN_OFFSET.saturating_add(idx);
        if offset > self.size {
            return Err(SpiFlashError::OutOfBounds);
        }
        let mut buf = [0xAB_u8; 1];
        flash
            .read_to(self.idx * self.size + offset, &mut buf)
            .await?;
        Ok(buf[0])
    }

    async fn segment_size_mut<T: SpiFlash>(
        &mut self,
        flash: &mut T,
    ) -> Result<usize, SpiFlashError<<T as SpiFlash>::Error>> {
        let segment_size = match self.segment_size {
            Some(size) => size.get() as usize,
            None => {
                let mut buf = [0; 4];
                flash
                    .read_to(
                        self.size * self.idx + SlotHeader::SEGMENT_SIZE_OFFSET,
                        &mut buf,
                    )
                    .await?;
                let segment_size = SegmentSize::take_from_bytes(&buf)
                    .map(|(SegmentSize(v), _)| v)
                    .unwrap_or(0);
                self.segment_size = NonZeroU32::new(segment_size);
                segment_size as usize
            }
        };
        Ok(segment_size)
    }

    pub(crate) async fn segment_size<T: SpiFlash>(
        &self,
        flash: &mut T,
    ) -> Result<usize, SpiFlashError<<T as SpiFlash>::Error>> {
        let segment_size = match self.segment_size {
            Some(size) => size.get() as usize,
            None => {
                let mut buf = [0; 4];
                flash
                    .read_to(
                        self.size * self.idx + SlotHeader::SEGMENT_SIZE_OFFSET,
                        &mut buf,
                    )
                    .await?;
                let segment_size = SegmentSize::take_from_bytes(&buf)
                    .map(|(SegmentSize(v), _)| v)
                    .unwrap_or(0);
                segment_size as usize
            }
        };
        Ok(segment_size)
    }

    pub(crate) async fn num_segments<T: SpiFlash>(
        &self,
        flash: &mut T,
    ) -> Result<NumberOfSegments, SpiFlashError<<T as SpiFlash>::Error>> {
        let mut buf = [0; 4];
        flash
            .read_to(
                self.size * self.idx + SlotHeader::NUMBER_OF_SEGMENTS_OFFSET,
                &mut buf,
            )
            .await?;
        Ok(NumberOfSegments::take_from_bytes(&buf)
            .map(|(v, _)| v)
            .unwrap_or(NumberOfSegments(0)))
    }

    pub(crate) async fn set_kind<T: SpiFlash>(
        &mut self,
        kind: Kind,
        flash: &mut T,
    ) -> Result<(), ManagerError<T::Error>> {
        let mut buf = [0; 4];
        kind.write_to_bytes(&mut buf)?;
        flash
            .write_from(self.size * self.idx + SlotHeader::KIND_OFFSET, &buf)
            .await?;
        Ok(())
    }

    pub(crate) async fn set_layout<T: SpiFlash>(
        &mut self,
        no_segments: NumberOfSegments,
        segment_size: SegmentSize,
        flash: &mut T,
    ) -> Result<(), ManagerError<<T as SpiFlash>::Error>> {
        let data_size = no_segments.0.saturating_mul(segment_size.0) as usize;
        if data_size > self.size.saturating_sub(DATA_REGION_OFFSET) {
            return Err(SpiFlashError::OutOfBounds.into());
        }
        let mut buf = [0; 4];
        no_segments.write_to_bytes(&mut buf)?;
        flash
            .write_from(
                self.size * self.idx + SlotHeader::NUMBER_OF_SEGMENTS_OFFSET,
                &buf,
            )
            .await?;
        segment_size.write_to_bytes(&mut buf)?;
        flash
            .write_from(self.idx * self.size + SlotHeader::SEGMENT_SIZE_OFFSET, &buf)
            .await?;
        self.segment_size = NonZeroU32::new(segment_size.0);

        Ok(())
    }

    async fn mark_segment_written<T: SpiFlash>(
        &self,
        idx: usize,
        flash: &mut T,
    ) -> Result<(), SpiFlashError<T::Error>> {
        if idx > MAX_SEGMENTS {
            return Err(SpiFlashError::OutOfBounds);
        }
        let offset = WRITTEN_OFFSET.saturating_add(idx);
        if offset > self.size {
            return Err(SpiFlashError::OutOfBounds);
        }
        flash
            .write_from(self.idx * self.size + offset, &[DATA_WRITTEN; 1])
            .await?;
        Ok(())
    }

    pub(crate) async fn write_segment<T: SpiFlash>(
        &mut self,
        idx: usize,
        buf: &[u8],
        flash: &mut T,
    ) -> Result<(), SpiFlashError<T::Error>> {
        if idx > MAX_SEGMENTS {
            return Err(SpiFlashError::OutOfBounds);
        }
        let segment_size = self.segment_size_mut(flash).await?;
        if segment_size == 0 {
            return Err(SpiFlashError::LogicError);
        }
        if segment_size != buf.len() {
            return Err(SpiFlashError::LogicError);
        }
        let offset: usize = DATA_REGION_OFFSET.saturating_add(idx.saturating_mul(segment_size));
        if offset > self.size {
            return Err(SpiFlashError::OutOfBounds);
        }
        flash.write_from(self.idx * self.size + offset, buf).await?;
        self.mark_segment_written(idx, flash).await?;
        Ok(())
    }

    pub(crate) async fn read_segment<T: SpiFlash>(
        &mut self,
        idx: usize,
        buf: &mut [u8],
        flash: &mut T,
    ) -> Result<usize, SpiFlashError<T::Error>> {
        if idx > MAX_SEGMENTS {
            return Err(SpiFlashError::OutOfBounds);
        }
        let segment_size = self.segment_size(flash).await?;
        if segment_size == 0 {
            return Err(SpiFlashError::LogicError);
        }
        let offset = DATA_REGION_OFFSET.saturating_add(idx.saturating_mul(segment_size));
        if offset > self.size {
            return Err(SpiFlashError::OutOfBounds);
        }
        let write_size = buf.len().min(segment_size);
        flash
            .read_to(self.idx * self.size + offset, &mut buf[..write_size])
            .await?;
        Ok(write_size)
    }

    #[cfg(feature = "matrixreconstructor")]
    pub(crate) async fn write_raw<T: SpiFlash>(
        &mut self,
        offset: usize,
        buf: &[u8],
        flash: &mut T,
    ) -> Result<usize, SpiFlashError<T::Error>> {
        if self.size.saturating_sub(HEADER_SIZE) < offset.saturating_add(buf.len()) {
            return Err(SpiFlashError::OutOfBounds);
        }

        flash
            .write_from(self.idx * self.size + HEADER_SIZE + offset, buf)
            .await?;
        Ok(buf.len())
    }

    #[cfg(feature = "matrixreconstructor")]
    pub(crate) async fn read_raw<T: SpiFlash>(
        &mut self,
        offset: usize,
        buf: &mut [u8],
        flash: &mut T,
    ) -> Result<usize, SpiFlashError<T::Error>> {
        if self.size.saturating_sub(HEADER_SIZE) < offset.saturating_add(buf.len()) {
            return Err(SpiFlashError::OutOfBounds);
        }

        flash
            .read_to(self.idx * self.size + HEADER_SIZE + offset, buf)
            .await?;
        Ok(buf.len())
    }

    pub(crate) async fn load_status_array<'a, T: SpiFlash>(
        &mut self,
        bitbuf: &'a mut BitCache,
        scratch_bytes: &mut [u8],
        flash: &mut T,
    ) -> Result<Take<BitIter<'a>>, SpiFlashError<T::Error>> {
        let no_segments = self.num_segments(flash).await?.0 as usize;
        fill_bitcache(
            flash,
            scratch_bytes,
            bitbuf,
            self.idx * self.size + WRITTEN_OFFSET,
            no_segments,
        )
        .await
    }
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

#[cfg(test)]
mod tests {
    extern crate std;
    use std::{prelude::rust_2021::*, print};

    use crate::testutils::{heap_flash::Flash, protocol_test_support::make_a_slot};

    use super::*;
    use insta::assert_snapshot;

    #[tokio::test]
    async fn empty_flash() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();
        let mut flash = Flash::new(64 * 1024, 1024 * 1024);
        let mgr = SlotManager::<4>::new(SLOT_SIZE);

        let (mut a, mut b) = mgr.alloc_slotpair(&mut flash, &mut scratch).await.unwrap();

        assert_eq!(a.idx, 0);
        assert_eq!(b.idx, 1);

        a.set_kind(Kind::Firmware, &mut flash).await.unwrap();
        a.set_layout(NumberOfSegments(1), SegmentSize(1), &mut flash)
            .await
            .unwrap();
        b.set_kind(Kind::Parity, &mut flash).await.unwrap();
        b.set_layout(NumberOfSegments(1), SegmentSize(1), &mut flash)
            .await
            .unwrap();

        let aheader = a
            .load_header(&mut flash, &mut scratch)
            .await
            .unwrap()
            .unwrap();
        let bheader = b
            .load_header(&mut flash, &mut scratch)
            .await
            .unwrap()
            .unwrap();

        assert_eq!(aheader.seq_no.0, 0);
        assert_eq!(bheader.seq_no.0, 1);
    }

    #[tokio::test]
    async fn partial_empty() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();
        let mut flash = Flash::new(64 * 1024, 1024 * 1024);
        let mgr = SlotManager::<4>::new(SLOT_SIZE);

        let mut scr = [0_u8; 1024];
        // 0, 1, 2, X
        for idx in 0..4 {
            flash.erase_all().await.unwrap();
            for j in 0..=idx {
                #[allow(clippy::cast_possible_truncation)]
                make_a_slot(j as u32, &mut scr);
                flash
                    .write_from(SLOT_SIZE * j, &scr[..SlotHeader::SIZE])
                    .await
                    .unwrap();
            }

            let (mut a, mut b) = mgr.alloc_slotpair(&mut flash, &mut scratch).await.unwrap();
            a.set_kind(Kind::Firmware, &mut flash).await.unwrap();
            a.set_layout(NumberOfSegments(1), SegmentSize(1), &mut flash)
                .await
                .unwrap();
            b.set_kind(Kind::Parity, &mut flash).await.unwrap();
            b.set_layout(NumberOfSegments(1), SegmentSize(1), &mut flash)
                .await
                .unwrap();

            let aheader = a
                .load_header(&mut flash, &mut scratch)
                .await
                .unwrap()
                .unwrap();
            let bheader = b
                .load_header(&mut flash, &mut scratch)
                .await
                .unwrap()
                .unwrap();

            assert_eq!(a.idx, (idx + 1) % 4);
            assert_eq!(aheader.seq_no.0, (idx + 1) as u32);
            assert_eq!(b.idx, (idx + 2) % 4);
            assert_eq!(bheader.seq_no.0, (idx + 2) as u32);
        }

        flash.erase_all().await.unwrap();
        for j in 0..4 {
            #[allow(clippy::cast_possible_truncation)]
            make_a_slot(j as u32, &mut scr);
            flash
                .write_from(SLOT_SIZE * j, &scr[..SlotHeader::SIZE])
                .await
                .unwrap();
        }

        assert_snapshot!(flash.dump_to_string());
    }

    #[tokio::test]
    async fn right_placement() {
        const SLOT_SIZE: usize = 256 * 1024;
        let mut scratch = ScratchRam::new();

        // CSpell:disable
        // (Current Status, Next Position, Next Sequence Number)
        let cases = [
            // Some (or all) slots are empty - the first empty slot is used
            (b"NNNN", 0, 0, 1, 1),
            (b"0NNN", 1, 1, 2, 2),
            (b"01NN", 2, 2, 3, 3),
            (b"012N", 3, 3, 0, 4),
            // Works the same whether you have/haven't erased the desired page
            (b"0123", 0, 4, 1, 5),
            (b"N123", 0, 4, 1, 5),
            (b"4123", 1, 5, 2, 6),
            (b"4N23", 1, 5, 2, 6),
            (b"4523", 2, 6, 3, 7),
            (b"45N3", 2, 6, 3, 7),
            (b"4563", 3, 7, 0, 8),
            (b"456N", 3, 7, 0, 8),
            (b"4567", 0, 8, 1, 9),
            (b"N567", 0, 8, 1, 9),
            // close to max seq_no
            (b"tuvw", 0, u32::MAX - 2, 1, u32::MAX - 1),
            // Also, we treat u32::MAX the same as "None"
            (b"zzzz", 0, 0, 1, 1),
            // What if there are two weird blank spots?
            // Show the "healing" progression
            (b"2N6N", 3, 7, 0, 8),
            (b"8N67", 1, 9, 2, 10),
            // Additional weirdo cases
            (b"5NNN", 1, 6, 2, 7),
            (b"N5NN", 2, 6, 3, 7),
            (b"NN5N", 3, 6, 0, 7),
            (b"NNN5", 0, 6, 1, 7),
            // Dupe index
            (b"233N", 2, 4, 3, 5),
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
                        // Treat lowercase letters as the last values before full,
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
        let mgr = SlotManager::<4>::new(SLOT_SIZE);

        let mut scr = [0_u8; 1024];

        for (state, a_idx, a_seq, b_idx, b_seq) in cases {
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

            let (mut a, mut b) = mgr.alloc_slotpair(&mut flash, &mut scratch).await.unwrap();
            a.set_kind(Kind::Firmware, &mut flash).await.unwrap();
            a.set_layout(NumberOfSegments(1), SegmentSize(1), &mut flash)
                .await
                .unwrap();
            b.set_kind(Kind::Parity, &mut flash).await.unwrap();
            b.set_layout(NumberOfSegments(1), SegmentSize(1), &mut flash)
                .await
                .unwrap();

            let aheader = a
                .load_header(&mut flash, &mut scratch)
                .await
                .unwrap()
                .unwrap();
            let bheader = b
                .load_header(&mut flash, &mut scratch)
                .await
                .unwrap()
                .unwrap();

            assert_eq!(a.idx, a_idx);
            assert_eq!(aheader.seq_no.0, a_seq);
            assert_eq!(b.idx, b_idx);
            assert_eq!(bheader.seq_no.0, b_seq);
        }
    }
}
