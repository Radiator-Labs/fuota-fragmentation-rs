//! We need a module that does "filesystem" like interactions on
//! our ring of slots.
//!
//! The aim for this file is to reduce complexity elsewhere.

use crate::{
    manager::{next_seq, ScratchRam},
    protocol::{FlashRepr, SlotHeader},
    spi_flash::{SpiFlash, SpiFlashError},
};

#[allow(clippy::exhaustive_structs)]
#[derive(Debug)]
pub struct IndexedHeader {
    pub idx: usize,
    pub hdr: Option<SlotHeader>,
}

#[allow(clippy::exhaustive_structs)]
#[derive(Debug)]
pub struct TwoHdrs<'a> {
    pub older: &'a IndexedHeader,
    pub newer: &'a IndexedHeader,
}

#[must_use]
pub fn get_two_newest(sli: &[IndexedHeader]) -> Option<TwoHdrs<'_>> {
    let mut iter = sli.iter().rev();
    let newer = loop {
        match iter.next() {
            Some(ih) if ih.hdr.is_some() => break ih,
            Some(_) => continue,
            None => return None,
        };
    };

    let older = iter.next()?;

    older.hdr.is_some().then_some(TwoHdrs { older, newer })
}

#[must_use]
pub fn get_next_seq_no(sli: &[IndexedHeader]) -> u32 {
    sli.iter()
        .rev()
        .find_map(|ih| ih.hdr.as_ref().map(|h| next_seq(h.seq_no.0)))
        .unwrap_or(0)
}

///
/// # Errors
/// Reports if SPI Flash activities fail
///
/// # Panics
/// Panics if hdrs is empty. TODO: eliminate this panic
pub async fn get_ordered_headers<T: SpiFlash, const N: usize>(
    flash: &mut T,
    slot_size: usize,
    scratch: &mut ScratchRam,
) -> Result<[IndexedHeader; N], SpiFlashError<T::Error>> {
    const ONE: IndexedHeader = IndexedHeader { idx: 0, hdr: None };

    // TODO: don't hold the whole set of headers
    let mut headers: [IndexedHeader; N] = [ONE; N];
    let mut all_none = true;

    for (i, slot) in headers.iter_mut().enumerate() {
        let address = i * slot_size;
        flash.read_to(address, &mut scratch.header_scratch).await?;
        slot.idx = i;
        slot.hdr = SlotHeader::take_from_bytes(&scratch.header_scratch).map(|x| x.0);
        all_none &= slot.hdr.is_none();
    }

    // Here, we're making something like `.windows()`, but it treats the wraparound first.
    //
    // EX: Given 4 slots with sequence numbers: [3, 4, 5, 6]
    //
    // We want to iterate like this:
    //
    // [
    //     (0, (6, 3)),
    //     (1, (3, 4)),
    //     (2, (4, 5)),
    //     (3, (5, 6)),
    //]
    //
    // We do this so that any place of discontinuity is discovered when the index is
    // in the RIGHT HAND position (so here, idx=0 is when the first item in the ring is
    // in the RIGHT HAND position).

    // First we make an iterator that STARTS with the LAST item and wraps around
    let (first, last) = headers.split_at(N - 1);
    let left_iter = last.iter().chain(first);

    // Then, we make an iterator that STARTS with the FIRST item.
    let right_iter = headers.iter();

    // Then, combine them, and enumerate the index.
    let mut ring = left_iter.zip(right_iter).enumerate();

    let opt_oldest_index = ring.find_map(|(i, (a, b))| {
        // A discontinuity is a case where either:
        //  - the RIGHT value is not `LEFT.wrapping_add(1)` value
        //  - the RIGHT value is NONE
        match (
            a.hdr.as_ref().map(|sh| sh.seq_no.0),
            b.hdr.as_ref().map(|sh| sh.seq_no.0),
        ) {
            (Some(_), None) => Some(i),
            (Some(s1), Some(s2)) if s2 != next_seq(s1) => Some(i),
            _ => None,
        }
    });

    if let Some(oldest_index) = opt_oldest_index {
        // NOTE: the standard library does provide a rotate_left method on slices, but it currently
        // uses much more binary space than this naive implementation.
        rotate_left(&mut headers, oldest_index);
    } else {
        assert!(all_none);
    }

    Ok(headers)
}

#[inline]
pub(crate) fn rotate_left<T>(slice: &mut [T], n: usize) {
    let len = slice.len();
    if len <= 1 {
        // Look, it's rotated!
        return;
    }
    // This is a little wasteful
    for _ in 0..n {
        // SAFETY: Performs swap of pointer accessed memory
        unsafe {
            let ptr = slice.as_mut_ptr();
            let first_val = ptr.read();
            core::ptr::copy(ptr.add(1), ptr, len - 1);
            ptr.add(len - 1).write(first_val);
        }
    }
}

// pub async fn find_oldest_slot<T: SpiFlash>(
//     &mut self,
//     flash: &mut T,
//     scratch: &mut ScratchRam,
// ) -> Result<OldestReport, SpiFlashError<T::Error>> {
//     let mut seq_nos: [Option<u32>; N] = [None; N];

//     for (i, slot) in seq_nos.iter_mut().enumerate() {
//         let address = i * self.slot_size;
//         flash.read_to(address, &mut scratch.header_scratch).await?;
//         *slot = SlotHeader::take_from_bytes(&scratch.header_scratch).map(|s| s.0.seq_no.0);
//     }

//     // Here, we're making something like `.windows()`, but it treats the wraparound first.
//     //
//     // EX: Given 4 slots with sequence numbers: [3, 4, 5, 6]
//     //
//     // We want to iterate like this:
//     //
//     // [
//     //     (0, (6, 3)),
//     //     (1, (3, 4)),
//     //     (2, (4, 5)),
//     //     (3, (5, 6)),
//     //]
//     //
//     // We do this so that any place of discontinuity is discovered when the index is
//     // in the RIGHT HAND position (so here, idx=0 is when the first item in the ring is
//     // in the RIGHT HAND position).

//     // First we make an iterator that STARTS with the LAST item and wraps around
//     let liter = seq_nos.iter().cycle().skip(N - 1);
//     // Then, we make an iterator that STARTS with the FIRST item.
//     let right_iter = seq_nos.iter();
//     // Then, combine them, taking one round's worth, and enumerate the index.
//     let mut ring = liter.zip(right_iter).enumerate().take(N);

//     let val = ring.find_map(|(i, (a, b))| {
//         // A discontinuity is a case where the RIGHT value is not `LEFT.wrapping_add(1)`
//         // value, OR the RIGHT value is NONE
//         match (a, b) {
//             (Some(seq), None) => Some((i, next_seq(*seq))),
//             (Some(s1), Some(s2)) if *s2 != next_seq(*s1) => Some((i, next_seq(*s1))),
//             _ => None,
//         }
//     });

//     // If we found NO discontinuities (including the wrap around!), this means
//     // we probably have a totally empty flash, so start with an initial value.
//     if let Some((idx, seq)) = val {
//         Ok(OldestReport {
//             slot_idx: idx,
//             next_seq_no: seq,
//         })
//     } else {
//         Ok(OldestReport {
//             slot_idx: 0,
//             next_seq_no: 0,
//         })
//     }
// }
