//! Bitcache
//!
//! A bit-packed array of bits, useful for capturing LDPC matrix rows

use bitvec::array::BitArray;

use crate::protocol::segment_status_table::{DATA_NOT_WRITTEN, DATA_WRITTEN, MAX_SEGMENTS};

const BC_SIZE: usize = MAX_SEGMENTS / 8;

#[derive(Debug, PartialEq)]
pub struct OutOfRange {
    _clippy: (),
}

/// Bitcache
///
/// A bit-packed array of bits, useful for capturing LDPC matrix rows
pub struct BitCache {
    buf: BitArray<[u8; BC_SIZE]>,
}

#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub enum FillError {
    OutOfRange,
    InvalidByte,
}

impl From<OutOfRange> for FillError {
    fn from(_value: OutOfRange) -> Self {
        FillError::OutOfRange
    }
}

impl Default for BitCache {
    fn default() -> Self {
        Self::new()
    }
}

impl BitCache {
    /// Create a new [`BitCache`], with all bits set to "false"
    #[must_use]
    pub const fn new() -> Self {
        Self {
            buf: BitArray::ZERO,
        }
    }

    /// Fill the bitcache from an on-flash memory region.
    ///
    /// NOTE: uses the definitions of [`DATA_WRITTEN`] and [`DATA_NOT_WRITTEN`],
    /// NOT the typical definition of 1 == true and 0 == false.
    ///
    /// # Errors
    ///
    /// Returns an error if the given start + data exceeds the capacity of the
    /// cache, or if any of the bytes contain an invalid value
    pub fn fill_from(&mut self, start: usize, data: &[u8]) -> Result<(), FillError> {
        if start + data.len() > MAX_SEGMENTS {
            return Err(FillError::OutOfRange);
        }

        let mut idx = start;
        for byte in data {
            match *byte {
                DATA_WRITTEN => self.set(idx, true)?,
                DATA_NOT_WRITTEN => self.set(idx, false)?,
                _ => return Err(FillError::InvalidByte),
            }
            idx += 1;
        }
        Ok(())
    }

    /// Get the idx-th bit of the cache
    ///
    /// Returns None if the idx is larger than capacity
    #[must_use]
    pub fn get(&self, idx: usize) -> Option<bool> {
        self.buf.get(idx).map(|v| *v)
    }

    /// Set the idx-th bit of the cache
    ///
    /// # Errors
    ///
    /// Returns an error if idx is larger than capacity
    pub fn set(&mut self, idx: usize, val: bool) -> Result<(), OutOfRange> {
        *self.buf.get_mut(idx).ok_or(OutOfRange { _clippy: () })? = val;
        Ok(())
    }

    /// Obtain an iterator over the entire [`BitCache`].
    ///
    /// Can be used with [`Iterator::take`] to obtain a subset of the
    /// bitcache
    #[must_use]
    pub fn iter(&self) -> BitIter<'_> {
        BitIter { bc: self, idx: 0 }
    }
}

#[derive(Clone)]
pub struct BitIter<'a> {
    bc: &'a BitCache,
    idx: usize,
}

// Only here because clippy said so
impl<'a> IntoIterator for &'a BitCache {
    type Item = bool;

    type IntoIter = BitIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

// NOTE: We *really* don't want to implement all trait methods for iterator.
#[allow(clippy::missing_trait_methods)]
impl<'a> Iterator for BitIter<'a> {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.bc.get(self.idx)?;
        self.idx += 1;
        Some(val)
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.idx += n;
        let val = self.bc.get(self.idx)?;
        self.idx += 1;
        Some(val)
    }
}

#[cfg(test)]
mod test {
    // It is idiomatic to use unwraps in tests.
    #![allow(clippy::unwrap_used)]

    use super::*;

    #[test]
    fn smoke() {
        let mut bit = BitCache::new();
        for idx in (0..MAX_SEGMENTS).step_by(7) {
            bit.set(idx, true).unwrap();
        }

        bit.iter().enumerate().for_each(|(i, b)| {
            assert_eq!(b, i % 7 == 0);
        });
    }

    #[test]
    fn fill() {
        let mut buf = [DATA_NOT_WRITTEN; MAX_SEGMENTS];
        buf.iter_mut().step_by(7).for_each(|b| *b = DATA_WRITTEN);

        let mut bit = BitCache::new();
        bit.fill_from(0, &buf).unwrap();

        bit.iter().enumerate().for_each(|(i, b)| {
            assert_eq!(b, i % 7 == 0);
        });
    }
}
