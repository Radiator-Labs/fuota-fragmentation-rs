//! Utilities for using [`parity-reconstruct`](crate) with the LoRaWAN Fragmented Data Block standard.
use bitvec::{array::BitArray, view::BitViewSized};

use crate::ParityMatrix;

/// Paritymatrix specified in the LoRaWAN Fragmented Data Block Transport standard
pub struct LfdbtParity {
    n: usize,
}

impl LfdbtParity {
    /// Create a LoRaWAN Fragmented Data Block Transport parity matrix
    pub fn new(n: usize) -> Self {
        LfdbtParity { n }
    }
}

fn prbs23(x: usize) -> usize {
    let b0 = x & 1;
    let b1 = (x & 32) >> 5;
    (x >> 1) + ((b0 ^ b1) << 22)
}

impl<U: BitViewSized> ParityMatrix<U> for LfdbtParity {
    fn row(&self, m: usize) -> bitvec::prelude::BitArray<U> {
        debug_assert!(BitArray::<U>::ZERO.len() >= self.n);
        if m < self.n {
            let mut out = BitArray::<U>::ZERO;
            out.set(m, true);
            out
        } else {
            let row_index = m - self.n;
            let jiggle = if self.n.count_ones() == 1 { 1 } else { 0 };

            let mut x = 1 + 1001 * row_index;
            let mut out = BitArray::<U>::ZERO;
            for _ in 0..(self.n / 2) {
                let r = loop {
                    x = prbs23(x);
                    let cand = x % (self.n + jiggle);
                    if cand < self.n {
                        break cand;
                    }
                };
                out.set(r, true);
            }

            out
        }
    }
}

#[cfg(test)]
mod tests {
    use bitvec::order::Lsb0;

    use super::*;

    #[test]
    fn test_diag() {
        let matrix = LfdbtParity::new(26);
        for i in 0..26 {
            let row: BitArray<[u32; 1]> = matrix.row(i);
            for j in 0..32 {
                if i == j {
                    assert!(row[j]);
                } else {
                    assert!(!row[j]);
                }
            }
        }
    }

    #[test]
    fn test_parity() {
        let matrix = LfdbtParity::new(26);
        let refdata: [u32; 26] = [
            0x1555554, 0x1A331CB, 0x3F2C620, 0x00B94FA, 0x1A022C3, 0x04FA190, 0x24A6A64, 0x0A69653,
            0x010AC9F, 0x0E6A191, 0x0A24D8C, 0x3D04916, 0x2808E37, 0x02158E8, 0x2846A9A, 0x00C885C,
            0x206DC78, 0x1469920, 0x0372542, 0x0152397, 0x3F16201, 0x144288C, 0x1B71504, 0x2B31230,
            0x2017E2E, 0x02520C1,
        ];
        for i in 26..52 {
            let row: BitArray<[u32; 1]> = matrix.row(i);
            assert_eq!(
                row.into_inner(),
                [refdata[i - 26]],
                "mistake row {} ({:x})",
                i,
                row.into_inner()[0]
            );
        }
    }
}
