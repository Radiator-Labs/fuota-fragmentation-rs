//! Reconstruction algorithm for data from parity data.
//! 
//! Below, we use N to indicate the number of blocks in the data
//! L is used to indicate the maximum number of missing data block
//! that we are prepared to handle
//! 
//! We use U and V for BitArray backing storages of size sufficient
//! to handle at least N bits (U) or L bits (V). This can't be done
//! directly due to limitations in the rust type system.
#![no_std]

use core::marker::PhantomData;

use bitvec::{array::BitArray, view::BitViewSized};

/// Trait describing a parity matrix.
pub trait ParityMatrix<U: BitViewSized> {
    /// Get the mth row of the parity matrix.
    ///
    /// All rows should be of exactly length N.
    ///
    /// For m < N, this should be a row with exactly
    /// one true, in position m.
    /// 
    /// Any excess bits in the bitarray MUST be false
    fn row(&self, m: usize) -> BitArray<U>;
}

/// Storage trait for matrix data
pub trait MatrixStorage<V: BitViewSized> {
    /// Store a row of the parity reconsturction matrix.
    ///
    /// Called at most once for each value of m
    ///
    /// When called for m, data has the following guarantees
    ///  data[m] = true
    ///  data[i] = false for all i > m
    fn set_row(&mut self, m: usize, data: BitArray<V>);
    /// Get a row of the parity reconsturction matrix.
    ///
    /// Note: can be called before corresponding set_row call.
    /// in that case, the result should be all false.
    fn row(&self, m: usize) -> BitArray<V>;
}

/// Storage trait for parity blocks
pub trait ParityStorage<const BLOCKSIZE: usize> {
    /// Store a parity block.
    ///
    /// Called at most once for each value of m.
    fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]);
    /// Get a block previously stored
    ///
    /// Called only after a corresponding store call
    /// for m.
    fn get(&self, m: usize) -> [u8; BLOCKSIZE];
}

/// Storage trait for reconstructed data.
pub trait DataStorage<const BLOCKSIZE: usize> {
    /// Store a newly received or reconstructed block.
    ///
    /// Called exactly once for each value of m in 0..N
    fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]);
    /// Get a block previously received.
    ///
    /// Called only after a corresponding store call
    /// for m.
    fn get(&self, m: usize) -> [u8; BLOCKSIZE];
}

#[derive(Debug, Clone)]
pub struct Reconstructor<Parity, Matrix, ParityData, Data, const BLOCKSIZE: usize, U: BitViewSized, V: BitViewSized> {
    n: usize,
    parity: Parity,
    matrix: Matrix,
    parityblocks: ParityData,
    datablocks: Data,
    used: BitArray<V>,
    _phantom: PhantomData<U>
}

impl<
        Parity: ParityMatrix<U>,
        Matrix: MatrixStorage<U>,
        ParityData: ParityStorage<BLOCKSIZE>,
        Data: DataStorage<BLOCKSIZE>,
        const BLOCKSIZE: usize,
        U: BitViewSized,
    > Reconstructor<Parity, Matrix, ParityData, Data, BLOCKSIZE, U, U>
{
    pub fn new(n: usize, parity: Parity, matrix: Matrix, parityblocks: ParityData, datablocks: Data) -> Self {
        Reconstructor {
            n,
            parity,
            matrix,
            parityblocks,
            datablocks,
            used: BitArray::ZERO,
            _phantom: PhantomData,
        }
    }

    // Returns true when we have sufficient data for reconstruction
    pub fn handle_block(&mut self, index: usize, mut data: [u8; BLOCKSIZE]) -> bool {
        let mut row = self.parity.row(index);
        let mut working_head = self.n - 1;
        loop {
            if row[working_head] && self.used[working_head] {
                // eliminate
                let other_block = self.parityblocks.get(working_head);
                for (i, el) in data.iter_mut().enumerate() {
                    *el ^= other_block[i];
                }
                row ^= self.matrix.row(working_head);
            } else if row[working_head] {
                // new row, store it
                self.matrix.set_row(working_head, row);
                self.parityblocks.store(working_head, data);
                self.used.set(working_head, true);
                break;
            }

            if working_head == 0 {
                break;
            } else {
                working_head -= 1;
            }
        }

        // Check whether we have sufficient data
        if !self.used.iter().take(self.n).any(|v| !v) {
            for i in 0..self.n {
                let mut output = self.parityblocks.get(i);
                let matrix_row = self.matrix.row(i);
                for j in 0..i {
                    if matrix_row[j] {
                        let other_block = self.datablocks.get(j);
                        for (k, el) in output.iter_mut().enumerate() {
                            *el ^= other_block[k];
                        }
                    }
                }
                self.datablocks.store(i, output);
            }
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    extern crate std;

    use core::iter::repeat;
    use std::vec::Vec;

    use super::*;

    struct TestParity<const N: usize>;

    impl<const N: usize, U: BitViewSized> ParityMatrix<U> for TestParity<N> {
        fn row(&self, m: usize) -> BitArray<U> {
            if m < N {
                let mut out = BitArray::ZERO;
                out.set(m, true);
                out
            } else {
                let mut out = BitArray::ZERO;
                for i in 0..N {
                    out.set(i, (m - N) & 1_usize.checked_shl(i as _).unwrap_or(0) != 0);
                }
                out
            }
        }
    }

    struct TestMatrixStorage<V: BitViewSized> {
        data: Vec<BitArray<V>>,
    }

    impl<V: BitViewSized> TestMatrixStorage<V> {
        fn new(n: usize) -> Self {
            Self {
                data: repeat(BitArray::ZERO).take(n).collect(),
            }
        }
    }

    impl<V: BitViewSized> MatrixStorage<V> for TestMatrixStorage<V> {
        fn set_row(&mut self, m: usize, data: BitArray<V>) {
            assert!(data[m]);
            for (i, el) in data.iter().enumerate() {
                assert!(i <= m || !el)
            }
            self.data[m] = data;
        }

        fn row(&self, m: usize) -> BitArray<V> {
            self.data[m].clone()
        }
    }

    struct BlockStorage<const BLOCKSIZE: usize> {
        data: Vec<[u8; BLOCKSIZE]>,
        used: Vec<bool>,
    }

    impl<const BLOCKSIZE: usize> BlockStorage<BLOCKSIZE> {
        fn new(n: usize) -> Self {
            Self {
                data: repeat([0; BLOCKSIZE]).take(n).collect(),
                used: repeat(false).take(n).collect(),
            }
        }
    }

    impl<const BLOCKSIZE: usize> DataStorage<BLOCKSIZE> for BlockStorage<BLOCKSIZE> {
        fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]) {
            assert!(!self.used[m]);
            self.data[m] = data;
            self.used[m] = true;
        }

        fn get(&self, m: usize) -> [u8; BLOCKSIZE] {
            assert!(self.used[m]);
            self.data[m]
        }
    }

    impl<const BLOCKSIZE: usize> ParityStorage<BLOCKSIZE> for BlockStorage<BLOCKSIZE> {
        fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]) {
            assert!(!self.used[m]);
            self.data[m] = data;
            self.used[m] = true;
        }

        fn get(&self, m: usize) -> [u8; BLOCKSIZE] {
            assert!(self.used[m]);
            self.data[m]
        }
    }

    #[test]
    fn simple_reconstruction_test() {
        let mut rec = Reconstructor::new(
            4,
            TestParity::<4>,
            TestMatrixStorage::<[u8;1]>::new(4),
            BlockStorage::new(4),
            BlockStorage::new(4),
        );
        assert_eq!(rec.handle_block(0, [1]), false);
        assert_eq!(rec.handle_block(2, [3]), false);
        assert_eq!(rec.handle_block(9, [2]), false);
        assert_eq!(rec.handle_block(10, [1]), false);
        assert_eq!(rec.handle_block(14, [6]), true);

        assert_eq!(rec.datablocks.used, [true, true, true, true]);
        assert_eq!(rec.datablocks.data, [[1], [2], [3], [4]]);
    }

    #[test]
    fn nontrivial_relation() {
        let mut rec = Reconstructor::new(
            4,
            TestParity::<4>,
            TestMatrixStorage::<[u8;1]>::new(4),
            BlockStorage::new(4),
            BlockStorage::new(4),
        );
        assert_eq!(rec.handle_block(0, [1]), false);
        assert_eq!(rec.handle_block(10, [1]), false);
        assert_eq!(rec.handle_block(14, [6]), false);
        assert_eq!(rec.handle_block(16, [7]), false);
        assert_eq!(rec.handle_block(19, [4]), true);

        assert_eq!(rec.datablocks.used, [true, true, true, true]);
        assert_eq!(rec.datablocks.data, [[1], [2], [3], [4]]);
    }
}
