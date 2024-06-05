use core::iter::repeat;

/// Trait describing a parity matrix.
pub trait ParityMatrix {
    /// Get the mth row of the parity matrix.
    ///
    /// All rows should be of exactly length N.
    ///
    /// For m < N, this should be a row with exactly
    /// one true, in position m.
    fn row(&self, m: usize) -> Vec<bool>;
}

/// Storage trait for matrix data
pub trait MatrixStorage {
    /// Store a row of the parity reconsturction matrix.
    ///
    /// Called at most once for each value of m
    ///
    /// When called for m, data has the following guarantees
    ///  data[m] = true
    ///  data[i] = false for all i > m
    fn set_row(&mut self, m: usize, data: Vec<bool>);
    /// Get a row of the parity reconsturction matrix.
    ///
    /// Note: can be called before corresponding set_row call.
    /// in that case, the result should be all false.
    fn row(&self, m: usize) -> Vec<bool>;
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

    /// Called exactly once for each value of m in 0..N
    fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]);
    /// Get a block previously received.

    /// Called only after a corresponding store call
    /// for m.
    fn get(&self, m: usize) -> [u8; BLOCKSIZE];
}

#[derive(Debug, Clone)]
pub struct Reconstructor<Parity, Matrix, ParityData, Data, const BLOCKSIZE: usize> {
    parity: Parity,
    matrix: Matrix,
    parityblocks: ParityData,
    datablocks: Data,
    used: Vec<bool>,
}

impl<
        Parity: ParityMatrix,
        Matrix: MatrixStorage,
        ParityData: ParityStorage<BLOCKSIZE>,
        Data: DataStorage<BLOCKSIZE>,
        const BLOCKSIZE: usize,
    > Reconstructor<Parity, Matrix, ParityData, Data, BLOCKSIZE>
{
    pub fn new(parity: Parity, matrix: Matrix, parityblocks: ParityData, datablocks: Data) -> Self {
        let n = parity.row(0).len();
        Reconstructor {
            parity,
            matrix,
            parityblocks,
            datablocks,
            used: repeat(false).take(n).collect(),
        }
    }

    // Returns true when we have sufficient data for reconstruction
    pub fn handle_block(&mut self, index: usize, mut data: [u8; BLOCKSIZE]) -> bool {
        let mut row = self.parity.row(index);
        debug_assert_eq!(row.len(), self.used.len());
        let mut working_head = row.len() - 1;
        loop {
            if row[working_head] && self.used[working_head] {
                // eliminate
                let other_block = self.parityblocks.get(working_head);
                for (i, el) in data.iter_mut().enumerate() {
                    *el ^= other_block[i];
                }
                let matrix_row = self.matrix.row(working_head);
                for (i, el) in row.iter_mut().enumerate() {
                    *el = *el != matrix_row[i];
                }
            } else if row[working_head] {
                // new row, store it
                self.matrix.set_row(working_head, row);
                self.parityblocks.store(working_head, data);
                self.used[working_head] = true;
                break;
            }

            if working_head == 0 {
                break;
            } else {
                working_head -= 1;
            }
        }

        // Check whether we have sufficient data
        if !self.used.iter().any(|v| !v) {
            for i in 0..self.used.len() {
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
    use super::*;

    struct TestParity<const N: usize>;

    impl<const N: usize> ParityMatrix for TestParity<N> {
        fn row(&self, m: usize) -> Vec<bool> {
            if m < N {
                let mut out: Vec<_> = repeat(false).take(N).collect();
                out[m] = true;
                out
            } else {
                let mut out = Vec::with_capacity(N);
                for i in 0..N {
                    out.push((m - N) & 1_usize.checked_shl(i as _).unwrap_or(0) != 0);
                }
                out
            }
        }
    }

    struct TestMatrixStorage {
        data: Vec<Vec<bool>>,
    }

    impl TestMatrixStorage {
        fn new(n: usize) -> Self {
            Self {
                data: repeat(repeat(false).take(n).collect()).take(n).collect(),
            }
        }
    }

    impl MatrixStorage for TestMatrixStorage {
        fn set_row(&mut self, m: usize, data: Vec<bool>) {
            assert!(data[m]);
            for (i, el) in data.iter().enumerate() {
                assert!(i <= m || !el)
            }
            self.data[m] = data;
        }

        fn row(&self, m: usize) -> Vec<bool> {
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
            TestParity::<4>,
            TestMatrixStorage::new(4),
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
            TestParity::<4>,
            TestMatrixStorage::new(4),
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
