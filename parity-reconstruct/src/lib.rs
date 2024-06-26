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
#![deny(missing_docs)]
#![allow(clippy::type_complexity)]
#![allow(async_fn_in_trait)]

#[cfg(feature = "flash")]
pub mod flash;

pub mod lfdbt;

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
    ///
    /// m can be arbirarily large.
    fn row(&self, m: usize) -> BitArray<U>;
}

/// Storage trait for matrix data
///
/// Note: the maximum number of missing data blocks that we
/// can handle is assumed to be BitArray<V>.
pub trait MatrixStorage<V: BitViewSized> {
    /// The error type of the storage
    type Error;
    /// Store a row of the parity reconstruction matrix.
    ///
    /// Called at most once for each value of m
    ///
    /// When called for m, data has the following guarantees
    /// - `data[m]` = true
    /// - `data[i]` = false for all i > m
    ///
    /// m is smaller than BitArray<V>::len()
    async fn set_row(&mut self, m: usize, data: BitArray<V>) -> Result<(), Self::Error>;
    /// Get a row of the parity reconsturction matrix.
    ///
    /// Note: can be called before corresponding set_row call.
    /// in that case, the result should be all false.
    ///
    /// m is smaller than BitArray<V>::len()
    async fn row(&mut self, m: usize) -> Result<BitArray<V>, Self::Error>;
}

/// Storage trait for parity blocks
pub trait ParityStorage<const BLOCKSIZE: usize> {
    /// The error type of the storage
    type Error;
    /// Store a parity block.
    ///
    /// Called at most once for each value of m.
    ///
    /// m is smaller than BitArray<V>::len()
    async fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]) -> Result<(), Self::Error>;
    /// Get a block previously stored
    ///
    /// Called only after a corresponding store call
    /// for m.
    ///
    /// m is smaller than BitArray<V>::len()
    async fn get(&mut self, m: usize) -> Result<[u8; BLOCKSIZE], Self::Error>;
}

/// Storage trait for reconstructed data.
pub trait DataStorage<const BLOCKSIZE: usize> {
    /// The error type of the storage
    type Error;
    /// Store a newly received or reconstructed block.
    ///
    /// Called exactly once for each value of m in 0..N
    async fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]) -> Result<(), Self::Error>;
    /// Get a block previously received.
    ///
    /// Called only after a corresponding store call
    /// for m.
    async fn get(&mut self, m: usize) -> Result<[u8; BLOCKSIZE], Self::Error>;
}

/// The error type of the reconstructor
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error<M, P, D> {
    /// The matrix storage returned an error
    MatrixStorageError(M),
    /// The parity storage returned an error
    ParityStorageError(P),
    /// The data storage returned an error
    DataStorageError(D),
}

/// Status after handling a new block
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockResult {
    /// More data is needed before a complete
    /// reconstruction can be made
    NeedMore,
    /// Too many data blocks are still missing
    /// to accept a parity block.
    TooManyMissing,
    /// Sufficient data has been received, the
    /// data store now contains a full copy of
    /// the original data.
    ///
    /// The value is the number of bytes of data.
    Done(usize),
}

/// Runtime data for the reconstructor
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ReconstructorData<U: BitViewSized, V: BitViewSized> {
    n: usize,
    l: usize,
    done: BitArray<U>,
    used: BitArray<V>,
}

/// Reconstructor for reconstructing data
/// from an incomplete stream of blocks and
/// parity data.
#[derive(Debug, Clone)]
pub struct Reconstructor<
    Parity,
    Matrix,
    ParityData,
    Data,
    const BLOCKSIZE: usize,
    U: BitViewSized,
    V: BitViewSized,
> {
    n: usize,
    l: usize,
    parity: Parity,
    matrix: Matrix,
    parityblocks: ParityData,
    datablocks: Data,
    done: BitArray<U>,
    used: BitArray<V>,
}

impl<
        Parity: ParityMatrix<U>,
        Matrix: MatrixStorage<V>,
        ParityData: ParityStorage<BLOCKSIZE>,
        Data: DataStorage<BLOCKSIZE>,
        const BLOCKSIZE: usize,
        U: BitViewSized,
        V: BitViewSized,
    > Reconstructor<Parity, Matrix, ParityData, Data, BLOCKSIZE, U, V>
{
    /// Create a new reconstructor. The matrix storage, parity block
    /// storage and data block storage are all assumed to be empty
    /// to start (e.g., any index can still be written to).
    pub fn new(
        n: usize,
        parity: Parity,
        matrix: Matrix,
        parityblocks: ParityData,
        datablocks: Data,
    ) -> Self {
        debug_assert!(n <= BitArray::<U>::ZERO.len());
        Reconstructor {
            n,
            l: 0,
            parity,
            matrix,
            parityblocks,
            datablocks,
            done: BitArray::ZERO,
            used: BitArray::ZERO,
        }
    }

    /// Create a new reconstructor, continuing from a previous run.
    /// The storage is assumed to be in the same state as before.
    pub fn new_from_previous_run(
        data: ReconstructorData<U, V>,
        parity: Parity,
        matrix: Matrix,
        parityblocks: ParityData,
        datablocks: Data,
    ) -> Self {
        Reconstructor {
            n: data.n,
            l: data.l,
            parity,
            matrix,
            parityblocks,
            datablocks,
            done: data.done,
            used: data.used,
        }
    }

    // Do the preprocessing needed to start handling parity blocks
    // This is called once the first parity block is received
    fn init_stage2(&mut self) {
        debug_assert_eq!(self.l, 0);

        self.l = self.done.iter().take(self.n).filter(|v| !**v).count();
    }

    // Should we have sufficient data? Note, this does not tell
    // itself if final processing has already been done. That needs
    // to be done on the first block after which this is true.
    fn is_complete(&self) -> bool {
        (self.l == 0 && !self.done.iter().take(self.n).any(|v| !*v))
            || (self.l != 0 && !self.used.iter().take(self.l).any(|v| !*v))
    }

    // Handle a data blcok during stage 1
    async fn handle_data_block(
        &mut self,
        index: usize,
        data: [u8; BLOCKSIZE],
    ) -> Result<(), Error<Matrix::Error, ParityData::Error, Data::Error>> {
        debug_assert!(self.n > index);
        debug_assert_eq!(self.l, 0);

        if !self.done[index] {
            self.done.set(index, true);
            self.datablocks
                .store(index, data)
                .await
                .map_err(Error::DataStorageError)?;
        }

        Ok(())
    }

    // Handle a parity block during stage 2
    async fn handle_parity_block(
        &mut self,
        index: usize,
        mut data: [u8; BLOCKSIZE],
    ) -> Result<(), Error<Matrix::Error, ParityData::Error, Data::Error>> {
        debug_assert_ne!(self.l, 0);
        let row = self.parity.row(index);

        // remove parity from already received datablocks.
        for (i, (rowel, have_data)) in row.iter().zip(self.done.iter()).enumerate().take(self.n) {
            if *rowel && *have_data {
                let other_block = self
                    .datablocks
                    .get(i)
                    .await
                    .map_err(Error::DataStorageError)?;
                for (datael, otherel) in data.iter_mut().zip(other_block.iter()) {
                    *datael ^= *otherel
                }
            }
        }

        // Construct reduced matrix row
        let mut reducedrow: BitArray<V> = BitArray::ZERO;
        let mut j = 0;
        for (rowel, have_data) in row.iter().zip(self.done.iter()).take(self.n) {
            if !*have_data {
                reducedrow.set(j, *rowel);
                j += 1;
            }
        }
        debug_assert_eq!(j, self.l);

        // not strictly neccessary, but catches incorrect references to row below
        drop(row);

        // Reduce further to upper triangular form, and store
        let mut working_head = self.l - 1;
        loop {
            if reducedrow[working_head] && self.used[working_head] {
                // eliminate
                let other_block = self
                    .parityblocks
                    .get(working_head)
                    .await
                    .map_err(Error::ParityStorageError)?;
                for (i, el) in data.iter_mut().enumerate() {
                    *el ^= other_block[i];
                }
                reducedrow ^= self
                    .matrix
                    .row(working_head)
                    .await
                    .map_err(Error::MatrixStorageError)?;
            } else if reducedrow[working_head] {
                // new row, store it
                self.matrix
                    .set_row(working_head, reducedrow)
                    .await
                    .map_err(Error::MatrixStorageError)?;
                self.parityblocks
                    .store(working_head, data)
                    .await
                    .map_err(Error::ParityStorageError)?;
                self.used.set(working_head, true);
                break;
            }

            if working_head == 0 {
                break;
            } else {
                working_head -= 1;
            }
        }

        Ok(())
    }

    // Convert a reduced index to an index in the full data array.
    fn reduced_to_full(&self, mut index: usize) -> usize {
        debug_assert!(index < self.l);

        for (i, have_data) in self.done.iter().enumerate().take(self.n) {
            if !*have_data {
                if index == 0 {
                    return i;
                } else {
                    index -= 1;
                }
            }
        }

        panic!("Invalid reduced index");
    }

    // Reconstruct the final data blocks from parity.
    async fn finish(&mut self) -> Result<(), Error<Matrix::Error, ParityData::Error, Data::Error>> {
        for i in 0..self.l {
            let mut output = self
                .parityblocks
                .get(i)
                .await
                .map_err(Error::ParityStorageError)?;
            let matrix_row = self
                .matrix
                .row(i)
                .await
                .map_err(Error::MatrixStorageError)?;
            for j in 0..i {
                if matrix_row[j] {
                    let other_block = self
                        .datablocks
                        .get(self.reduced_to_full(j))
                        .await
                        .map_err(Error::DataStorageError)?;
                    for (k, el) in output.iter_mut().enumerate() {
                        *el ^= other_block[k];
                    }
                }
            }
            self.datablocks
                .store(self.reduced_to_full(i), output)
                .await
                .map_err(Error::DataStorageError)?;
        }

        Ok(())
    }

    /// Handle a single block that has been received. The index is the index of the
    /// row in the parity matrix that was used to generate the data block.
    pub async fn handle_block(
        &mut self,
        index: usize,
        data: [u8; BLOCKSIZE],
    ) -> Result<BlockResult, Error<Matrix::Error, ParityData::Error, Data::Error>> {
        if self.is_complete() {
            return Ok(BlockResult::Done(self.n * BLOCKSIZE));
        }

        if index >= self.n && self.l == 0 {
            self.init_stage2();

            if BitArray::<V>::ZERO.len() < self.l {
                // Given this value of l, we would
                // overflow the matrix rows, abort.
                self.l = 0;
                return Ok(BlockResult::TooManyMissing);
            }
        }

        // Self.l indicates in which stage we are
        // l==0 => stage 1 (just data blocks)
        // l>0 => stage 2 (processing parity)
        if self.l == 0 {
            self.handle_data_block(index, data).await?;
            if self.is_complete() {
                Ok(BlockResult::Done(self.n * BLOCKSIZE))
            } else {
                Ok(BlockResult::NeedMore)
            }
        } else {
            self.handle_parity_block(index, data).await?;

            if self.is_complete() {
                // Ensure we actually produce the
                // final data blocks.
                self.finish().await?;
                Ok(BlockResult::Done(self.n * BLOCKSIZE))
            } else {
                Ok(BlockResult::NeedMore)
            }
        }
    }

    /// Deconstruct the objects to its component parts.
    ///
    /// If you're in the middle of operations, you can do this and later continue with [Self::new_from_previous_run].
    pub fn free(self) -> (ReconstructorData<U, V>, Parity, Matrix, ParityData, Data) {
        let Self {
            n,
            l,
            parity,
            matrix,
            parityblocks,
            datablocks,
            done,
            used,
        } = self;

        (
            ReconstructorData { n, l, done, used },
            parity,
            matrix,
            parityblocks,
            datablocks,
        )
    }
}

#[cfg(test)]
pub(crate) mod tests {
    extern crate std;

    use core::iter::repeat;
    use std::vec::Vec;

    use super::*;

    pub(crate) struct TestParity<const N: usize>;

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
        type Error = core::convert::Infallible;

        async fn set_row(&mut self, m: usize, data: BitArray<V>) -> Result<(), Self::Error> {
            assert!(data[m]);
            for (i, el) in data.iter().enumerate() {
                assert!(i <= m || !el)
            }
            self.data[m] = data;

            Ok(())
        }

        async fn row(&mut self, m: usize) -> Result<BitArray<V>, Self::Error> {
            Ok(self.data[m].clone())
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
        type Error = core::convert::Infallible;

        async fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]) -> Result<(), Self::Error> {
            assert!(!self.used[m]);
            self.data[m] = data;
            self.used[m] = true;

            Ok(())
        }

        async fn get(&mut self, m: usize) -> Result<[u8; BLOCKSIZE], Self::Error> {
            assert!(self.used[m]);
            Ok(self.data[m])
        }
    }

    impl<const BLOCKSIZE: usize> ParityStorage<BLOCKSIZE> for BlockStorage<BLOCKSIZE> {
        type Error = core::convert::Infallible;

        async fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]) -> Result<(), Self::Error> {
            assert!(!self.used[m]);
            self.data[m] = data;
            self.used[m] = true;

            Ok(())
        }

        async fn get(&mut self, m: usize) -> Result<[u8; BLOCKSIZE], Self::Error> {
            assert!(self.used[m]);
            Ok(self.data[m])
        }
    }

    #[futures_test::test]
    async fn simple_reconstruction_test() {
        let mut rec = Reconstructor::<_, _, _, _, 1, [u8; 1], [u8; 1]>::new(
            4,
            TestParity::<4>,
            TestMatrixStorage::new(2),
            BlockStorage::new(2),
            BlockStorage::new(4),
        );
        assert_eq!(rec.handle_block(0, [1]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(2, [3]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(9, [2]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(10, [1]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(14, [6]).await, Ok(BlockResult::Done(4)));

        assert_eq!(rec.datablocks.used, [true, true, true, true]);
        assert_eq!(rec.datablocks.data, [[1], [2], [3], [4]]);
    }

    #[futures_test::test]
    async fn nontrivial_relation() {
        let mut rec = Reconstructor::<_, _, _, _, 1, [u8; 1], [u8; 1]>::new(
            4,
            TestParity::<4>,
            TestMatrixStorage::new(3),
            BlockStorage::new(3),
            BlockStorage::new(4),
        );
        assert_eq!(rec.handle_block(0, [1]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(10, [1]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(14, [6]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(16, [7]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(19, [4]).await, Ok(BlockResult::Done(4)));

        assert_eq!(rec.datablocks.used, [true, true, true, true]);
        assert_eq!(rec.datablocks.data, [[1], [2], [3], [4]]);
    }

    #[futures_test::test]
    async fn no_parity_needed() {
        let mut rec = Reconstructor::<_, _, _, _, 1, [u8; 1], [u8; 1]>::new(
            4,
            TestParity::<4>,
            TestMatrixStorage::new(0),
            BlockStorage::new(0),
            BlockStorage::new(4),
        );

        assert_eq!(rec.handle_block(0, [1]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(1, [2]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(2, [3]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(3, [4]).await, Ok(BlockResult::Done(4)));

        assert_eq!(rec.datablocks.used, [true, true, true, true]);
        assert_eq!(rec.datablocks.data, [[1], [2], [3], [4]]);
    }

    #[futures_test::test]
    async fn repeats_are_ok() {
        let mut rec = Reconstructor::<_, _, _, _, 1, [u8; 1], [u8; 1]>::new(
            4,
            TestParity::<4>,
            TestMatrixStorage::new(0),
            BlockStorage::new(0),
            BlockStorage::new(4),
        );

        assert_eq!(rec.handle_block(0, [1]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(0, [1]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(1, [2]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(1, [2]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(2, [3]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(2, [3]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(3, [4]).await, Ok(BlockResult::Done(4)));
        assert_eq!(rec.handle_block(3, [4]).await, Ok(BlockResult::Done(4)));

        assert_eq!(rec.datablocks.used, [true, true, true, true]);
        assert_eq!(rec.datablocks.data, [[1], [2], [3], [4]]);
    }

    #[futures_test::test]
    async fn too_many_missing() {
        let mut rec = Reconstructor::<_, _, _, _, 1, [u8; 2], [u8; 1]>::new(
            16,
            TestParity::<16>,
            TestMatrixStorage::new(8),
            BlockStorage::new(8),
            BlockStorage::new(16),
        );

        assert_eq!(rec.handle_block(0, [0]).await, Ok(BlockResult::NeedMore));
        assert_eq!(
            rec.handle_block(19, [1]).await,
            Ok(BlockResult::TooManyMissing)
        );
        assert_eq!(rec.handle_block(1, [1]).await, Ok(BlockResult::NeedMore));
        assert_eq!(
            rec.handle_block(19, [1]).await,
            Ok(BlockResult::TooManyMissing)
        );
        assert_eq!(rec.handle_block(2, [2]).await, Ok(BlockResult::NeedMore));
        assert_eq!(
            rec.handle_block(19, [1]).await,
            Ok(BlockResult::TooManyMissing)
        );
        assert_eq!(rec.handle_block(3, [3]).await, Ok(BlockResult::NeedMore));
        assert_eq!(
            rec.handle_block(19, [1]).await,
            Ok(BlockResult::TooManyMissing)
        );
        assert_eq!(rec.handle_block(4, [4]).await, Ok(BlockResult::NeedMore));
        assert_eq!(
            rec.handle_block(19, [1]).await,
            Ok(BlockResult::TooManyMissing)
        );
        assert_eq!(rec.handle_block(5, [5]).await, Ok(BlockResult::NeedMore));
        assert_eq!(
            rec.handle_block(19, [1]).await,
            Ok(BlockResult::TooManyMissing)
        );
        assert_eq!(rec.handle_block(6, [6]).await, Ok(BlockResult::NeedMore));
        assert_eq!(
            rec.handle_block(19, [1]).await,
            Ok(BlockResult::TooManyMissing)
        );
        assert_eq!(rec.handle_block(7, [7]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(19, [1]).await, Ok(BlockResult::NeedMore));
    }

    #[futures_test::test]
    async fn out_of_order() {
        let mut rec = Reconstructor::<_, _, _, _, 1, [u8; 1], [u8; 1]>::new(
            4,
            TestParity::<4>,
            TestMatrixStorage::new(3),
            BlockStorage::new(3),
            BlockStorage::new(4),
        );
        assert_eq!(rec.handle_block(0, [1]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(14, [6]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(9, [2]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(2, [3]).await, Ok(BlockResult::NeedMore));
        assert_eq!(rec.handle_block(10, [1]).await, Ok(BlockResult::Done(4)));
        assert_eq!(rec.handle_block(10, [1]).await, Ok(BlockResult::Done(4)));

        assert_eq!(rec.datablocks.used, [true, true, true, true]);
        assert_eq!(rec.datablocks.data, [[1], [2], [3], [4]]);
    }
}
