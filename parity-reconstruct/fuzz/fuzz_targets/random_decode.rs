#![no_main]

use std::{cell::RefCell, iter::repeat, mem::size_of, rc::Rc};

use bitvec::{array::BitArray, order::Lsb0, slice::BitSlice, view::BitViewSized};
use libfuzzer_sys::fuzz_target;
use parity_reconstruct::{
    lfdbt::LfdbtParity, BlockResult, DataStorage, MatrixStorage, ParityMatrix, ParityStorage,
    Reconstructor, ReconstructorData,
};

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

    fn num_rows(&self) -> usize {
        self.data.len()
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

struct WrappedBlockStorage<const BLOCKSIZE: usize>(Rc<RefCell<BlockStorage<BLOCKSIZE>>>);

impl<const BLOCKSIZE: usize> DataStorage for WrappedBlockStorage<BLOCKSIZE> {
    type Error = core::convert::Infallible;

    async fn store(&mut self, m: usize, data: &[u8]) -> Result<(), Self::Error> {
        let mut this = self.0.borrow_mut();
        assert!(!this.used[m]);
        this.data[m] = data.try_into().unwrap();
        this.used[m] = true;
        Ok(())
    }

    async fn get(&mut self, m: usize, buf: &mut [u8]) -> Result<(), Self::Error> {
        let this = self.0.borrow();
        assert!(this.used[m]);
        buf.copy_from_slice(&this.data[m]);
        Ok(())
    }
}

impl<const BLOCKSIZE: usize> ParityStorage for BlockStorage<BLOCKSIZE> {
    type Error = core::convert::Infallible;

    async fn store(&mut self, m: usize, data: &[u8]) -> Result<(), Self::Error> {
        assert!(!self.used[m]);
        self.data[m] = data.try_into().unwrap();
        self.used[m] = true;
        Ok(())
    }

    async fn get(&mut self, m: usize, buf: &mut [u8]) -> Result<(), Self::Error> {
        assert!(self.used[m]);
        buf.copy_from_slice(&self.data[m]);
        Ok(())
    }
}

fuzz_target!(|testcase: (Vec<[u8; 4]>, &[u8])| {
    // This fuzzer simulates reconstruction of data from a subset of blocks
    // which is received. The data is provided in testcase.0, with testcase.1
    // marking which blocks are received and which not.

    let data = &testcase.0;
    let received: &BitSlice<_, Lsb0> = BitSlice::from_slice(testcase.1);
    // Ensure there is at least a chance of success
    if data.len() < 2 || data.len() > 1024 || data.len() + 16 > received.count_ones() {
        return;
    }

    let matrix = LfdbtParity::new(data.len());
    let datastore = Rc::new(RefCell::new(BlockStorage::new(data.len())));
    let mut recdata = ReconstructorData::<[usize; 1024 / size_of::<usize>()],[usize; 1024 / size_of::<usize>()]>::new(data.len(), 4);
    let mut reconstructor: Reconstructor<_, _, _, _, 4, _, _> = recdata.hydrate(
        LfdbtParity::new(data.len()),
        TestMatrixStorage::new(data.len()),
        BlockStorage::<4>::new(data.len()),
        WrappedBlockStorage(datastore.clone()),
    );

    // Which data blocks have been included in the blocks provided sofar?
    // we use this to check that there isn't a very obvious gap in what was
    // provided.
    let mut total: BitArray<[usize; 1024 / size_of::<usize>()]> = BitArray::ZERO;

    for (i, provide) in received.iter().enumerate() {
        if *provide {
            // Block i isn't simulated as lost, so construct it.
            let row: BitArray<[usize; 1024 / size_of::<usize>()]> = matrix.row(i);
            total |= row;
            let mut block = [0; 4];
            for (j, take) in row.iter().enumerate().take(data.len()) {
                if *take {
                    for (blockel, datael) in block.iter_mut().zip(data[j].iter()) {
                        *blockel ^= *datael
                    }
                }
            }

            match futures::executor::block_on(reconstructor.handle_block(i, &block)).unwrap() {
                BlockResult::NeedMore => {}
                BlockResult::TooManyMissing => panic!("Should always fit"),
                BlockResult::Done(_) => {
                    let store = datastore.borrow();
                    assert_eq!(&store.data, data);
                    assert!(!store.used.iter().any(|v| !*v));
                    return;
                }
            }
        }
    }

    // Note: Failure here isn't necessarily a bug, this can also be triggered with well constructed block sequences, at least in theory.
    // If this fails again, consider using a cprng for the block sequence instead.
    assert!(total.count_ones() != data.len());
});
