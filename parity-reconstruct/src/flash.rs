//! Flash implementation based on [embedded-storage] for the varying traits of the crate
//!

use core::ops::Range;

use bitvec::array::BitArray;

use crate::{DataStorage, MatrixStorage, ParityStorage};

pub struct FlashMatrixStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    flash: F,
    flash_range: Range<u32>,
}

impl<F> FlashMatrixStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    pub fn new(flash: F, flash_range: Range<u32>) -> Self {
        Self { flash, flash_range }
    }
}

impl<F, const N: usize> MatrixStorage<[u8; N]> for FlashMatrixStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    type Error = F::Error;

    fn set_row(&mut self, m: usize, data: BitArray<[u8; N]>) -> Result<(), Self::Error> {
        todo!()
    }

    fn row(&self, m: usize) -> Result<BitArray<[u8; N]>, Self::Error> {
        todo!()
    }
}

pub struct FlashParityStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    flash: F,
    flash_range: Range<u32>,
}

impl<F> FlashParityStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    pub fn new(flash: F, flash_range: Range<u32>) -> Self {
        Self { flash, flash_range }
    }
}

impl<F, const BLOCKSIZE: usize> ParityStorage<BLOCKSIZE> for FlashParityStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    type Error = F::Error;

    fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]) -> Result<(), Self::Error> {
        todo!()
    }

    fn get(&self, m: usize) -> Result<[u8; BLOCKSIZE], Self::Error> {
        todo!()
    }
}

pub struct FlashDataStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    flash: F,
    flash_range: Range<u32>,
}

impl<F> FlashDataStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    pub fn new(flash: F, flash_range: Range<u32>) -> Self {
        Self { flash, flash_range }
    }
}

impl<F, const BLOCKSIZE: usize> DataStorage<BLOCKSIZE> for FlashDataStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    type Error = F::Error;

    fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]) -> Result<(), Self::Error> {
        todo!()
    }

    fn get(&self, m: usize) -> Result<[u8; BLOCKSIZE], Self::Error> {
        todo!()
    }
}
