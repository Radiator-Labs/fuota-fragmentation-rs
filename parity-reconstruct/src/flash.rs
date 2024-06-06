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
    pub fn new(mut flash: F, flash_range: Range<u32>) -> Result<Self, F::Error> {
        flash.erase(flash_range.start, flash_range.end)?;

        Ok(Self { flash, flash_range })
    }
}

impl<F, const N: usize> MatrixStorage<[u8; N]> for FlashMatrixStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    type Error = F::Error;

    fn set_row(&mut self, m: usize, data: BitArray<[u8; N]>) -> Result<(), Self::Error> {
        store(&mut self.flash, &self.flash_range, m, data.as_raw_slice())
    }

    fn row(&mut self, m: usize) -> Result<BitArray<[u8; N]>, Self::Error> {
        let mut data = BitArray::new([0; N]);
        get(
            &mut self.flash,
            &self.flash_range,
            m,
            data.as_raw_mut_slice(),
        )?;
        Ok(data)
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
    pub fn new(mut flash: F, flash_range: Range<u32>) -> Result<Self, F::Error> {
        flash.erase(flash_range.start, flash_range.end)?;

        Ok(Self { flash, flash_range })
    }
}

impl<F, const BLOCKSIZE: usize> ParityStorage<BLOCKSIZE> for FlashParityStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    type Error = F::Error;

    fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]) -> Result<(), Self::Error> {
        store(&mut self.flash, &self.flash_range, m, &data)
    }

    fn get(&mut self, m: usize) -> Result<[u8; BLOCKSIZE], Self::Error> {
        let mut data = [0; BLOCKSIZE];
        get(&mut self.flash, &self.flash_range, m, &mut data)?;
        Ok(data)
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
    pub fn new(mut flash: F, flash_range: Range<u32>) -> Result<Self, F::Error> {
        flash.erase(flash_range.start, flash_range.end)?;

        Ok(Self { flash, flash_range })
    }
}

impl<F, const BLOCKSIZE: usize> DataStorage<BLOCKSIZE> for FlashDataStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    type Error = F::Error;

    fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]) -> Result<(), Self::Error> {
        store(&mut self.flash, &self.flash_range, m, &data)
    }

    fn get(&mut self, m: usize) -> Result<[u8; BLOCKSIZE], Self::Error> {
        let mut data = [0; BLOCKSIZE];
        get(&mut self.flash, &self.flash_range, m, &mut data)?;
        Ok(data)
    }
}

fn store<F: embedded_storage::nor_flash::NorFlash>(
    flash: &mut F,
    flash_range: &Range<u32>,
    index: usize,
    data: &[u8],
) -> Result<(), F::Error> {
    assert!(F::WRITE_SIZE <= 32);

    let rounded_up_len = data.len().next_multiple_of(F::WRITE_SIZE);
    let rounded_down_len = previous_multiple_of(data.len(), F::WRITE_SIZE);
    let offset = index * rounded_up_len;

    // Split so that data0 can be stored directly. Data1 needs to be padded if not empty
    let (data0, data1) = data.split_at(rounded_down_len);

    flash.write(flash_range.start + offset as u32, data0)?;

    if !data1.is_empty() {
        let mut buffer = [0; 32];

        buffer[..data1.len()].copy_from_slice(data1);
        flash.write(
            flash_range.start + offset as u32 + data0.len() as u32,
            &buffer[..F::WRITE_SIZE],
        )?;
    }

    Ok(())
}

fn get<F: embedded_storage::nor_flash::NorFlash>(
    flash: &mut F,
    flash_range: &Range<u32>,
    index: usize,
    data: &mut [u8],
) -> Result<(), F::Error> {
    assert!(F::READ_SIZE <= 32);

    let rounded_up_len = data.len().next_multiple_of(F::READ_SIZE);
    let rounded_down_len = previous_multiple_of(data.len(), F::READ_SIZE);
    let offset = index * rounded_up_len;

    // Split so that data0 can be read directly. Data1 needs to be padding if not empty
    let (data0, data1) = data.split_at_mut(rounded_down_len);

    flash.read(flash_range.start + offset as u32, data0)?;

    if !data1.is_empty() {
        let mut buffer = [0; 32];

        flash.read(
            flash_range.start + offset as u32 + data0.len() as u32,
            &mut buffer[..F::READ_SIZE],
        )?;

        data1.copy_from_slice(&buffer[..data1.len()]);
    }

    Ok(())
}

const fn previous_multiple_of(val: usize, rhs: usize) -> usize {
    let next = val.next_multiple_of(rhs);

    if next > val {
        next.saturating_sub(rhs)
    } else {
        next
    }
}

#[cfg(test)]
mod tests {
    use crate::{tests::TestParity, BlockResult, Reconstructor};

    use super::*;

    #[test]
    fn simple_reconstruction_test() {
        let mut rec = Reconstructor::<_, _, _, _, 1, [u8; 1], [u8; 1]>::new(
            4,
            TestParity::<4>,
            FlashMatrixStorage::new(
                embedded_storage_inmemory::MemFlash::<1024, 128, 4>::new(0),
                0..0x400,
            )
            .unwrap(),
            FlashParityStorage::new(
                embedded_storage_inmemory::MemFlash::<1024, 128, 4>::new(0),
                0..0x400,
            )
            .unwrap(),
            FlashDataStorage::new(
                embedded_storage_inmemory::MemFlash::<1024, 128, 4>::new(0),
                0..0x400,
            )
            .unwrap(),
        );
        assert_eq!(rec.handle_block(0, [1]).unwrap(), BlockResult::NeedMore);
        assert_eq!(rec.handle_block(2, [3]).unwrap(), BlockResult::NeedMore);
        assert_eq!(rec.handle_block(9, [2]).unwrap(), BlockResult::NeedMore);
        assert_eq!(rec.handle_block(10, [1]).unwrap(), BlockResult::NeedMore);
        assert_eq!(rec.handle_block(14, [6]).unwrap(), BlockResult::Done);

        rec.finish()
    }
}
