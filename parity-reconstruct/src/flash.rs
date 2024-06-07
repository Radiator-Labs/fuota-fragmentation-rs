//! Flash implementation based on [embedded-storage] for the varying traits of the crate
//!

use core::{cell::RefCell, ops::Range};

use bitvec::array::BitArray;

use crate::{DataStorage, MatrixStorage, ParityStorage};

const MAX_WORD_SIZE: usize = 32;

pub struct SharedFlash<'a, F> {
    flash: RefCell<&'a mut F>,
}

impl<'a, F> SharedFlash<'a, F> {
    pub fn new(flash: RefCell<&'a mut F>) -> Self {
        Self { flash }
    }
}

impl<'a, F: embedded_storage::nor_flash::ErrorType> embedded_storage::nor_flash::ErrorType
    for &SharedFlash<'a, F>
{
    type Error = F::Error;
}

impl<'a, F: embedded_storage::nor_flash::ReadNorFlash> embedded_storage::nor_flash::ReadNorFlash
    for &SharedFlash<'a, F>
{
    const READ_SIZE: usize = F::READ_SIZE;

    fn read(&mut self, offset: u32, bytes: &mut [u8]) -> Result<(), Self::Error> {
        self.flash.borrow_mut().read(offset, bytes)
    }

    fn capacity(&self) -> usize {
        self.flash.borrow_mut().capacity()
    }
}

impl<'a, F: embedded_storage::nor_flash::NorFlash> embedded_storage::nor_flash::NorFlash
    for &SharedFlash<'a, F>
{
    const WRITE_SIZE: usize = F::WRITE_SIZE;

    const ERASE_SIZE: usize = F::ERASE_SIZE;

    fn erase(&mut self, from: u32, to: u32) -> Result<(), Self::Error> {
        self.flash.borrow_mut().erase(from, to)
    }

    fn write(&mut self, offset: u32, bytes: &[u8]) -> Result<(), Self::Error> {
        self.flash.borrow_mut().write(offset, bytes)
    }
}

impl<'a, F: embedded_storage::nor_flash::MultiwriteNorFlash>
    embedded_storage::nor_flash::MultiwriteNorFlash for &SharedFlash<'a, F>
{
}

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
        todo!()
    }

    fn row(&mut self, m: usize) -> Result<BitArray<[u8; N]>, Self::Error> {
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
    pub fn new(mut flash: F, flash_range: Range<u32>) -> Result<Self, F::Error> {
        flash.erase(flash_range.start, flash_range.end)?;

        Ok(Self { flash, flash_range })
    }
}

/// Implemented with a simple algorithm.
/// Between every parity block there is a little bit of potential padding.
impl<F, const BLOCKSIZE: usize> ParityStorage<BLOCKSIZE> for FlashParityStorage<F>
where
    F: embedded_storage::nor_flash::NorFlash,
{
    type Error = F::Error;

    fn store(&mut self, m: usize, data: [u8; BLOCKSIZE]) -> Result<(), Self::Error> {
        assert!(F::WRITE_SIZE <= MAX_WORD_SIZE);
        assert!((F::WRITE_SIZE / F::READ_SIZE).is_power_of_two());

        let rounded_up_len = data.len().next_multiple_of(F::WRITE_SIZE);
        let rounded_down_len = previous_multiple_of(data.len(), F::WRITE_SIZE);
        let offset = m * rounded_up_len;

        // Split so that data0 can be stored directly. Data1 needs to be padded if not empty
        let (data0, data1) = data.split_at(rounded_down_len);

        self.flash
            .write(self.flash_range.start + offset as u32, data0)?;

        if !data1.is_empty() {
            let mut buffer = [0; MAX_WORD_SIZE];

            buffer[..data1.len()].copy_from_slice(data1);
            self.flash.write(
                self.flash_range.start + offset as u32 + data0.len() as u32,
                &buffer[..F::WRITE_SIZE],
            )?;
        }

        Ok(())
    }

    fn get(&mut self, m: usize) -> Result<[u8; BLOCKSIZE], Self::Error> {
        let mut data = [0; BLOCKSIZE];

        assert!(F::WRITE_SIZE <= MAX_WORD_SIZE);
        assert!((F::WRITE_SIZE / F::READ_SIZE).is_power_of_two());

        let rounded_up_len = data.len().next_multiple_of(F::READ_SIZE);
        let rounded_down_len = previous_multiple_of(data.len(), F::READ_SIZE);
        let offset = m * rounded_up_len;

        // Split so that data0 can be read directly. Data1 needs to be padding if not empty
        let (data0, data1) = data.split_at_mut(rounded_down_len);

        self.flash
            .read(self.flash_range.start + offset as u32, data0)?;

        if !data1.is_empty() {
            let mut buffer = [0; MAX_WORD_SIZE];

            self.flash.read(
                self.flash_range.start + offset as u32 + data0.len() as u32,
                &mut buffer[..F::READ_SIZE],
            )?;

            data1.copy_from_slice(&buffer[..data1.len()]);
        }

        Ok(data)
    }
}

pub struct FlashDataStorage<F>
where
    F: embedded_storage::nor_flash::MultiwriteNorFlash,
{
    flash: F,
    flash_range: Range<u32>,
}

impl<F> FlashDataStorage<F>
where
    F: embedded_storage::nor_flash::MultiwriteNorFlash,
{
    pub fn new(mut flash: F, flash_range: Range<u32>) -> Result<Self, F::Error> {
        flash.erase(flash_range.start, flash_range.end)?;

        Ok(Self { flash, flash_range })
    }

    fn split_slice_addrs<const BLOCKSIZE: usize>(
        m: usize,
        data: &mut [u8; BLOCKSIZE],
    ) -> ((&mut [u8], u32), (&mut [u8], u32), (&mut [u8], u32)) {
        assert!(F::WRITE_SIZE <= MAX_WORD_SIZE);
        assert!((F::WRITE_SIZE / F::READ_SIZE).is_power_of_two());
        assert!(BLOCKSIZE >= F::WRITE_SIZE);

        let true_start_address = (m * BLOCKSIZE) as u32;
        let start_address_align_offset = true_start_address % F::WRITE_SIZE as u32;
        let true_end_address = ((m + 1) * BLOCKSIZE) as u32;
        let end_address_align_offset = true_end_address % F::WRITE_SIZE as u32;

        let padded_start_address = if start_address_align_offset != 0 {
            true_start_address - F::WRITE_SIZE as u32 + start_address_align_offset
        } else {
            true_start_address
        };

        let body_start_address = if start_address_align_offset != 0 {
            padded_start_address + F::WRITE_SIZE as u32
        } else {
            true_start_address
        };

        let (pad_start, body) = if start_address_align_offset != 0 {
            data.split_at_mut(F::WRITE_SIZE - (true_start_address - padded_start_address) as usize)
        } else {
            (&mut [][..], &mut data[..])
        };

        let (body, pad_end) = if end_address_align_offset != 0 {
            body.split_at_mut(body.len() - end_address_align_offset as usize)
        } else {
            (&mut body[..], &mut [][..])
        };

        let body_len = body.len();

        (
            (pad_start, padded_start_address),
            (body, body_start_address),
            (pad_end, body_start_address + body_len as u32),
        )
    }
}

/// Algorithm that doesn't have any padding between blocks
impl<F, const BLOCKSIZE: usize> DataStorage<BLOCKSIZE> for FlashDataStorage<F>
where
    F: embedded_storage::nor_flash::MultiwriteNorFlash,
{
    type Error = F::Error;

    fn store(&mut self, m: usize, mut data: [u8; BLOCKSIZE]) -> Result<(), Self::Error> {
        let ((pad_start, start_addr), (body, body_addr), (pad_end, end_addr)) =
            Self::split_slice_addrs(m, &mut data);

        if !pad_start.is_empty() {
            let mut buffer = [0xFF; MAX_WORD_SIZE];
            buffer[..MAX_WORD_SIZE - pad_start.len()].copy_from_slice(pad_start);

            self.flash
                .write(start_addr, &buffer[MAX_WORD_SIZE - F::WRITE_SIZE..])?;
        }

        self.flash.write(body_addr, body)?;

        if !pad_end.is_empty() {
            let mut buffer = [0xFF; MAX_WORD_SIZE];
            buffer[..pad_end.len()].copy_from_slice(pad_end);

            self.flash.write(end_addr, &buffer[..F::WRITE_SIZE])?;
        }

        Ok(())
    }

    fn get(&mut self, m: usize) -> Result<[u8; BLOCKSIZE], Self::Error> {
        let mut data = [0; BLOCKSIZE];

        let ((pad_start, start_addr), (body, body_addr), (pad_end, end_addr)) =
            Self::split_slice_addrs(m, &mut data);

        if !pad_start.is_empty() {
            let mut buffer = [0x00; MAX_WORD_SIZE];
            self.flash
                .read(start_addr, &mut buffer[MAX_WORD_SIZE - F::WRITE_SIZE..])?;

            pad_start.copy_from_slice(&buffer[..MAX_WORD_SIZE - pad_start.len()]);
        }

        self.flash.read(body_addr, body)?;

        if !pad_end.is_empty() {
            let mut buffer = [0x00; MAX_WORD_SIZE];
            self.flash.read(end_addr, &mut buffer[..F::WRITE_SIZE])?;

            pad_end.copy_from_slice(&buffer[..pad_end.len()]);
        }

        Ok(data)
    }
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
            LfdbtParity::new(4),
            FlashMatrixStorage::new(mem_flash::MemFlash::<1024, 128, 1>::new(0), 0..0x400).unwrap(),
            FlashParityStorage::new(mem_flash::MemFlash::<1024, 128, 1>::new(0), 0..0x400).unwrap(),
            FlashDataStorage::new(mem_flash::MemFlash::<1024, 128, 1>::new(0), 0..0x400).unwrap(),
        );
        assert_eq!(rec.handle_block(0, [1]).unwrap(), BlockResult::NeedMore);
        assert_eq!(rec.handle_block(2, [3]).unwrap(), BlockResult::NeedMore);
        assert_eq!(rec.handle_block(9, [2]).unwrap(), BlockResult::NeedMore);
        assert_eq!(rec.handle_block(10, [1]).unwrap(), BlockResult::NeedMore);
        assert_eq!(rec.handle_block(14, [6]).unwrap(), BlockResult::Done);
    }

    // Taken and adapted from: https://github.com/lulf/embedded-storage-inmemory
    mod mem_flash {
        use embedded_storage::nor_flash::{
            ErrorType, MultiwriteNorFlash, NorFlash, NorFlashError, NorFlashErrorKind, ReadNorFlash,
        };

        pub struct MemFlash<const SIZE: usize, const ERASE_SIZE: usize, const WRITE_SIZE: usize> {
            pub mem: [u8; SIZE],
            pub written: [bool; SIZE],
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct MemFlashError;

        impl<const SIZE: usize, const ERASE_SIZE: usize, const WRITE_SIZE: usize>
            MemFlash<SIZE, ERASE_SIZE, WRITE_SIZE>
        {
            pub const fn new(fill: u8) -> Self {
                Self {
                    mem: [fill; SIZE],
                    written: [false; SIZE],
                }
            }

            fn read(&mut self, offset: u32, bytes: &mut [u8]) -> Result<(), MemFlashError> {
                let len = bytes.len();
                bytes.copy_from_slice(&self.mem[offset as usize..offset as usize + len]);
                Ok(())
            }

            fn write(&mut self, offset: u32, bytes: &[u8]) -> Result<(), MemFlashError> {
                let offset = offset as usize;
                assert!(bytes.len() % WRITE_SIZE == 0);
                assert!(offset % WRITE_SIZE == 0);
                assert!(offset + bytes.len() <= SIZE);

                for written in self.written.iter_mut().skip(offset).take(bytes.len()) {
                    if *written {
                        panic!("Written twice");
                    }

                    *written = true;
                }

                for (mem_byte, new_byte) in self
                    .mem
                    .iter_mut()
                    .skip(offset)
                    .take(bytes.len())
                    .zip(bytes)
                {
                    *mem_byte &= *new_byte;
                }

                Ok(())
            }

            fn erase(&mut self, from: u32, to: u32) -> Result<(), MemFlashError> {
                let from = from as usize;
                let to = to as usize;
                assert!(from % ERASE_SIZE == 0);
                assert!(
                    to % ERASE_SIZE == 0,
                    "To: {}, erase size: {}",
                    to,
                    ERASE_SIZE
                );
                for i in from..to {
                    self.written[i] = false;
                    self.mem[i] = 0xFF;
                }
                Ok(())
            }
        }

        impl<const SIZE: usize, const ERASE_SIZE: usize, const WRITE_SIZE: usize> Default
            for MemFlash<SIZE, ERASE_SIZE, WRITE_SIZE>
        {
            fn default() -> Self {
                Self::new(0xFF)
            }
        }

        impl<const SIZE: usize, const ERASE_SIZE: usize, const WRITE_SIZE: usize> ErrorType
            for MemFlash<SIZE, ERASE_SIZE, WRITE_SIZE>
        {
            type Error = MemFlashError;
        }

        impl NorFlashError for MemFlashError {
            fn kind(&self) -> NorFlashErrorKind {
                NorFlashErrorKind::Other
            }
        }

        impl<const SIZE: usize, const ERASE_SIZE: usize, const WRITE_SIZE: usize> ReadNorFlash
            for MemFlash<SIZE, ERASE_SIZE, WRITE_SIZE>
        {
            const READ_SIZE: usize = 1;

            fn read(&mut self, offset: u32, bytes: &mut [u8]) -> Result<(), Self::Error> {
                self.read(offset, bytes)
            }

            fn capacity(&self) -> usize {
                SIZE
            }
        }

        impl<const SIZE: usize, const ERASE_SIZE: usize, const WRITE_SIZE: usize> NorFlash
            for MemFlash<SIZE, ERASE_SIZE, WRITE_SIZE>
        {
            const WRITE_SIZE: usize = WRITE_SIZE;
            const ERASE_SIZE: usize = ERASE_SIZE;

            fn write(&mut self, offset: u32, bytes: &[u8]) -> Result<(), Self::Error> {
                self.write(offset, bytes)
            }

            fn erase(&mut self, from: u32, to: u32) -> Result<(), Self::Error> {
                self.erase(from, to)
            }
        }

        impl<const SIZE: usize, const ERASE_SIZE: usize, const WRITE_SIZE: usize> MultiwriteNorFlash
            for MemFlash<SIZE, ERASE_SIZE, WRITE_SIZE>
        {
        }
    }
}
