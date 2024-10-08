//! Flash implementation based on [embedded-storage] for the varying traits of the crate
//!

use core::{cell::RefCell, ops::Range};

use bitvec::array::BitArray;

use crate::{DataStorage, MatrixStorage, ParityStorage};

const MAX_WORD_SIZE: usize = 32;

/// A simple shared flash impl that is made for use within a single thread (and no parallel async usage).
/// It's completely optional to use and only here for convenience.
///
/// It implements the [embedded_storage_async] traits and uses a refcell internally.
#[derive(Debug)]
pub struct SharedFlash<'a, F> {
    flash: RefCell<&'a mut F>,
}

impl<'a, F> SharedFlash<'a, F> {
    /// Create a new instance of the shared flash
    pub fn new(flash: RefCell<&'a mut F>) -> Self {
        Self { flash }
    }
}

impl<'a, F: embedded_storage_async::nor_flash::ErrorType>
    embedded_storage_async::nor_flash::ErrorType for &SharedFlash<'a, F>
{
    type Error = F::Error;
}

impl<'a, F: embedded_storage_async::nor_flash::ReadNorFlash>
    embedded_storage_async::nor_flash::ReadNorFlash for &SharedFlash<'a, F>
{
    const READ_SIZE: usize = F::READ_SIZE;

    #[allow(clippy::await_holding_refcell_ref)]
    async fn read(&mut self, offset: u32, bytes: &mut [u8]) -> Result<(), Self::Error> {
        self.flash.borrow_mut().read(offset, bytes).await
    }

    fn capacity(&self) -> usize {
        self.flash.borrow_mut().capacity()
    }
}

impl<'a, F: embedded_storage_async::nor_flash::NorFlash> embedded_storage_async::nor_flash::NorFlash
    for &SharedFlash<'a, F>
{
    const WRITE_SIZE: usize = F::WRITE_SIZE;

    const ERASE_SIZE: usize = F::ERASE_SIZE;

    #[allow(clippy::await_holding_refcell_ref)]
    async fn erase(&mut self, from: u32, to: u32) -> Result<(), Self::Error> {
        self.flash.borrow_mut().erase(from, to).await
    }

    #[allow(clippy::await_holding_refcell_ref)]
    async fn write(&mut self, offset: u32, bytes: &[u8]) -> Result<(), Self::Error> {
        self.flash.borrow_mut().write(offset, bytes).await
    }
}

impl<'a, F: embedded_storage_async::nor_flash::MultiwriteNorFlash>
    embedded_storage_async::nor_flash::MultiwriteNorFlash for &SharedFlash<'a, F>
{
}

/// An implementation for the [MatrixStorage] trait.
/// It stores the matrix data in flash relatively efficiently.
pub struct FlashMatrixStorage<F>
where
    F: embedded_storage_async::nor_flash::NorFlash,
{
    flash: F,
    flash_range: Range<u32>,
}

impl<F> FlashMatrixStorage<F>
where
    F: embedded_storage_async::nor_flash::NorFlash,
{
    /// Create a new instance
    pub async fn new(mut flash: F, flash_range: Range<u32>) -> Result<Self, F::Error> {
        assert!(F::WRITE_SIZE <= MAX_WORD_SIZE);
        assert!((F::WRITE_SIZE / F::READ_SIZE).is_power_of_two());

        flash.erase(flash_range.start, flash_range.end).await?;

        Ok(Self { flash, flash_range })
    }

    /// Returns the size of the row in bytes as it can be stored in flash (including padding)
    const fn flash_row_size(m: usize) -> usize {
        let byte_size = (m + 1).next_multiple_of(8) / 8;
        byte_size.next_multiple_of(F::WRITE_SIZE)
    }

    fn flash_row_address_offset(m: usize) -> u32 {
        let completes = m / (F::WRITE_SIZE * 8);
        let partials = m % (F::WRITE_SIZE * 8);
        let partial_len = Self::flash_row_size(m);

        // Number of complete groups of F::WRITE_SIZE * 8 and the storage
        // they take, plus the size of the incomplete group.
        //
        // The size of the complete groups follows from the triangle numbers, which
        // give the number of chunks * 2 (assuming 1 member per group), which are F::WRITE_SIZE big,
        // multiplied by half the group size (4 * F::WRITE)
        (completes * (completes + 1) * 4 * F::WRITE_SIZE * F::WRITE_SIZE + partials * partial_len)
            as u32
    }
}

impl<F, const N: usize> MatrixStorage<[u8; N]> for FlashMatrixStorage<F>
where
    F: embedded_storage_async::nor_flash::NorFlash,
{
    type Error = F::Error;

    async fn set_row(&mut self, m: usize, data: BitArray<[u8; N]>) -> Result<(), Self::Error> {
        let address = self.flash_range.start + Self::flash_row_address_offset(m);
        let data_len = Self::flash_row_size(m);

        let (data_body, data_padded) = if data_len > N {
            data.as_raw_slice().split_at(data_len - F::WRITE_SIZE)
        } else {
            (&data.as_raw_slice()[..data_len], &[][..])
        };

        self.flash.write(address, data_body).await?;

        if !data_padded.is_empty() {
            let mut buffer = [0; MAX_WORD_SIZE];
            buffer[..data_padded.len()].copy_from_slice(data_padded);
            self.flash
                .write(address + data_body.len() as u32, &buffer[..F::WRITE_SIZE])
                .await?;
        }

        Ok(())
    }

    async fn row(&mut self, m: usize) -> Result<BitArray<[u8; N]>, Self::Error> {
        let address = self.flash_range.start + Self::flash_row_address_offset(m);
        let data_len = Self::flash_row_size(m);

        let mut data = BitArray::ZERO;

        let (data_body, data_padded) = if data_len > N {
            data.as_raw_mut_slice()
                .split_at_mut(data_len - F::WRITE_SIZE)
        } else {
            (&mut data.as_raw_mut_slice()[..data_len], &mut [][..])
        };

        self.flash.read(address, data_body).await?;

        if !data_padded.is_empty() {
            let mut buffer = [0; MAX_WORD_SIZE];
            self.flash
                .read(
                    address + data_body.len() as u32,
                    &mut buffer[..F::WRITE_SIZE],
                )
                .await?;
            data_padded.copy_from_slice(&buffer[..data_padded.len()]);
        }

        Ok(data)
    }

    fn num_rows(&self) -> usize {
        let storage_size = self.flash_range.len();
        let mut matrix_size = 0usize;
        let mut num_rows = 0usize;
        loop {
            matrix_size = matrix_size.saturating_add(Self::flash_row_size(num_rows));
            if matrix_size < storage_size && num_rows < BitArray::<[u8; N]>::ZERO.len() {
                num_rows += 1;
            } else {
                return num_rows;
            }
        }
    }
}

/// Implementation of the [ParityStorage] trait.
/// It stores the parity data in flash.
pub struct FlashParityStorage<F>
where
    F: embedded_storage_async::nor_flash::NorFlash,
{
    flash: F,
    flash_range: Range<u32>,
}

impl<F> FlashParityStorage<F>
where
    F: embedded_storage_async::nor_flash::NorFlash,
{
    /// Create a new instance
    pub async fn new(mut flash: F, flash_range: Range<u32>) -> Result<Self, F::Error> {
        assert!(F::WRITE_SIZE <= MAX_WORD_SIZE);
        assert!((F::WRITE_SIZE / F::READ_SIZE).is_power_of_two());

        flash.erase(flash_range.start, flash_range.end).await?;

        Ok(Self { flash, flash_range })
    }
}

/// Implemented with a simple algorithm.
/// Between every parity block there is a little bit of potential padding.
impl<F> ParityStorage for FlashParityStorage<F>
where
    F: embedded_storage_async::nor_flash::NorFlash,
{
    type Error = F::Error;

    async fn store(&mut self, m: usize, data: &[u8]) -> Result<(), Self::Error> {
        let rounded_up_len = data.len().next_multiple_of(F::WRITE_SIZE);
        let rounded_down_len = previous_multiple_of(data.len(), F::WRITE_SIZE);
        let offset = m * rounded_up_len;

        // Split so that data0 can be stored directly. Data1 needs to be padded if not empty
        let (data0, data1) = data.split_at(rounded_down_len);

        self.flash
            .write(self.flash_range.start + offset as u32, data0)
            .await?;

        if !data1.is_empty() {
            let mut buffer = [0; MAX_WORD_SIZE];

            buffer[..data1.len()].copy_from_slice(data1);
            self.flash
                .write(
                    self.flash_range.start + offset as u32 + data0.len() as u32,
                    &buffer[..F::WRITE_SIZE],
                )
                .await?;
        }

        Ok(())
    }

    async fn get(&mut self, m: usize, data: &mut [u8]) -> Result<(), Self::Error> {
        let rounded_up_len = data.len().next_multiple_of(F::WRITE_SIZE);
        let rounded_down_len = previous_multiple_of(data.len(), F::WRITE_SIZE);
        let offset = m * rounded_up_len;

        // Split so that data0 can be read directly. Data1 needs to be padding if not empty
        let (data0, data1) = data.split_at_mut(rounded_down_len);

        self.flash
            .read(self.flash_range.start + offset as u32, data0)
            .await?;

        if !data1.is_empty() {
            let mut buffer = [0; MAX_WORD_SIZE];

            self.flash
                .read(
                    self.flash_range.start + offset as u32 + data0.len() as u32,
                    &mut buffer[..F::READ_SIZE],
                )
                .await?;

            data1.copy_from_slice(&buffer[..data1.len()]);
        }

        Ok(())
    }
}

/// Implementation of the [DataStorage] trait.
/// It stores the data in flash without any internal padding.
/// This helps bootloaders so they can just copy the data over without further processing.
pub struct FlashDataStorage<F>
where
    F: embedded_storage_async::nor_flash::MultiwriteNorFlash,
{
    flash: F,
    flash_range: Range<u32>,
}

impl<F> FlashDataStorage<F>
where
    F: embedded_storage_async::nor_flash::MultiwriteNorFlash,
{
    /// Create a new instance
    pub async fn new(mut flash: F, flash_range: Range<u32>) -> Result<Self, F::Error> {
        assert!(F::WRITE_SIZE <= MAX_WORD_SIZE);
        assert!((F::WRITE_SIZE / F::READ_SIZE).is_power_of_two());

        flash.erase(flash_range.start, flash_range.end).await?;

        Ok(Self { flash, flash_range })
    }

    fn split_slice_addrs_mut<'data>(
        &self,
        m: usize,
        data: &'data mut [u8],
    ) -> (
        (&'data mut [u8], u32),
        (&'data mut [u8], u32),
        (&'data mut [u8], u32),
    ) {
        let true_start_address = self.flash_range.start + (m * data.len()) as u32;
        let start_address_align_offset = true_start_address % F::WRITE_SIZE as u32;
        let true_end_address = self.flash_range.start + ((m + 1) * data.len()) as u32;
        let end_address_align_offset = true_end_address % F::WRITE_SIZE as u32;

        let padded_start_address = if start_address_align_offset != 0 {
            true_start_address - start_address_align_offset
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

    fn split_slice_addrs<'data>(
        &self,
        m: usize,
        data: &'data [u8],
    ) -> ((&'data [u8], u32), (&'data [u8], u32), (&'data [u8], u32)) {
        let true_start_address = self.flash_range.start + (m * data.len()) as u32;
        let start_address_align_offset = true_start_address % F::WRITE_SIZE as u32;
        let true_end_address = self.flash_range.start + ((m + 1) * data.len()) as u32;
        let end_address_align_offset = true_end_address % F::WRITE_SIZE as u32;

        let padded_start_address = if start_address_align_offset != 0 {
            true_start_address - start_address_align_offset
        } else {
            true_start_address
        };

        let body_start_address = if start_address_align_offset != 0 {
            padded_start_address + F::WRITE_SIZE as u32
        } else {
            true_start_address
        };

        let (pad_start, body) = if start_address_align_offset != 0 {
            data.split_at(F::WRITE_SIZE - (true_start_address - padded_start_address) as usize)
        } else {
            (&[][..], data)
        };

        let (body, pad_end) = if end_address_align_offset != 0 {
            body.split_at(body.len() - end_address_align_offset as usize)
        } else {
            (body, &[][..])
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
impl<F> DataStorage for FlashDataStorage<F>
where
    F: embedded_storage_async::nor_flash::MultiwriteNorFlash,
{
    type Error = F::Error;

    async fn store(&mut self, m: usize, data: &[u8]) -> Result<(), Self::Error> {
        assert!(data.len() >= F::WRITE_SIZE);

        let ((pad_start, start_addr), (body, body_addr), (pad_end, end_addr)) =
            self.split_slice_addrs(m, data);

        if !pad_start.is_empty() {
            let mut buffer = [0xFF; MAX_WORD_SIZE];
            buffer[MAX_WORD_SIZE - pad_start.len()..].copy_from_slice(pad_start);

            self.flash
                .write(start_addr, &buffer[MAX_WORD_SIZE - F::WRITE_SIZE..])
                .await?;
        }

        self.flash.write(body_addr, body).await?;

        if !pad_end.is_empty() {
            let mut buffer = [0xFF; MAX_WORD_SIZE];
            buffer[..pad_end.len()].copy_from_slice(pad_end);

            self.flash.write(end_addr, &buffer[..F::WRITE_SIZE]).await?;
        }

        Ok(())
    }

    async fn get(&mut self, m: usize, data: &mut [u8]) -> Result<(), Self::Error> {
        let ((pad_start, start_addr), (body, body_addr), (pad_end, end_addr)) =
            self.split_slice_addrs_mut(m, data);

        if !pad_start.is_empty() {
            let mut buffer = [0x00; MAX_WORD_SIZE];
            self.flash
                .read(start_addr, &mut buffer[MAX_WORD_SIZE - F::WRITE_SIZE..])
                .await?;

            pad_start.copy_from_slice(&buffer[MAX_WORD_SIZE - pad_start.len()..]);
        }

        self.flash.read(body_addr, body).await?;

        if !pad_end.is_empty() {
            let mut buffer = [0x00; MAX_WORD_SIZE];
            self.flash
                .read(end_addr, &mut buffer[..F::WRITE_SIZE])
                .await?;

            pad_end.copy_from_slice(&buffer[..pad_end.len()]);
        }

        Ok(())
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
    extern crate std;

    use mem_flash::MemFlash;

    use crate::{tests::TestParity, BlockResult, Reconstructor, ReconstructorData};

    use super::*;

    #[futures_test::test]
    async fn simple_reconstruction_test() {
        let mut recdata = ReconstructorData::new(4, 1);
        let mut rec: Reconstructor<'_, _, _, _, _, 1, [u8; 1], [u8; 1]> = recdata.hydrate(
            TestParity::<4>,
            FlashMatrixStorage::new(mem_flash::MemFlash::<1024, 128, 1>::new(0), 0..0x400)
                .await
                .unwrap(),
            FlashParityStorage::new(mem_flash::MemFlash::<1024, 128, 1>::new(0), 0..0x400)
                .await
                .unwrap(),
            FlashDataStorage::new(mem_flash::MemFlash::<1024, 128, 1>::new(0), 0..0x400)
                .await
                .unwrap(),
        );
        assert_eq!(
            rec.handle_block(0, &[1]).await.unwrap(),
            BlockResult::NeedMore
        );
        assert_eq!(
            rec.handle_block(2, &[3]).await.unwrap(),
            BlockResult::NeedMore
        );
        assert_eq!(
            rec.handle_block(9, &[2]).await.unwrap(),
            BlockResult::NeedMore
        );
        assert_eq!(
            rec.handle_block(10, &[1]).await.unwrap(),
            BlockResult::NeedMore
        );
        assert_eq!(
            rec.handle_block(14, &[6]).await.unwrap(),
            BlockResult::Done(4)
        );
    }

    #[futures_test::test]
    async fn data_storage_valid() {
        // 8-byte flash words with 9-byte block sizes
        let mut ds =
            FlashDataStorage::new(mem_flash::MemFlash::<1024, 128, 8>::new(0), 0x100..0x400)
                .await
                .unwrap();

        ds.store(0, &[1, 2, 3, 4, 5, 6, 7, 8, 9]).await.unwrap();
        ds.store(2, &[21, 22, 23, 24, 25, 26, 27, 28, 29])
            .await
            .unwrap();
        ds.store(1, &[11, 12, 13, 14, 15, 16, 17, 18, 19])
            .await
            .unwrap();

        assert_eq!(&ds.flash.mem[..0x100], &[0; 0x100]);
        assert_eq!(
            &ds.flash.mem[0x100..][..9 * 3],
            &[
                1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 24, 25,
                26, 27, 28, 29
            ]
        );
        assert_eq!(&ds.flash.mem[0x100..][9 * 3..], &[0xFF; 0x300 - 9 * 3]);

        let mut buf = [0u8; 9];
        assert!(ds.get(0, &mut buf).await.is_ok());
        assert_eq!(buf, [1, 2, 3, 4, 5, 6, 7, 8, 9]);
        assert!(ds.get(1, &mut buf).await.is_ok());
        assert_eq!(buf, [11, 12, 13, 14, 15, 16, 17, 18, 19]);
        assert!(ds.get(2, &mut buf).await.is_ok());
        assert_eq!(buf, [21, 22, 23, 24, 25, 26, 27, 28, 29]);
    }

    #[futures_test::test]
    async fn matrix_storage_valid() {
        // 8-byte flash words with 9-byte block sizes
        let mut ds =
            FlashMatrixStorage::new(mem_flash::MemFlash::<1024, 128, 8>::new(0), 0x100..0x400)
                .await
                .unwrap();

        ds.set_row(0, BitArray::new([1, 0, 0, 0, 0, 0, 0, 0, 0]))
            .await
            .unwrap();
        ds.set_row(2, BitArray::new([4, 0, 0, 0, 0, 0, 0, 0, 0]))
            .await
            .unwrap();
        ds.set_row(1, BitArray::new([2, 0, 0, 0, 0, 0, 0, 0, 0]))
            .await
            .unwrap();
        ds.set_row(20, BitArray::new([0xAA, 0xAA, 0x0A, 0, 0, 0, 0, 0, 0]))
            .await
            .unwrap();
        ds.set_row(
            65,
            BitArray::new([0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0x01]),
        )
        .await
        .unwrap();

        assert_eq!(&ds.flash.mem[..0x100], &[0; 0x100]);
        assert_eq!(
            &ds.flash.mem[0x100..][..3 * 8],
            &[1, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0,]
        );
        assert_eq!(
            &ds.flash.mem[0x100..][20 * 8..][..8],
            [0xAA, 0xAA, 0x0A, 0, 0, 0, 0, 0]
        );
        assert_eq!(
            &ds.flash.mem[0x100..][66 * 8..][..16],
            [0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0x01, 0, 0, 0, 0, 0, 0, 0]
        );

        assert_eq!(
            ds.row(0).await,
            Ok(BitArray::new([1, 0, 0, 0, 0, 0, 0, 0, 0]))
        );
        assert_eq!(
            ds.row(1).await,
            Ok(BitArray::new([2, 0, 0, 0, 0, 0, 0, 0, 0]))
        );
        assert_eq!(
            ds.row(2).await,
            Ok(BitArray::new([4, 0, 0, 0, 0, 0, 0, 0, 0]))
        );
        assert_eq!(
            ds.row(20).await,
            Ok(BitArray::new([0xAA, 0xAA, 0x0A, 0, 0, 0, 0, 0, 0]))
        );
        assert_eq!(
            ds.row(65).await,
            Ok(BitArray::new([
                0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0x01
            ]))
        );
    }

    #[test]
    fn test_matrix_row_addresses() {
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(0),
            0
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(1),
            1
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(2),
            2
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(3),
            3
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(4),
            4
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(5),
            5
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(6),
            6
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(7),
            7
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(8),
            8
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(9),
            10
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(10),
            12
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(11),
            14
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(12),
            16
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(13),
            18
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(14),
            20
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(15),
            22
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(16),
            24
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 1>>::flash_row_address_offset(17),
            27
        );

        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(0),
            0
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(1),
            2
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(2),
            4
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(3),
            6
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(4),
            8
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(5),
            10
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(6),
            12
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(7),
            14
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(8),
            16
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(9),
            18
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(10),
            20
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(11),
            22
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(12),
            24
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(13),
            26
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(14),
            28
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(15),
            30
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(16),
            32
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(17),
            36
        );
        assert_eq!(
            FlashMatrixStorage::<MemFlash<1024, 256, 2>>::flash_row_address_offset(33),
            102
        );
    }

    #[futures_test::test]
    async fn parity_storage_valid() {
        // 8-byte flash words with 9-byte block sizes
        let mut ds =
            FlashParityStorage::new(mem_flash::MemFlash::<1024, 128, 8>::new(0), 0x100..0x400)
                .await
                .unwrap();

        ds.store(0, &[1, 2, 3, 4, 5, 6, 7, 8, 9]).await.unwrap();
        ds.store(2, &[21, 22, 23, 24, 25, 26, 27, 28, 29])
            .await
            .unwrap();
        ds.store(1, &[11, 12, 13, 14, 15, 16, 17, 18, 19])
            .await
            .unwrap();

        let mut buf = [0u8; 9];
        assert!(ds.get(0, &mut buf).await.is_ok());
        assert_eq!(buf, [1, 2, 3, 4, 5, 6, 7, 8, 9]);
        assert!(ds.get(1, &mut buf).await.is_ok());
        assert_eq!(buf, [11, 12, 13, 14, 15, 16, 17, 18, 19]);
        assert!(ds.get(2, &mut buf).await.is_ok());
        assert_eq!(buf, [21, 22, 23, 24, 25, 26, 27, 28, 29]);
    }

    // Taken and adapted from: https://github.com/lulf/embedded-storage-inmemory
    mod mem_flash {
        use embedded_storage_async::nor_flash::{
            ErrorType, MultiwriteNorFlash, NorFlash, NorFlashError, NorFlashErrorKind, ReadNorFlash,
        };

        pub struct MemFlash<const SIZE: usize, const ERASE_SIZE: usize, const WRITE_SIZE: usize> {
            pub mem: [u8; SIZE],
            pub written: [u8; SIZE],
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct MemFlashError;

        impl<const SIZE: usize, const ERASE_SIZE: usize, const WRITE_SIZE: usize>
            MemFlash<SIZE, ERASE_SIZE, WRITE_SIZE>
        {
            pub const fn new(fill: u8) -> Self {
                Self {
                    mem: [fill; SIZE],
                    written: [0; SIZE],
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
                assert!(
                    offset % WRITE_SIZE == 0,
                    "offset: {offset}, WRITE_SIZE: {WRITE_SIZE}"
                );
                assert!(offset + bytes.len() <= SIZE);

                for written in self.written.iter_mut().skip(offset).take(bytes.len()) {
                    if *written > 2 {
                        panic!("Written thrice");
                    }

                    *written += 1;
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
                    self.written[i] = 0;
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

            async fn read(&mut self, offset: u32, bytes: &mut [u8]) -> Result<(), Self::Error> {
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

            async fn write(&mut self, offset: u32, bytes: &[u8]) -> Result<(), Self::Error> {
                self.write(offset, bytes)
            }

            async fn erase(&mut self, from: u32, to: u32) -> Result<(), Self::Error> {
                self.erase(from, to)
            }
        }

        impl<const SIZE: usize, const ERASE_SIZE: usize, const WRITE_SIZE: usize> MultiwriteNorFlash
            for MemFlash<SIZE, ERASE_SIZE, WRITE_SIZE>
        {
        }
    }
}
