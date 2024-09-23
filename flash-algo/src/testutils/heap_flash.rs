//! A mocked SPI flash simulator intended for testing
//!
//! This module implements the `[SpiFlash]` trait, intended to be used for
//! host testing of the flash algorithm. It mimics the typical read, write, erase
//! behaviors of a typical SPI NOR flash part, while checking certain invariants, such
//! as multiple writes to the same byte without an erase (which could lead to corrupted
//! values).

extern crate std;
use crate::spi_flash::{SpiFlash, SpiFlashError};
use std::{format, prelude::rust_2021::*, vec};

/// It is typical that individual write commands cannot span more than a single page
/// at a time. Most commonly, flash parts have a page size of 256 bytes.
///
/// Writes spanning a page boundary will be split into separate writes.
const BYTES_PER_PAGE: usize = 256;

/// A heap allocated, SPI Flash simulator.
///
/// See the `[SpiFlash]` trait methods for usage.
///
/// A single Flash part is hierarchical, consisting of:
///
/// * [Flash]
///     * N x [Block]s per [Flash]
///         * M x [Page]s per [Block]
///
/// The flash part is divided up this way, as typically:
///
/// * Writes can only occur within a single Page
/// * Erases can only be performed on a block-at-a-time
pub struct Flash {
    blocks: Vec<Block>,
    block_size: usize,
    total_size: usize,
}

/// An erasable subset of [Flash].
pub struct Block {
    pages: Vec<Page>,
}

impl Block {
    fn erase(&mut self) {
        self.pages.iter_mut().for_each(|p| {
            *p = Page::new();
        });
    }
}

/// A writable subset of a [Block].
pub struct Page {
    data: [u8; BYTES_PER_PAGE],
    written: [bool; BYTES_PER_PAGE],
}

impl Page {
    /// Create a new page, with blank (0xFF) data, and with all bytes
    /// marked as unwritten.
    const fn new() -> Self {
        Page {
            data: [0xFF; BYTES_PER_PAGE],
            written: [false; BYTES_PER_PAGE],
        }
    }
}

impl Flash {
    /// Create a new simulated, blank flash part
    ///
    /// The flash part is divided up into blocks of `block_size`. `total_size`
    /// must be a multiple of `block_size`, and `block_size` must be a multiple
    /// of `BYTES_PER_PAGE`, which is 256.
    ///
    /// ```rust
    /// use flash_algo::testutils::heap_flash::Flash;
    ///
    /// // Create a 1MiB flash part with 64KiB blocks
    /// let flash = Flash::new(64 * 1024, 1024 * 1024);
    /// ```
    /// # Panics
    /// Panics if `total_size` is not evenly divisible by `block_size`
    /// Panics if `block_size` is not evenly divisible by `BYTES_PER_PAGE`
    #[must_use]
    pub fn new(block_size: usize, total_size: usize) -> Self {
        assert_eq!(total_size % block_size, 0);
        assert_eq!(block_size % BYTES_PER_PAGE, 0);
        let num_blocks = total_size / block_size;
        let pages_per_block = block_size / BYTES_PER_PAGE;
        let mut blocks = vec![];
        for _ in 0..num_blocks {
            let mut pages = vec![];
            for _ in 0..pages_per_block {
                pages.push(Page::new());
            }
            blocks.push(Block { pages });
        }
        Flash {
            blocks,
            block_size,
            total_size,
        }
    }

    /// Erase the entire flash part
    fn whole_erase(&mut self) {
        self.blocks.iter_mut().for_each(Block::erase);
    }

    /// Writes entire flash to string, hex-dump style.
    ///
    /// * WRITTEN bytes are in upper-hex, e.g. "0A" or "FF"
    /// * UNWRITTEN (erased) bytes are in lower-hex, always "ff".
    /// * Lines that are all UNWRITTEN are elided (with `...`).
    ///
    /// This is intended for use with snapshot tests.
    ///
    /// ```rust
    /// use flash_algo::testutils::heap_flash::Flash;
    /// use flash_algo::spi_flash::SpiFlash;
    ///
    /// # tokio::runtime::Runtime::new().unwrap().block_on(async {
    /// // Create a 256B flash part with 256B blocks
    /// let mut flash = Flash::new(256, 256);
    /// flash.write_from(0x10, &[0x01, 0x02, 0x03, 0xFF]).await.unwrap();
    /// flash.write_from(0x30, &[0x05, 0x06, 0x07, 0xFF]).await.unwrap();
    ///
    /// let expected = "\
    /// ...
    /// 00000010 | 01 02 03 ff ff ff ff ff ff ff ff ff ff ff ff ff
    /// ...
    /// 00000030 | 05 06 07 ff ff ff ff ff ff ff ff ff ff ff ff ff
    ///";
    /// let dump = flash.dump_to_string();
    ///
    /// // paper over some slight whitespace difference
    /// for (el, dl) in expected.lines().zip(dump.lines()) {
    ///     assert_eq!(el.trim(), dl.trim());
    /// }
    /// # });
    /// ```
    #[must_use]
    pub fn dump_to_string(&self) -> String {
        let mut address = 0_u32;
        let mut out = String::new();

        let mut gap = false;

        for block in &self.blocks {
            for page in &block.pages {
                for (ch, wch) in page.data.chunks(16).zip(page.written.chunks(16)) {
                    let all_ffs = ch.iter().all(|b| *b == 0xFF);
                    let all_unw = wch.iter().all(|w| !w);

                    #[allow(clippy::format_push_string)] // TODO: eliminate this allow
                    // Don't write blank regions
                    if all_ffs && all_unw {
                        gap = true;
                    } else {
                        if gap {
                            out += "...\r\n";
                            gap = false;
                        }
                        out += &format!("{address:08X} | ");
                        for (b, w) in ch.iter().zip(wch.iter()) {
                            if *w {
                                out += &format!("{b:02X} ");
                            } else {
                                out += "ff ";
                            }
                        }
                        out += "\n";
                    }

                    address += 16;
                }
            }
        }
        out
    }
}

/// The associated error type
///
/// This needs to exist to fulfill the `[SpiFlash]` trait's associated type,
/// however it is empty as `[Flash]` will always panic if an invariant is
/// violated for testing purposes.
#[derive(Debug, PartialEq)]
pub enum Error {}

impl SpiFlash for Flash {
    type Error = Error;

    fn total_size(&self) -> usize {
        self.total_size
    }

    fn block_size(&self) -> usize {
        self.block_size
    }

    async fn erase_block(&mut self, start_addr: usize) -> Result<(), SpiFlashError<Error>> {
        // Erases must be block-aligned
        if (start_addr % self.block_size) != 0 {
            return Err(SpiFlashError::UnalignedAccess);
        }
        // Check out-of-bounds access
        if start_addr >= self.total_size {
            #[cfg(feature = "defmt")]
            defmt::info!(
                "erase_block: start_addr {=usize} >= total_size {=usize}",
                start_addr,
                self.total_size
            );

            return Err(SpiFlashError::OutOfBounds);
        }
        // Erase the given block
        let idx = start_addr / self.block_size;

        #[allow(clippy::indexing_slicing)] // TODO: Evaluate this slicing
        self.blocks[idx].erase();
        Ok(())
    }

    async fn erase_all(&mut self) -> Result<(), SpiFlashError<Error>> {
        self.whole_erase();
        Ok(())
    }

    async fn read_to(
        &mut self,
        start_addr: usize,
        mut buf: &mut [u8],
    ) -> Result<(), SpiFlashError<Error>> {
        // Can't read out of bounds
        if start_addr >= self.total_size {
            #[cfg(feature = "defmt")]
            defmt::info!(
                "read_to: start_addr {=usize} >= total_size {=usize}",
                start_addr,
                self.total_size
            );

            return Err(SpiFlashError::OutOfBounds);
        }
        if (start_addr + buf.len()) > self.total_size {
            #[cfg(feature = "defmt")]
            defmt::info!(
                "start_addr {=usize} + buf.len {=usize} > total_size {=usize}",
                start_addr,
                buf.len(),
                self.total_size
            );

            return Err(SpiFlashError::OutOfBounds);
        }

        // On normal flash parts, you can do reads that span more than one page.
        // We can't, because we structured things into explicit pages.
        //
        // Calculate the starting block, and any potential page offset in that initial
        // block.
        let starting_block = start_addr / self.block_size;
        let mut block_offset = start_addr % self.block_size;

        // For each BLOCK...
        'copy: for block in self.blocks.iter().skip(starting_block) {
            let (starting_page, mut page_offset) = if block_offset == 0 {
                // No block offset? Start at the first page with no page offset
                (0, 0)
            } else {
                // Some block offset? Then we need to figure out how many pages
                // to skip, and how many bytes in that first page to skip.
                let starting_page = block_offset / BYTES_PER_PAGE;
                let page_offset = block_offset % BYTES_PER_PAGE;

                // Once we've done the first unaligned read, all subsequent reads
                // will be aligned.
                block_offset = 0;
                (starting_page, page_offset)
            };

            // ...then for each PAGE in THIS BLOCK...
            for page in block.pages.iter().skip(starting_page) {
                // Do a potentially unaligned read of the given page
                let basic_amount = BYTES_PER_PAGE - page_offset;
                let amount = basic_amount.min(buf.len());

                let (now, later) = buf.split_at_mut(amount);
                buf = later;
                #[allow(clippy::indexing_slicing)] // TODO: Evaluate this slicing
                now.copy_from_slice(&page.data[page_offset..][..amount]);

                // Similar to the block level, once we've aligned, we're good
                page_offset = 0;

                // if we've copied as many bytes as exist in the destination buffer,
                // we're all done!
                if buf.is_empty() {
                    break 'copy;
                }
            }
        }
        Ok(())
    }

    async fn write_from(
        &mut self,
        start_addr: usize,
        mut buf: &[u8],
    ) -> Result<(), SpiFlashError<Error>> {
        if start_addr >= self.total_size {
            #[cfg(feature = "defmt")]
            defmt::info!(
                "write_from start_addr {=usize} >= total_size {=usize}",
                start_addr,
                self.total_size
            );

            return Err(SpiFlashError::OutOfBounds);
        }
        if (start_addr + buf.len()) > self.total_size {
            #[cfg(feature = "defmt")]
            defmt::info!(
                "start_addr {=usize} + buf.len {=usize} > total_size {=usize}",
                start_addr,
                buf.len(),
                self.total_size
            );

            return Err(SpiFlashError::OutOfBounds);
        }

        // See Self::read_to for a discussion of how this alignment code works.
        // We are doing the exact same procedure for WRITE as we do for READ.
        let starting_block = start_addr / self.block_size;
        let mut block_offset = start_addr % self.block_size;
        'copy: for block in self.blocks.iter_mut().skip(starting_block) {
            let (starting_page, mut page_offset) = if block_offset == 0 {
                (0, 0)
            } else {
                let starting_page = block_offset / BYTES_PER_PAGE;
                let page_offset = block_offset % BYTES_PER_PAGE;
                block_offset = 0;
                (starting_page, page_offset)
            };

            for page in block.pages.iter_mut().skip(starting_page) {
                let initial_amount = BYTES_PER_PAGE - page_offset;
                let amount = initial_amount.min(buf.len());

                let (now, later) = buf.split_at(amount);
                buf = later;

                #[allow(clippy::indexing_slicing)] // TODO: Evaluate this slicing
                let erases = &mut page.written[page_offset..][..amount];

                // UNLIKE read operations, we can only WRITE each given byte once.
                // Technically this is MORE strict, as you probably could write
                // multiple times to the same byte, however the output would act as
                // a masking, so writing 0x0F then 0xF0 would result in 0x00, as
                // bits can only be cleared. We take a slightly stricter interpretation,
                // however we do "ignore" writes of 0xFF.
                //
                // If two writes occur to the same byte, panic.
                for (wr, dat) in erases.iter_mut().zip(now) {
                    if *dat != 0xFF {
                        assert!(!*wr);
                        *wr = true;
                    }
                }

                #[allow(clippy::indexing_slicing)] // TODO: Evaluate this slicing
                page.data[page_offset..][..amount].copy_from_slice(now);

                page_offset = 0;

                if buf.is_empty() {
                    break 'copy;
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use insta::assert_snapshot;

    use super::*;

    #[allow(clippy::unwrap_used)]
    #[tokio::test]
    async fn smoke() {
        // 64KiB pages, 1MiB flash
        let mut f = Flash::new(64 * 1024, 1024 * 1024);
        f.erase_all().await.unwrap();

        {
            let mut buf = [0_u8; 4096];
            f.read_to(0, &mut buf).await.unwrap();
            for b in &buf {
                assert_eq!(0xFF, *b);
            }
        }
        {
            let mut buf = [0_u8; 4096];
            f.read_to(123_456, &mut buf).await.unwrap();
            for b in &buf {
                assert_eq!(0xFF, *b);
            }
        }
        {
            let mut buf = [0_u8; 4096];
            #[allow(clippy::cast_possible_truncation)]
            buf.iter_mut().enumerate().for_each(|(i, b)| {
                *b = i as u8;
            });
            f.write_from(123_456, &buf).await.unwrap();
        };
        let d = f.dump_to_string();
        assert_snapshot!(d);
    }

    #[allow(clippy::unwrap_used)]
    #[tokio::test]
    #[should_panic(expected = "assertion failed: !*wr")]
    async fn write_twice() {
        let mut f = Flash::new(64 * 1024, 1024 * 1024);
        let mut buf = [0_u8; 4096];
        #[allow(clippy::cast_possible_truncation)]
        buf.iter_mut().enumerate().for_each(|(i, b)| {
            *b = i as u8;
        });
        f.write_from(123_456, &buf).await.unwrap();

        // now write to the same range
        f.write_from(123_456, &[0]).await.unwrap();
    }
}
