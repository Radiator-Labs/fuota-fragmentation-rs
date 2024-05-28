use core::future::Future;

/// Errors returned by [`SpiFlash`] implementations
#[allow(clippy::exhaustive_enums)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[derive(Debug, PartialEq)]
pub enum SpiFlashError<C> {
    /// An unaligned access was attempted
    UnalignedAccess,
    /// Access outside the bounds of the flash chip were attempted
    OutOfBounds,
    /// An unspecified, fatal, hardware-level error occurred
    HardwareFailure,
    /// Error types specific to the implementor
    Custom(C),
    /// An internal logic error
    LogicError,
}

impl<C> From<C> for SpiFlashError<C> {
    fn from(value: C) -> Self {
        SpiFlashError::Custom(value)
    }
}

/// SPI flash abstraction trait
///
/// This models a standard SPI NOR flash part
pub trait SpiFlash {
    type Error: core::fmt::Debug + PartialEq;

    /// The total size of the SPI flash device
    fn total_size(&self) -> usize;
    /// The smallest possible erase size of the SPI flash device
    fn block_size(&self) -> usize;

    /// Erase a single block, starting at `start_addr`.
    fn erase_block(
        &mut self,
        start_addr: usize,
    ) -> impl Future<Output = Result<(), SpiFlashError<Self::Error>>>;
    /// Erase the ENTIRE flash
    fn erase_all(&mut self) -> impl Future<Output = Result<(), SpiFlashError<Self::Error>>>;

    /// Read FROM the flash TO the provided buffer.
    ///
    /// Starts at `start_addr`, and `buf.len()` bytes are copied.
    fn read_to(
        &mut self,
        start_addr: usize,
        buf: &mut [u8],
    ) -> impl Future<Output = Result<(), SpiFlashError<Self::Error>>>;

    /// Write TO the flash FROM the provided buffer
    ///
    /// The entire range `[start_addr..][..buf.len()]` MUST have been erased.
    fn write_from(
        &mut self,
        start_addr: usize,
        buf: &[u8],
    ) -> impl Future<Output = Result<(), SpiFlashError<Self::Error>>>;
}
