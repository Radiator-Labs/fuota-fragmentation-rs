#![no_std]

pub(crate) mod bitcache;
pub mod fragmentation;
pub mod manager;
pub mod spi_flash;
pub mod update;

#[cfg(test)]
mod tests;
#[cfg(any(test, feature = "testutils"))]
pub mod testutils;

pub mod layout {
    pub use crate::manager::layout::*;
}
