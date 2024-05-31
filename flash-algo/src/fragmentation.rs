//! Fragmentation calculation tools
//!
//! This module contains tools that are used for calculation of Forward Error Correction
//! fragmentation according to the lorawan spec.

use crate::bitcache::BitCache;
use core::{assert, cfg};

/// This function is a pseudo-random number generator, that is
/// permuted every time the function is called.
///
/// ```rust
/// use flash_algo::fragmentation::fragmentation_prbs23;
///
/// // Start with an initial value, generate four numbers
/// let mut x = 1001;
/// let mut buf = [0u32; 4];
/// buf.iter_mut().for_each(|w| {
///    x = fragmentation_prbs23(x);
///    *w = x;
/// });
///
/// assert_eq!(
///     &buf,
///     &[0x0000_01F4, 0x0040_00FA, 0x0060_007D, 0x0030_003E],
/// );
/// ```
#[must_use]
pub fn fragmentation_prbs23(x: u32) -> u32 {
    let b0 = x & 1;
    let b1 = (x & 0x20) >> 5_u32;
    (x / 2) + ((b0 ^ b1) << 22_u32)
}

/// Get the 1-indexed parity matrix row
///
/// The returned matrix is a list of which "data segments" this "parity segment"
/// is applicable to. To avoid having to store or transmit "which parity segments
/// apply to which data segments", we use `[fragmentation_prbs23()]` to generate
/// the applicability vector.
///
/// ## Arguments:
///
/// * `cap_n`, or "Capital N": The 1-indexed parity row
/// * `cap_m`, or "Capital M": The number of data segments
/// * `buf`: A mutable slice of bools. `buf.len()` MUST equal `cap_m`.
///
/// Typically, we send M data segments, plus P parity segments. For example,
/// if we have a fragmented transfer that consists of M=16 data segments, and
/// P=4 parity segments, we would call this function with `cap_n == 1..=4`.
///
/// ## Example
///
/// Get the 3rd parity applicability vector
///
/// ```rust
/// use flash_algo::fragmentation::get_parity_matrix_row;
/// use flash_algo::bitcache::BitCache;
///
/// const M: u32 = 16;
///
/// let mut bitbuf = BitCache::new();
///
/// get_parity_matrix_row(3, M, &mut bitbuf);
///
/// let app_vec = bitbuf.iter().take(M as usize).collect::<Vec<bool>>();
///
/// // The third parity frame is the resulting XOR of 1-indexed data segments:
/// // `D1 ^ D2 ^ D3 ^ D9 ^ D11 ^ D13 ^ D14`.
/// #[cfg(feature = "force-full-r")]
/// assert_eq!(&app_vec, &[
///     true, true, true, false,
///     false, true, false, false,
///     true, false, true, false,
///     true, true, false, false,
/// ]);
/// #[cfg(not(feature = "force-full-r"))]
/// assert_eq!(&app_vec, &[
///     true, true, true, false,
///     false, false, false, false,
///     true, false, true, false,
///     true, true, false, false,
/// ]);
/// ```
///
/// # Panics
/// Will panic if `cap_n` == 0.
/// Will panic if `buf.len()` != `cap_m`
pub fn get_parity_matrix_row(cap_n: u32, cap_m: u32, buf: &mut BitCache) {
    assert!(cap_n != 0);
    assert!(buf.capacity() >= cap_m as usize);

    buf.set_all(false);

    let m = u32::from(cap_m.is_power_of_two());
    let mut x = 1 + (1001_u32.wrapping_mul(cap_n));
    let mut nb_coeff = 0;
    while nb_coeff < (cap_m >> 1_u32) {
        let mut r = 1 << 16_u32;
        while r >= cap_m {
            x = fragmentation_prbs23(x);
            r = x % (cap_m + m);
        }

        // NOTE: C++ code ALLOWS redundancy, but the MATLAB code DOES NOT
        // we probably SHOULD NOT allow redundancy, unless we need to for
        // compat reasons.
        //
        // This check ensures that we don't count "duplicate" spread hits. The goal
        // here is that we pick a random 1/2 of all segments, but WITHOUT this check, we
        // don't catch cases when we pick the same segment multiple times!
        //
        // This essentially is the difference between each parity chunk covering
        //
        // * R == 1/2 of data segments (with `force-full-r`)
        // * 1 <= R <= 1/2 of data segments (without `force-full-r`)
        let allow_redundancy = cfg!(not(feature = "force-full-r"));

        // NOTE: we already asserted on bounds checking above
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::indexing_slicing)] // TODO: eliminate possible panic
        if allow_redundancy || !buf.get(r as usize).unwrap() {
            buf.set(r as usize, true).unwrap();
            nb_coeff += 1;
        }
    }
}
