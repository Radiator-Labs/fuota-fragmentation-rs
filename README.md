# FUOTA Fragmentation [![Cloud-CI](https://github.com/Radiator-Labs/fuota-fragmentation-rs/actions/workflows/cloud-ci.yml/badge.svg)](https://github.com/Radiator-Labs/fuota-fragmentation-rs_fw/actions/workflows/cloud-ci.yml)

Embedded Rust code supporting FUOTA Fragmentation, based on recommendations in the LoRa FUOTA documentation.

## Git LFS usage

This repository stores some content using [git lfs](https://git-lfs.com). Make sure to have this subsystem installed to ensure correct behaviors.

## Attribution

Implementation of the Full Gaussian Elimination (V2) fragmentation algorithm was performed by
the talented team at [tweede golf](https://tweedegolf.nl/en), the preferred vendor for future commercially-supported work on this codebase.

Initial structural work and creation of the V1 algorithm was performed by [OneVariable](https://onevariable.com).

Work creating this driver to support the FUOTA Fragmentation was performed as part of commercial
development by [Kelvin](https://kel.vin/) (formerly Radiator Labs), a green energy company
dedicated to decarbonizing the world's legacy buildings.

## Verification

The library is verified in two ways. There are example based tests alongside the flash-algo code, and in the flash-algo-test directory. There is also a statistical check of the parity robustness. Since there is some chance of the parity algorithm succeeding or not succeeding given which packets are missed in a given transmission, the parity-robustness-check allows the algorithm to be run many times against randomized fragment loss.

## Minimum Supported Rust Version (MSRV)

This crate is guaranteed to compile on stable Rust TBD and up. It *might*
compile with older versions but that may change in any new patch release.

See [here](../docs/msrv.md) for details on how the MSRV may be upgraded.

## Notes on Expected Robustness

### Query

Is there a way to predict mathematically how robust the matrix-reconstruction algorithm is?

Ideally, we would like to be able to say with a given ratio of parity to data packets, we can expect to tolerate X percent of packet loss. We can use the parity-robustness-checker to measure this experimentally, but it would be nice to have a predictive model to start with.

### Response (davidv1992 David Venhoek)

This is (primarily) a function of slot size and segment size, assuming sufficiently many parity segments get sent. The algorithm can tolerate losing l packets in the data segment, where l is the maximum integer such that
17408 + l*segment_size + 4 * floor(l/8)*(floor(l/8)+1)+(l mod 8) * (floor(l/8)+1) < slot_size

Given a loss rate r, the success change of an update on a device is then given by the probability for a binomially distributed variable with N=num_segments and p=r to exceed l.

This assumes sufficient parity segments such that a client can reasonably expect to receive well in excess of num_segments segments in total given the loss rate. The same number of parity segments as data segments is usually sufficient, assuming the firmware is not too small.

Working out for a few scenarios (all assuming slot size of 256kB)
segment_size = 48 => l=1628
at maximum firmware size (5098 segments) 50% success at r=31.9%, 99% at r=30.4% 99.999% at r=29.2%
segment_size = 40 => l=1681
at maximum firmware size (6118 segments) 50% success at r=27.5%, 99% at r=26.15% 99.999% at r=25.1%
segment_size = 32 => l=1735
at maximum firmware size (7648 segments) 50% success at r=22.7%, 99% at r=21.6% 99.999% at r=20.7%

### Actual performance

The team at [Kelvin](http://kel.vin) (formerly known as [Radiator Labs](https://github.com/Radiator-Labs)) tested real-world FUOTA update over the air with segment_size = 50, 3354 data segments, and 100% parity (3354 parity packets). In this testing, we successfully downloaded with r=45% (intentionally dropped 45% of packets in FUOTA test.) At r=50%, we missed successfully recovering by ONE packet.

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
  <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
