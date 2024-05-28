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

## Minimum Supported Rust Version (MSRV)

This crate is guaranteed to compile on stable Rust TBD and up. It *might*
compile with older versions but that may change in any new patch release.

See [here](../docs/msrv.md) for details on how the MSRV may be upgraded.

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
