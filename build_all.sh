        (cd original-flash-algo && cargo build --release --target thumbv7em-none-eabi --no-default-features)
        (cd original-flash-algo && cargo build --release --target thumbv7em-none-eabi --features force-full-r)
        (cd original-flash-algo && cargo build --release --target thumbv7em-none-eabi --features defmt)
        (cd original-flash-algo && cargo build --release --target thumbv7em-none-eabi --features "force-full-r,defmt")
        (cd flash-algo-new && cargo build --release --target thumbv7em-none-eabi --no-default-features)
        (cd flash-algo-new && cargo build --release --target thumbv7em-none-eabi --features force-full-r)
        (cd flash-algo-new && cargo build --release --target thumbv7em-none-eabi --features matrixreconstructor)
        (cd flash-algo-new && cargo build --release --target thumbv7em-none-eabi --features defmt)
        (cd flash-algo-new && cargo build --release --target thumbv7em-none-eabi --features "force-full-r,defmt")
        (cd flash-algo-new && cargo build --release --target thumbv7em-none-eabi --features "matrixreconstructor,defmt")
        cargo test --no-default-features
        cargo test --no-default-features --features force-full-r
        cargo test --no-default-features --features matrixreconstructor
        cargo fmt --all -- --check
        cargo clippy --no-default-features
        cargo clippy --no-default-features --features force-full-r
        cargo clippy --no-default-features --features matrixreconstructor
        cargo machete
