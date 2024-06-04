mod fragments;
mod test_data;
mod test_fuota;

use clap::Parser;
use fragments::Fragments;
use test_data::{TestCycle, TestRun};
use test_fuota::{FuotaResponse, TestFuota};

static RAW_FW: &[u8] = include_bytes!("../../flash-algo-test/test-assets/firmware-001/example.bin");

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Number of test cycles to execute
    #[arg(short, long, default_value_t = 1)]
    count: usize,

    #[arg(short, long, default_value_t = 40)]
    size: usize,

    #[arg(short, long, default_value_t = 0.45)]
    parity: f64,
}

#[async_std::main]
async fn main() -> std::io::Result<()> {
    let args = Args::parse();
    assert!(args.count > 0);
    assert!(args.size > 0);
    assert!(args.parity >= 0.0);

    let fragments = Fragments::new(RAW_FW, args.size, args.parity);
    let mut results = TestRun::new(args.count, args.size, args.parity, &fragments);
    for _ in 0..args.count {
        results.add_cycle(perform_test_cycle(&fragments).await);
    }
    println!("result: {results:?}");
    Ok(())
}

async fn perform_test_cycle(fragments: &Fragments) -> TestCycle {
    // create simulated flash with known data
    // fragment input
    let mut test_fuota = TestFuota::new().await;
    test_fuota
        .start_session(
            fragments.num_data_fragments() as u32,
            fragments.fragment_size() as u32,
        )
        .await;

    let mut fuota_result = insert_fragments(&mut test_fuota, fragments.data_fragments(), 1).await;
    if fuota_result == FuotaResponse::Incomplete {
        fuota_result = insert_fragments(
            &mut test_fuota,
            fragments.parity_fragments(),
            1 + fragments.num_data_fragments(),
        )
        .await;
    }
    match fuota_result {
        FuotaResponse::Incomplete => TestCycle::failed_crc(),
        FuotaResponse::Complete {
            last_fragment_index,
        } => TestCycle::passed(last_fragment_index),
    }

    // "send" each fragment to download
    // check each result for "complete"
    // Once complete, confirm the content
    // Can double-check the content with the CRC, if desired.
}

async fn insert_fragments(
    test_fuota: &mut TestFuota,
    fragment_set: &[Vec<u8>],
    offset: usize,
) -> FuotaResponse {
    for (i, fragment) in fragment_set.iter().enumerate() {
        let index = offset + i;
        match test_fuota.insert_fragment(index, fragment).await {
            FuotaResponse::Incomplete => (),
            FuotaResponse::Complete {
                last_fragment_index,
            } => {
                return FuotaResponse::Complete {
                    last_fragment_index,
                }
            }
        }
    }
    FuotaResponse::Incomplete
}
