mod fragments;
mod omissions;
mod test_data;
mod test_fuota;

use clap::Parser;
use fragments::Fragments;
use omissions::Omissions;
use rand::{prelude::*, SeedableRng};
use test_data::{TestCycle, TestRun};
use test_fuota::{FuotaResponse, TestFuota};

static RAW_FW: &[u8] = include_bytes!("../../flash-algo-test/test-assets/firmware-001/example.bin");

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Output the detailed JSON output
    #[arg(short, long, default_value_t = false)]
    detailed: bool,

    /// Number of test cycles to execute
    #[arg(short, long, default_value_t = 1)]
    count: usize,

    /// Size of the fragments
    #[arg(short, long, default_value_t = 40)]
    size: usize,

    /// Amount of parity fragments as a fraction of the number of data fragments
    #[arg(short, long, default_value_t = 0.45)]
    parity: f64,

    /// Percent of fragments to not skip
    #[arg(short, long, default_value_t = 0.02)]
    skip_rate: f64,
    // /// Seed for psuedo-random numbers used randomizing the parity check
    // #[arg(short, long)]
    // seed: [u8; 32],
}

#[async_std::main]
async fn main() -> std::io::Result<()> {
    let args = Args::parse();
    assert!(args.count > 0);
    assert!(args.size > 0);
    assert!(args.parity >= 0.0);
    assert!(args.skip_rate >= 0.0);
    assert!(args.skip_rate <= 1.0);

    let mut seed: <SmallRng as SeedableRng>::Seed = Default::default();
    thread_rng().fill(&mut seed);
    let mut rng = SmallRng::from_seed(seed);

    let fragments = Fragments::new(RAW_FW, args.size, args.parity);
    let mut results = TestRun::new(
        args.count,
        args.size,
        args.parity,
        args.skip_rate,
        &seed,
        &fragments,
    );
    for _ in 0..args.count {
        let omissions = Omissions::new(args.skip_rate, &mut rng, &fragments);
        results.add_cycle(perform_test_cycle(&fragments, &omissions).await);
    }

    if args.detailed {
        let detailed_results = ::serde_json::to_string_pretty(&results).unwrap();
        println!("{detailed_results}");
    }
    let summary = ::serde_json::to_string_pretty(&results.summary()).unwrap();
    println!("{summary}");

    Ok(())
}

async fn perform_test_cycle(fragments: &Fragments, omissions: &Omissions) -> TestCycle {
    // create simulated flash with known data
    // fragment input
    let mut test_fuota = TestFuota::new().await;
    test_fuota
        .start_session(
            fragments.num_data_fragments() as u32,
            fragments.fragment_size() as u32,
        )
        .await;

    let mut fuota_result =
        insert_fragments(&mut test_fuota, fragments.data_fragments(), 0, omissions).await;
    if fuota_result == FuotaResponse::Incomplete {
        fuota_result = insert_fragments(
            &mut test_fuota,
            fragments.parity_fragments(),
            fragments.num_data_fragments(),
            omissions,
        )
        .await;
    }
    match fuota_result {
        FuotaResponse::Incomplete => TestCycle::failed_crc(omissions),
        FuotaResponse::Complete {
            last_fragment_index,
        } => TestCycle::passed(last_fragment_index, omissions),
    }
}

async fn insert_fragments(
    test_fuota: &mut TestFuota,
    fragment_set: &[Vec<u8>],
    offset: usize,
    omissions: &Omissions,
) -> FuotaResponse {
    for (i, fragment) in fragment_set.iter().enumerate() {
        let index = offset + i;
        if omissions.emit(index) {
            match test_fuota.insert_fragment(index + 1, fragment).await {
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
    }
    FuotaResponse::Incomplete
}
