// #[cfg(test)]
// mod test {
//     // use super::TestFuota;
//     use bin_fragmenter::fragments::make_fragments;

//     const FRAGMENT_SIZE: usize = 40;
//     const PARITY_PERCENT: f64 = 0.45;
//     static RAW_FW: &[u8] =
//         include_bytes!("../../flash-algo-test/test-assets/firmware-001/example.bin");

//     #[test]
//     async fn check_it_out() {
//         // create simulated flash with known data
//         // fragment input
//         let fragments = make_fragments(&RAW_FW, FRAGMENT_SIZE, PARITY_PERCENT);
//         let mut test_fuota = TestFuota::new().await;
//         test_fuota.start_session(fragments.);

//         // prep flash for download
//         // "send" each fragment to download
//         // check each result for "complete"
//         // Once complete, confirm the content
//         // Can double-check the content with the CRC, if desired.
//     }
// }

mod fragments;
mod test_data;

use clap::Parser;
use flash_algo::manager::{
    ActiveStatus, AppBootStatus, ScratchRam, SlotManager, WriteSegmentOutcome,
};
use flash_algo_test::heap_flash::Flash;
use fragments::Fragments;
use test_data::{TestCycle, TestRun};

const FRAGMENT_SIZE: usize = 40;
const PARITY_PERCENT: f64 = 0.45;
static RAW_FW: &[u8] = include_bytes!("../../flash-algo-test/test-assets/firmware-001/example.bin");

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Number of test cycles to execute
    #[arg(short, long, default_value_t = 1)]
    count: usize,
}

#[async_std::main]
async fn main() -> std::io::Result<()> {
    let args = Args::parse();
    let mut results = TestRun::default();
    let fragments = Fragments::new(RAW_FW, FRAGMENT_SIZE, PARITY_PERCENT);
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

    let mut completed = insert_fragments(&mut test_fuota, fragments.data_fragments(), 1).await;
    if !completed {
        completed = insert_fragments(
            &mut test_fuota,
            fragments.parity_fragments(),
            1 + fragments.num_data_fragments(),
        )
        .await;
    }
    if !completed {
        TestCycle::failed_crc()
    } else {
        TestCycle::passed()
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
) -> bool {
    for (i, fragment) in fragment_set.iter().enumerate() {
        let index = offset + i;
        if let Some(resp) = test_fuota.insert_fragment(index, fragment).await {
            match resp {
                FuotaResponse::FuotaComplete => {
                    return true;
                }
                FuotaResponse::FuotaStatusNoSession => {
                    panic!("Unexpected FuotaStatusNoSession at {index}")
                }
                FuotaResponse::FuotaStatusActiveSession {
                    number_of_frags_received,
                    number_of_frags_missing,
                } => panic!("Unexpected FuotaStatusActiveSession at {index}. Num received {number_of_frags_received}, num missing {number_of_frags_missing}"),
            }
        }
    }
    false
}

struct TestFuota {
    flash: Flash,
    manager: SlotManager<4>,
    scratch: ScratchRam,
    opt_session: Option<ActiveStatus>,
}
impl TestFuota {
    pub async fn new() -> Self {
        let mut test_fuota = Self {
            // Create a 1MiB flash part with 64KiB blocks
            flash: Flash::new(64 * 1024, 1024 * 1024),
            manager: SlotManager::<4>::new(256 * 1024),
            scratch: ScratchRam::new(),
            opt_session: None,
        };
        test_fuota.opt_session = test_fuota.initialize().await;
        test_fuota
    }
    async fn initialize(&mut self) -> Option<ActiveStatus> {
        match self
            .manager
            .app_boot_status(&mut self.flash, &mut self.scratch)
            .await
        {
            Ok(initial_fuota_status) => match initial_fuota_status {
                AppBootStatus::Idle => None,
                AppBootStatus::InProgress(session) => Some(session),
            },
            Err(e) => panic!("app_boot_status error {e:?}",),
        }
    }

    pub async fn start_session(&mut self, num_fragments: u32, fragment_size: u32) {
        self.frag_session_delete().await;
        self.opt_session = match self
            .manager
            .start(
                &mut self.flash,
                &mut self.scratch,
                fragment_size,
                num_fragments,
            )
            .await
        {
            Ok(session) => Some(session),
            Err(e) => panic!("FragSessionSetupRequest error {e:?}"),
        };
    }

    async fn frag_session_delete(&mut self) {
        self.opt_session = None;
        if let Err(e) = self
            .manager
            .cancel_all_ext_pending_from_scratch(&mut self.flash, &mut self.scratch)
            .await
        {
            panic!("FragSessionDeleteRequest error {e:?}");
        }
    }

    async fn insert_fragment(&mut self, index: usize, fragment: &[u8]) -> Option<FuotaResponse> {
        match self.opt_session {
            Some(ref mut session) => {
                match session
                    .write_segment(
                        &mut self.flash,
                        &mut self.scratch.firmware_rd_scratch,
                        index as u32,
                        fragment,
                    )
                    .await
                {
                    Ok(res) => match res {
                        WriteSegmentOutcome::Consumed => None,
                        WriteSegmentOutcome::ConsumedMaybeParity => {
                            act_on_consumed_maybe_parity(
                                session,
                                &mut self.flash,
                                &mut self.scratch,
                            )
                            .await
                        }
                        WriteSegmentOutcome::FirmwareComplete => Some(
                            act_on_firmware_complete(session, &mut self.flash, &mut self.scratch)
                                .await,
                        ),
                    },
                    Err(e) => {
                        panic!("DataFragment error {e:?} at {index}");
                    }
                }
            }
            None => {
                panic!("Fragment with no active session");
            }
        }
    }
}

async fn act_on_consumed_maybe_parity(
    session: &mut ActiveStatus,
    flash: &mut Flash,
    scratch: &mut ScratchRam,
) -> Option<FuotaResponse> {
    'repair: loop {
        let rep_res = session.repair_step(flash, scratch).await;
        match rep_res {
            Ok(Some(_n)) => (),
            Ok(None) => break 'repair,
            Err(_) => panic!("FUOTA write error"),
        }
    }
    // We might be complete if a repair just completed
    if session.is_complete() {
        Some(act_on_firmware_complete(session, flash, scratch).await)
    } else {
        None
    }
}

#[allow(clippy::used_underscore_binding)]
async fn act_on_firmware_complete(
    session: &mut ActiveStatus,
    flash: &mut Flash,
    scratch: &mut ScratchRam,
) -> FuotaResponse {
    match session.check_and_mark_done(flash, scratch).await {
        Ok(_slot) => FuotaResponse::FuotaComplete,
        Err(e) => panic!("check_and_mark_done error {e:?}"),
    }
}

#[derive(Debug, Clone, Copy)]
#[allow(clippy::exhaustive_enums)]
pub enum FuotaResponse {
    FuotaComplete,
    FuotaStatusNoSession,
    FuotaStatusActiveSession {
        number_of_frags_received: u16,
        number_of_frags_missing: u16,
    },
    // FuotaError,
}
