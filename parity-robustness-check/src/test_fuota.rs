use flash_algo::manager::{
    ActiveStatus, AppBootStatus, ScratchRam, SlotManager, WriteSegmentOutcome,
};
use flash_algo_test::heap_flash::Flash;

pub(crate) struct TestFuota {
    flash: Flash,
    manager: SlotManager<4>,
    scratch: ScratchRam,
    opt_session: Option<ActiveStatus>,
}
impl TestFuota {
    pub(crate) async fn new() -> Self {
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

    pub(crate) async fn insert_fragment(&mut self, index: usize, fragment: &[u8]) -> FuotaResponse {
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
                        WriteSegmentOutcome::Consumed => FuotaResponse::Incomplete,
                        WriteSegmentOutcome::ConsumedMaybeParity => {
                            act_on_consumed_maybe_parity(
                                session,
                                &mut self.flash,
                                &mut self.scratch,
                                index,
                            )
                            .await
                        }
                        WriteSegmentOutcome::FirmwareComplete => {
                            act_on_firmware_complete(
                                session,
                                &mut self.flash,
                                &mut self.scratch,
                                index,
                            )
                            .await
                        }
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
    index: usize,
) -> FuotaResponse {
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
        act_on_firmware_complete(session, flash, scratch, index).await
    } else {
        FuotaResponse::Incomplete
    }
}

#[allow(clippy::used_underscore_binding)]
async fn act_on_firmware_complete(
    session: &mut ActiveStatus,
    flash: &mut Flash,
    scratch: &mut ScratchRam,
    index: usize,
) -> FuotaResponse {
    match session.check_and_mark_done(flash, scratch).await {
        Ok(_slot) => FuotaResponse::Complete {
            last_fragment_index: index,
        },
        Err(e) => panic!("check_and_mark_done error {e:?}"),
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(clippy::exhaustive_enums)]
pub(crate) enum FuotaResponse {
    Incomplete,
    Complete { last_fragment_index: usize },
    // FuotaError,
}
