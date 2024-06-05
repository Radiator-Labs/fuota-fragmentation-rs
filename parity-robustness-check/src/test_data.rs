use serde::{Deserialize, Serialize};

use crate::{fragments::Fragments, omissions::Omissions};

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct TestRun {
    cycle_count: usize,
    fragment_size: usize,
    parity_percentage: f64,
    num_data_fragments: usize,
    num_fragments: usize,
    seed: [u8; 32],
    cycles: Vec<TestCycle>,
}
impl TestRun {
    pub(crate) fn new(
        cycle_count: usize,
        fragment_size: usize,
        parity_percentage: f64,
        seed: &[u8; 32],
        fragments: &Fragments,
    ) -> Self {
        Self {
            cycle_count,
            fragment_size,
            parity_percentage,
            num_data_fragments: fragments.num_data_fragments(),
            num_fragments: fragments.num_data_fragments() + fragments.num_parity_fragments(),
            seed: *seed,
            cycles: Vec::<TestCycle>::new(),
        }
    }

    pub(crate) fn add_cycle(&mut self, cycle_result: TestCycle) {
        self.cycles.push(cycle_result);
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct TestCycle {
    num_skips_in_data: usize,
    num_skips: usize,
    result: Result,
}
impl TestCycle {
    pub(crate) fn passed(final_index: usize, omissions: &Omissions) -> Self {
        Self {
            num_skips_in_data: omissions.num_skips_in_data(),
            num_skips: omissions.num_skips(),
            result: Result::Pass { final_index },
        }
    }
    pub(crate) fn failed_crc(omissions: &Omissions) -> Self {
        Self {
            num_skips_in_data: omissions.num_skips_in_data(),
            num_skips: omissions.num_skips(),
            result: Result::FailedCrc,
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) enum Result {
    Pass { final_index: usize },
    FailedCrc,
    FailedEquality,
}
