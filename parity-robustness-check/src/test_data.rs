use serde::{Deserialize, Serialize};

use crate::fragments::Fragments;

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct TestRun {
    cycle_count: usize,
    fragment_size: usize,
    parity_percentage: f64,
    num_data_fragments: usize,
    num_fragments: usize,
    cycles: Vec<TestCycle>,
}
impl TestRun {
    pub(crate) fn new(
        cycle_count: usize,
        fragment_size: usize,
        parity_percentage: f64,
        fragments: &Fragments,
    ) -> Self {
        Self {
            cycle_count,
            fragment_size,
            parity_percentage,
            num_data_fragments: fragments.num_data_fragments(),
            num_fragments: fragments.num_data_fragments() + fragments.num_parity_fragments(),

            cycles: Vec::<TestCycle>::new(),
        }
    }

    pub(crate) fn add_cycle(&mut self, cycle_result: TestCycle) {
        self.cycles.push(cycle_result);
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct TestCycle {
    result: Result,
}
impl TestCycle {
    pub(crate) fn passed(final_index: usize) -> Self {
        Self {
            result: Result::Pass { final_index },
        }
    }
    pub(crate) fn failed_crc() -> Self {
        Self {
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
