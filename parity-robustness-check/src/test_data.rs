use crate::{fragments::Fragments, omissions::Omissions};
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct TestRun {
    cycle_count: usize,
    fragment_size: usize,
    parity_percentage: f64,
    skip_rate: f64,
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
        skip_rate: f64,
        seed: &[u8; 32],
        fragments: &Fragments,
    ) -> Self {
        Self {
            cycle_count,
            fragment_size,
            parity_percentage,
            skip_rate,
            num_data_fragments: fragments.num_data_fragments(),
            num_fragments: fragments.num_data_fragments() + fragments.num_parity_fragments(),
            seed: *seed,
            cycles: Vec::<TestCycle>::new(),
        }
    }

    pub(crate) fn add_cycle(&mut self, cycle_result: TestCycle) {
        self.cycles.push(cycle_result);
    }

    pub(crate) fn summary(&self) -> TestRunSummary {
        let num_passed = self.cycles.iter().filter(|m| m.is_passed()).count();
        let num_failed_crc = self.cycles.iter().filter(|m| m.is_failed_crc()).count();
        let num_failed_equality = self
            .cycles
            .iter()
            .filter(|m| m.is_failed_equality())
            .count();
        let total = num_passed + num_failed_crc + num_failed_equality;
        let num_passing_emits = self
            .cycles
            .iter()
            .fold(0, |acc, m| acc + m.num_sent_count());
        assert!(total == self.cycle_count);

        let pass_percentage = (num_passed as f64 / total as f64) * 100.0;
        let average_passing_emit_count = num_passing_emits as f64 / total as f64;

        TestRunSummary {
            cycle_count: self.cycle_count,
            fragment_size: self.fragment_size,
            parity_percentage: self.parity_percentage,
            skip_rate: self.skip_rate,
            num_data_fragments: self.num_data_fragments,
            num_fragments: self.num_fragments,
            // seed: self.seed,
            num_passed,
            num_failed_crc,
            num_failed_equality,
            pass_percentage,
            average_passing_emit_count,
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct TestRunSummary {
    cycle_count: usize,
    fragment_size: usize,
    parity_percentage: f64,
    skip_rate: f64,
    num_data_fragments: usize,
    num_fragments: usize,
    // seed: [u8; 32],
    num_passed: usize,
    num_failed_crc: usize,
    num_failed_equality: usize,
    pass_percentage: f64,
    average_passing_emit_count: f64,
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
    pub(crate) fn is_passed(&self) -> bool {
        matches!(self.result, Result::Pass { final_index: _ })
    }
    pub(crate) fn is_failed_crc(&self) -> bool {
        self.result == Result::FailedCrc
    }
    pub(crate) fn is_failed_equality(&self) -> bool {
        self.result == Result::FailedEquality
    }
    pub(crate) fn num_sent_count(&self) -> usize {
        if let Result::Pass { final_index } = self.result {
            final_index
        } else {
            0
        }
    }
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
pub(crate) enum Result {
    Pass { final_index: usize },
    FailedCrc,
    FailedEquality,
}
