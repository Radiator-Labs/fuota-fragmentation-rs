use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Deserialize, Serialize)]
pub(crate) struct TestRun {
    cycles: Vec<TestCycle>,
}
impl TestRun {
    pub(crate) fn add_cycle(&mut self, cycle_result: TestCycle) {
        self.cycles.push(cycle_result);
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct TestCycle {
    result: Result,
}
impl TestCycle {
    pub(crate) fn passed() -> Self {
        Self {
            result: Result::Pass,
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
    Pass,
    FailedCrc,
    FailedEquality,
}
