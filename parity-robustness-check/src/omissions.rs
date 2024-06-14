use crate::fragments::Fragments;
use rand::{rngs::SmallRng, Rng};

pub(crate) struct Omissions {
    num_skips_in_data: usize,
    num_skips: usize,
    emit_fragment: Vec<bool>,
}

impl Omissions {
    pub(crate) fn new(
        skip_percent: f64,
        burst_skip_percent: f64,
        rng: &mut SmallRng,
        fragments: &Fragments,
    ) -> Self {
        let num_data_fragments = fragments.num_data_fragments();
        let num_parity_fragments = fragments.num_parity_fragments();
        let mut emit_fragment = Vec::<bool>::new();
        for _ in 0..num_data_fragments {
            let r: f64 = rng.gen();
            emit_fragment.push(r > skip_percent);
        }
        for _ in 0..num_parity_fragments {
            let r: f64 = rng.gen();
            emit_fragment.push(r > skip_percent);
        }
        let num_fragments = num_data_fragments + num_parity_fragments;
        let num_burst_skip = ((num_fragments as f64) * burst_skip_percent) as usize;
        let r: f64 = rng.gen();
        let start = (((num_fragments - num_burst_skip) as f64) * r) as usize;
        for item in emit_fragment.iter_mut().skip(start).take(num_burst_skip) {
            *item = false;
        }

        let num_skips_in_data = emit_fragment[0..num_data_fragments]
            .iter()
            .filter(|m| !**m)
            .count();
        let num_skips = emit_fragment.iter().filter(|m| !**m).count();
        Self {
            emit_fragment,
            num_skips_in_data,
            num_skips,
        }
    }

    pub(crate) fn emit(&self, index: usize) -> bool {
        assert!(
            index < self.emit_fragment.len(),
            "index {index}, frag len {}",
            self.emit_fragment.len()
        );

        self.emit_fragment[index]
    }

    pub(crate) fn num_skips_in_data(&self) -> usize {
        self.num_skips_in_data
    }

    pub(crate) fn num_skips(&self) -> usize {
        self.num_skips
    }
}
