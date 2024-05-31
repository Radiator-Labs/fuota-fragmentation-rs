use serde::{Deserialize, Serialize};

#[allow(clippy::exhaustive_structs)]
#[derive(Serialize, Deserialize)]
pub struct Fragments {
    crc32: u32,
    data_fragments: Vec<Vec<u8>>,
    parity_fragments: Vec<Vec<u8>>,
}

impl Fragments {
    pub fn new(crc32: u32, data_fragments: Vec<Vec<u8>>, parity_fragments: Vec<Vec<u8>>) -> Self {
        Self {
            crc32,
            data_fragments,
            parity_fragments,
        }
    }

    pub fn crc32(&self) -> u32 {
        self.crc32
    }

    pub fn fragment_size(&self) -> usize {
        self.data_fragments[0].len()
    }
    pub fn num_data_fragments(&self) -> usize {
        self.data_fragments.len()
    }
    pub fn num_parity_fragments(&self) -> usize {
        self.parity_fragments.len()
    }
}
