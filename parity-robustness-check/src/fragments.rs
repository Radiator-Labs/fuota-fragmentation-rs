use crc::{Crc, CRC_32_CKSUM};
use flash_algo::{
    bitcache::BitCache,
    fragmentation::get_parity_matrix_row,
    protocol::{Crc32, FlashRepr, Signature},
};

#[allow(clippy::exhaustive_structs)]
pub struct Fragments {
    _crc32: u32,
    data_fragments: Vec<Vec<u8>>,
    parity_fragments: Vec<Vec<u8>>,
}

impl Fragments {
    pub fn new(unpadded_bin: &[u8], segment_size: usize, parity_pct: f64) -> Self {
        let mut sized_bin = unpadded_bin.to_vec();
        while (sized_bin.len() + Crc32::SIZE + Signature::SIZE) % segment_size != 0 {
            sized_bin.push(0);
        }

        // Calculate CRC
        let crc_engine = Crc::<u32>::new(&CRC_32_CKSUM);
        let crc32 = crc_engine.checksum(&sized_bin);

        let mut input = vec![];
        input.extend_from_slice(&crc32.to_le_bytes());
        input.extend_from_slice(&[0xFF; 64]);
        input.extend_from_slice(&sized_bin);

        // Break firmware into segments
        let data_fragments: Vec<Vec<u8>> = input
            .chunks_exact(segment_size)
            .map(<[u8]>::to_vec)
            .collect();

        let num_data_fragments = data_fragments.len();
        let num_parity_fragments = ((data_fragments.len() as f64) * parity_pct) as usize;

        // Generate parity segments
        let mut parity_fragments = vec![];

        for i in 0..num_parity_fragments {
            let mut bitbuf = BitCache::new();
            get_parity_matrix_row((i + 1) as u32, num_data_fragments as u32, &mut bitbuf);
            let parity_row = bitbuf
                .iter()
                .take(num_data_fragments)
                .collect::<Vec<bool>>();

            let parity_fragment = parity_row.iter().zip(data_fragments.iter()).fold(
                vec![0_u8; segment_size],
                |mut buf, (applies, data)| {
                    if *applies {
                        buf.iter_mut().zip(data.iter()).for_each(|(b, d)| {
                            *b ^= *d;
                        });
                    }
                    buf
                },
            );
            parity_fragments.push(parity_fragment);
        }
        Self {
            _crc32: crc32,
            data_fragments,
            parity_fragments,
        }
    }

    pub fn _crc32(&self) -> u32 {
        self._crc32
    }

    pub fn fragment_size(&self) -> usize {
        self.data_fragments[0].len()
    }
    pub fn num_data_fragments(&self) -> usize {
        self.data_fragments.len()
    }
    pub fn _num_parity_fragments(&self) -> usize {
        self.parity_fragments.len()
    }

    pub fn data_fragments(&self) -> &Vec<Vec<u8>> {
        &self.data_fragments
    }
    pub fn parity_fragments(&self) -> &Vec<Vec<u8>> {
        &self.parity_fragments
    }
}
