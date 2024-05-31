use clap::Parser;
use crc::{Crc, CRC_32_CKSUM};
use flash_algo::{
    bitcache::BitCache,
    fragmentation::get_parity_matrix_row,
    protocol::{Crc32, FlashRepr, Signature},
};
use fragments::Fragments;
use std::{fs::File, io::Write};

pub mod fragments;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the binary file
    #[arg(short, long)]
    file: String,

    /// Length of a message in bytes
    #[arg(short, long, default_value_t = 50)]
    size: usize,

    /// Parity percentage
    #[arg(short, long, default_value_t = 0.45)]
    parity_pct: f64,
}
const MAX_NUM_SEGMENTS: usize = 0x0000_4000_usize;
fn main() {
    let args = Args::parse();
    if args.file.is_empty() {
        panic!("File name missing")
    }

    let Ok(f) = std::fs::read(args.file.clone()) else {
        println!("No file '{}'", args.file);
        return;
    };
    println!("Loaded '{}'. Size: {}", args.file, f.len());
    let bin_file = f;

    let fragments = make_fragments(&bin_file, args.size, args.parity_pct);

    let fragment_size = fragments.fragment_size();
    let num_data_fragments = fragments.num_data_fragments();
    let num_parity_fragments = fragments.num_parity_fragments();
    let num_fragments = num_data_fragments + num_parity_fragments;
    let crc32 = fragments.crc32();
    println!(
        "Sz {fragment_size} #f {num_data_fragments} #p {num_parity_fragments} crc {crc32:#010x}"
    );
    assert!(
        MAX_NUM_SEGMENTS >= num_fragments,
        "Number of segments (f+p) {} must be less or equal to {} (2^14).",
        num_fragments,
        MAX_NUM_SEGMENTS
    );

    let extended_file_name = format!("{}.json", args.file);
    let mut file = match File::create(extended_file_name.clone()) {
        Err(err) => {
            let message = format!("Unable to create {extended_file_name}, due to {err:?}");
            panic!("{message}");
        }
        Ok(file) => file,
    };
    let j = match serde_json::to_string(&fragments) {
        Err(err) => {
            let message = format!("Unable to serialize fm, due to {err:?}");
            panic!("{message}");
        }
        Ok(j) => j,
    };
    if let Err(err) = file.write_all(j.as_bytes()) {
        let message = format!("Unable to write file, due to {err:?}");
        panic!("{message}");
    }
}

fn make_fragments(unpadded_bin: &[u8], segment_size: usize, parity_pct: f64) -> Fragments {
    let mut sized_bin = unpadded_bin.to_vec();
    while (sized_bin.len() + Crc32::SIZE + Signature::SIZE) % segment_size != 0 {
        sized_bin.push(0);
    }

    // Calculate CRC
    let crc_engine = Crc::<u32>::new(&CRC_32_CKSUM);
    let crc32 = crc_engine.checksum(&sized_bin);

    // Break firmware into segments
    let data_fragments: Vec<Vec<u8>> = sized_bin
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

    Fragments::new(crc32, data_fragments, parity_fragments)
}
