//! Basic test, using the reference data provided here:
//!
//! <https://github.com/elsalahy/test-fuota-server>

use crc::Crc;
use flash_algo::{
    bitcache::BitCache,
    fragmentation::get_parity_matrix_row,
    manager::{
        ActiveStatus, AppBootStatus, ScratchRam, SlotManager, WriteSegmentOutcome, HEADER_SIZE,
    },
    protocol::{segment_status_table::MAX_SEGMENTS, Crc32, FlashRepr, Signature},
};

const SEGMENT_SIZE_BYTES: usize = 45;
static RAW_FW: &[u8] = include_bytes!("../test-assets/firmware-001/example.bin");

fn generate_parity_rows(count: usize) -> Vec<Vec<u8>> {
    let data_chunks = fw_with_hdr()
        .chunks(SEGMENT_SIZE_BYTES)
        .map(|s| s.to_vec())
        .collect::<Vec<_>>();
    let mut g = vec![];

    // Generate parity matrix
    for i in 0..count {
        let mut row = BitCache::new();
        get_parity_matrix_row((i + 1) as u32, data_chunks.len() as u32, &mut row);
        g.push(row.iter().collect());
    }

    // Calculate the parity segment *values*, e.g. the data contents
    // of the parity segment
    fn encode(data: &[Vec<u8>], g: &[Vec<bool>]) -> Vec<Vec<u8>> {
        let mut p = Vec::with_capacity(g.len());
        for row in g.iter() {
            let mut payload = vec![0; data[0].len()];

            for (i, flag) in row.iter().enumerate() {
                if *flag {
                    assert_eq!(data[i].len(), payload.len());
                    for (dat, par) in data[i].iter().zip(payload.iter_mut()) {
                        *par ^= dat;
                    }
                }
            }

            p.push(payload);
        }
        p
    }

    encode(&data_chunks, &g)
}

fn padded_fw() -> Vec<u8> {
    let mut v = RAW_FW.to_vec();
    let payload_hdr_len = Crc32::SIZE + Signature::SIZE;
    while (v.len() + payload_hdr_len) % SEGMENT_SIZE_BYTES != 0 {
        v.push(0);
    }
    v
}

fn fw_with_hdr() -> Vec<u8> {
    let mut v = vec![];
    let padded = padded_fw();
    let crc32 = calc_crc32().to_le_bytes();
    let sig = [0xAF; 64];
    v.extend_from_slice(&crc32);
    v.extend_from_slice(&sig);
    v.extend_from_slice(&padded);
    v
}

fn calc_crc32() -> u32 {
    let crc = Crc::<u32>::new(&crc::CRC_32_CKSUM);
    crc.checksum(&padded_fw())
}

fn chunk_size() -> usize {
    SEGMENT_SIZE_BYTES
}

#[tokio::test]
async fn basics() {
    const SLOT_SIZE: usize = 256 * 1024;

    let mut scratch = ScratchRam::new();

    let mut flash = flash_algo_test::heap_flash::Flash::new(64 * 1024, 1024 * 1024);
    let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

    let init = mgr.app_boot_status(&mut flash, &mut scratch).await.unwrap();
    assert_eq!(init, AppBootStatus::Idle);

    let fw_chunks = fw_with_hdr()
        .chunks(SEGMENT_SIZE_BYTES)
        .map(|s| s.to_vec())
        .collect::<Vec<_>>();
    let parity_chunks_ct = (fw_chunks.len() as f64 * 0.5).round() as usize;
    let parity_chunks = generate_parity_rows(parity_chunks_ct);

    let num_fw_chunks = fw_chunks.len() as u32;
    let _num_parity_chunks = parity_chunks.len() as u32;

    let mut start = mgr
        .start(&mut flash, &mut scratch, chunk_size() as u32, num_fw_chunks)
        .await
        .unwrap();

    assert_eq!(
        start,
        ActiveStatus {
            segment_size: chunk_size(),
            slot_size: SLOT_SIZE,
            firmware_slot_idx: 0,
            total_firmware_segments: num_fw_chunks,
            remaining_firmware_segments: num_fw_chunks,
            parity_slot_idx: 1,
            total_parity_segments: 16384,
            remaining_parity_segments: 16384,
        }
    );

    let blank_to_start = flash.dump_to_string();
    insta::assert_snapshot!(blank_to_start);

    for (i, ch) in fw_chunks[..fw_chunks.len() - 1].iter().enumerate() {
        let res = start
            .write_segment(
                &mut flash,
                &mut scratch.firmware_rd_scratch,
                i as u32 + 1,
                ch,
            )
            .await
            .unwrap();
        assert_eq!(res, WriteSegmentOutcome::Consumed);
    }
    let res = start
        .write_segment(
            &mut flash,
            &mut scratch.firmware_rd_scratch,
            num_fw_chunks,
            fw_chunks.last().unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res, WriteSegmentOutcome::FirmwareComplete);

    start
        .check_and_mark_done(&mut flash, &mut scratch)
        .await
        .unwrap();

    let all_done = flash.dump_to_string();
    insta::assert_snapshot!(all_done);

    let validate = mgr
        .validate_firmware_slot(&mut flash, &mut scratch, 0)
        .await
        .unwrap();

    assert_eq!(validate.data_start, {
        // Data should come after:
        //
        // 1. The header
        HEADER_SIZE
        // 2. The "have we gotten the segment" table
        + MAX_SEGMENTS
        // 3. The ed25519 signature of the remaining firmware
        + Signature::SIZE
        // 4. The CRC32 checksum of the remaining firmware
        + Crc32::SIZE
    });
    assert_eq!(validate.data_len, num_fw_chunks as usize * chunk_size());
}

#[cfg(not(feature = "force-full-r"))]
#[tokio::test]
async fn miss_any_one() {
    use flash_algo::{
        manager::{ScratchRam, DATA_REGION_OFFSET},
        spi_flash::SpiFlash,
    };

    let data_frags = fw_with_hdr()
        .chunks(SEGMENT_SIZE_BYTES)
        .map(|s| s.to_vec())
        .collect::<Vec<_>>();
    let parity_chunks_ct = (data_frags.len() as f64 * 0.5).round() as usize;
    let parity_frags = generate_parity_rows(parity_chunks_ct);

    let mut covered = vec![false; data_frags.len()];
    for parity_idx in 0..parity_frags.len() {
        let mut row = BitCache::new();
        get_parity_matrix_row(parity_idx as u32 + 1, data_frags.len() as u32, &mut row);
        let row: Vec<bool> = row.iter().collect();
        covered
            .iter_mut()
            .zip(row.iter())
            .for_each(|(c, p)| *c = *c || *p);
    }

    let mut is_cov = vec![];
    let mut not_cov = vec![];

    for (i, b) in covered.iter().enumerate() {
        if *b {
            is_cov.push(i);
        } else {
            not_cov.push(i);
        }
    }

    println!("is: {:?}, isn't: {:?}", is_cov, not_cov);
    const SLOT_SIZE: usize = 256 * 1024;

    let mut scratch = ScratchRam::new();

    for to_skip in is_cov {
        let mut flash = flash_algo_test::heap_flash::Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let init = mgr.app_boot_status(&mut flash, &mut scratch).await.unwrap();
        assert_eq!(init, AppBootStatus::Idle);

        let mut start = mgr
            .start(
                &mut flash,
                &mut scratch,
                chunk_size() as u32,
                data_frags.len() as u32,
            )
            .await
            .unwrap();

        assert_eq!(
            start,
            ActiveStatus {
                segment_size: chunk_size(),
                slot_size: SLOT_SIZE,
                firmware_slot_idx: 0,
                total_firmware_segments: data_frags.len() as u32,
                remaining_firmware_segments: data_frags.len() as u32,
                parity_slot_idx: 1,
                total_parity_segments: 16384,
                remaining_parity_segments: 16384,
            }
        );

        for (i, ch) in data_frags.iter().enumerate() {
            if i == to_skip {
                println!("SKIP {i}: {ch:?}");
                continue;
            }
            let res = start
                .write_segment(
                    &mut flash,
                    &mut scratch.firmware_rd_scratch,
                    i as u32 + 1,
                    ch,
                )
                .await
                .unwrap();
            assert_eq!(res, WriteSegmentOutcome::Consumed);
        }

        let mut winner = None;

        for (i, ch) in parity_frags.iter().enumerate() {
            let res = start
                .write_segment(
                    &mut flash,
                    &mut scratch.firmware_rd_scratch,
                    data_frags.len() as u32 + i as u32 + 1,
                    ch,
                )
                .await
                .unwrap();
            assert_eq!(res, WriteSegmentOutcome::ConsumedMaybeParity);
            winner = start.repair_step(&mut flash, &mut scratch).await.unwrap();
            if winner.is_some() {
                break;
            }
        }

        println!("{to_skip}: Recovered missing chunk {}", winner.unwrap());
        let mut scr = vec![0_u8; chunk_size()];
        let data_addr = DATA_REGION_OFFSET + (chunk_size() * to_skip);
        flash.read_to(data_addr, &mut scr).await.unwrap();
        assert_eq!(data_frags[to_skip].as_slice(), scr);
    }
}

#[tokio::test]
async fn could_it_recover() {
    const SLOT_SIZE: usize = 256 * 1024;

    // What is the coverage of each of the fragments?
    let data_frags = fw_with_hdr()
        .chunks(SEGMENT_SIZE_BYTES)
        .map(|s| s.to_vec())
        .collect::<Vec<_>>();
    let parity_chunks_ct = (data_frags.len() as f64 * 0.5).round() as usize;
    let parity_frags = generate_parity_rows(parity_chunks_ct);

    let mut covered = vec![0usize; data_frags.len()];
    for parity_idx in 0..parity_frags.len() {
        let mut row = BitCache::new();
        get_parity_matrix_row(parity_idx as u32 + 1, data_frags.len() as u32, &mut row);
        let row = row.iter().collect::<Vec<bool>>();
        covered.iter_mut().zip(row.iter()).for_each(|(c, p)| {
            if *p {
                *c += 1
            }
        });
    }

    // This is "how many parity frames touch this data frame"
    let pre_calc_coverage = [
        83, 118, 123, 115, 113, 108, 124, 120, 125, 126, 117, 106, 112, 127, 109, 136, 117, 117,
        113, 108, 119, 118, 113, 127, 117, 102, 124, 125, 96, 125, 125, 112, 104, 110, 112, 121,
        106, 112, 116, 112, 117, 116, 100, 114, 98, 141, 124, 114, 117, 113, 112, 118, 122, 114,
        117, 126, 96, 129, 122, 110, 115, 116, 110, 102, 112, 107, 108, 110, 112, 123, 127, 115,
        111, 105, 120, 111, 105, 121, 129, 119, 115, 98, 109, 107, 108, 117, 111, 119, 113, 96,
        116, 125, 118, 123, 112, 115, 117, 106, 118, 121, 101, 115, 114, 106, 126, 118, 104, 118,
        110, 118, 118, 121, 118, 101, 107, 120, 112, 114, 104, 117, 115, 110, 128, 126, 122, 109,
        124, 134, 116, 106, 119, 110, 118, 100, 122, 114, 103, 125, 111, 115, 122, 116, 110, 121,
        108, 117, 100, 118, 126, 103, 103, 121, 122, 113, 110, 112, 113, 133, 101, 105, 130, 132,
        110, 127, 107, 122, 119, 93, 129, 124, 133, 108, 111, 120, 117, 109, 109, 104, 114, 119,
        113, 123, 132, 111, 112, 117, 127, 107, 120, 115, 118, 107, 121, 95, 104, 102, 110, 109,
        118, 125, 113, 109, 110, 113, 111, 111, 109, 114, 131, 103, 113, 123, 126, 115, 118, 131,
        123, 117, 115, 116, 119, 126, 116, 115, 111, 116, 117, 115, 120, 123, 110, 119, 118, 111,
        119, 116, 103, 118, 113, 122, 107, 125, 98, 117, 120, 114, 117, 108, 121, 119, 104, 116,
        110, 118, 133, 116, 112, 128, 118, 104, 118, 109, 122, 116, 114, 103, 121, 123, 127, 113,
        113, 106, 116, 112, 122, 116, 116, 122, 112, 105, 118, 116, 105, 114, 116, 98, 121, 127,
        116, 104, 115, 120, 77, 96, 110, 119, 124, 124, 119, 121, 100, 115, 117, 118, 124, 109,
        115, 113, 112, 106, 120, 109, 127, 117, 112, 132, 94, 115, 119, 123, 128, 107, 111, 127,
        115, 102, 122, 126, 98, 128, 110, 125, 119, 111, 110, 130, 126, 113, 121, 111, 107, 118,
        119, 103, 120, 121, 122, 122, 124, 104, 111, 102, 105, 121, 121, 135, 124, 106, 118, 127,
        121, 114, 115, 122, 110, 103, 120, 103, 116, 117, 112, 115, 122, 116, 110, 110, 102, 117,
        116, 131, 105, 114, 119, 108, 104, 113, 110, 110, 116, 104, 98, 117, 124, 118, 114, 123,
        111, 128, 118, 125, 128, 105, 121, 112, 112, 101, 115, 122, 112, 127, 113, 118, 120, 111,
        110, 113, 113, 128, 104, 112, 117, 122, 106, 132, 119, 111, 125, 116, 108, 119, 113, 136,
        110, 120, 114, 104, 136, 108, 105, 114, 124, 119, 111, 136, 123, 112, 117, 119, 121, 105,
        109, 119, 116, 112, 108, 100, 111, 112, 118, 109, 106, 117, 104, 113, 131, 110, 130, 117,
        100, 117, 114, 124, 123, 109, 109, 111, 109, 113, 121, 115, 113, 117, 109, 108, 109, 116,
        134, 104, 129, 122, 119, 118, 118, 107, 116, 119, 113, 124, 121, 100, 120, 113, 123, 109,
        124, 111, 121, 111, 122, 110, 130, 119, 116, 124, 113, 122, 119, 95, 94, 126, 121, 111,
        113, 131, 113, 128, 125, 111, 131, 118, 118, 108, 111, 110, 105, 126, 111, 122, 122, 110,
        130, 118, 112, 104, 123, 116, 127, 115, 117, 125, 112, 104, 116, 109, 112, 125, 107, 132,
        110, 109, 105, 116, 111, 109, 112, 116, 117, 104, 123, 102, 113, 118, 119, 120, 111, 131,
        112, 109, 93, 109, 121, 105, 118, 108, 76,
    ];

    assert_eq!(covered, &pre_calc_coverage);
    assert_eq!(*covered.iter().min().unwrap(), 76);
    assert_eq!(*covered.iter().max().unwrap(), 141);

    fn all_pass(_: usize) -> bool {
        false
    }
    fn every_64_fail(i: usize) -> bool {
        i % 64 == 0
    }
    fn first_five_fail(i: usize) -> bool {
        i < 5
    }
    fn second_five_fail(i: usize) -> bool {
        (5..10).contains(&i)
    }
    fn every_16_fail(i: usize) -> bool {
        i % 16 == 0
    }
    fn every_16_fail_offset(i: usize) -> bool {
        ((i + 8) % 16) == 0
    }
    fn burst_20_fail(i: usize) -> bool {
        (100..120).contains(&i)
    }

    #[allow(clippy::complexity)]
    let cases: &[(fn(usize) -> bool, fn(usize) -> bool, bool, usize)] = &[
        (all_pass, all_pass, true, 0),
        (every_64_fail, all_pass, true, 29),
        (every_16_fail, all_pass, false, parity_frags.len()),
        (every_16_fail_offset, all_pass, false, parity_frags.len()),
        (first_five_fail, all_pass, true, 6),
        (second_five_fail, all_pass, true, 8),
        (burst_20_fail, all_pass, false, parity_frags.len()),
    ];

    let mut scratch = ScratchRam::new();

    for (test_i, (frag_skip_fn, parity_skip_fn, success, num_parity_needed)) in
        cases.iter().enumerate()
    {
        let mut flash = flash_algo_test::heap_flash::Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let init = mgr.app_boot_status(&mut flash, &mut scratch).await.unwrap();
        assert_eq!(init, AppBootStatus::Idle);

        let mut start = mgr
            .start(
                &mut flash,
                &mut scratch,
                chunk_size() as u32,
                data_frags.len() as u32,
            )
            .await
            .unwrap();

        assert_eq!(
            start,
            ActiveStatus {
                segment_size: chunk_size(),
                slot_size: SLOT_SIZE,
                firmware_slot_idx: 0,
                total_firmware_segments: data_frags.len() as u32,
                remaining_firmware_segments: data_frags.len() as u32,
                parity_slot_idx: 1,
                total_parity_segments: 16384,
                remaining_parity_segments: 16384,
            }
        );

        for (i, ch) in data_frags.iter().enumerate() {
            if frag_skip_fn(i) {
                println!("SKIP {i}");
                continue;
            }
            let res = start
                .write_segment(
                    &mut flash,
                    &mut scratch.firmware_rd_scratch,
                    i as u32 + 1,
                    ch,
                )
                .await
                .unwrap();
            assert!(matches!(
                res,
                WriteSegmentOutcome::Consumed | WriteSegmentOutcome::FirmwareComplete
            ));
        }

        let mut parity_used = 0;

        if !start.is_complete() {
            for (i, ch) in parity_frags.iter().enumerate() {
                if parity_skip_fn(i) {
                    println!("SKIP PARITY {i}");
                    continue;
                }

                parity_used += 1;

                let res = start
                    .write_segment(
                        &mut flash,
                        &mut scratch.firmware_rd_scratch,
                        data_frags.len() as u32 + i as u32 + 1,
                        ch,
                    )
                    .await
                    .unwrap();
                match res {
                    WriteSegmentOutcome::Consumed => todo!(),
                    WriteSegmentOutcome::ConsumedMaybeParity => 'repair: loop {
                        let rep_res = start.repair_step(&mut flash, &mut scratch).await.unwrap();
                        if let Some(idx) = rep_res {
                            println!("repaired {idx} after receiving parity {i}");
                        } else {
                            break 'repair;
                        }
                        if start.is_complete() {
                            break 'repair;
                        }
                    },
                    WriteSegmentOutcome::FirmwareComplete => break,
                }
            }
        }

        println!("{test_i}:");
        assert_eq!(*success, start.is_complete());
        assert_eq!(parity_used, *num_parity_needed);
        println!("---DONE---");
    }
}
