//! This module tests the functional behavior of the firmware update process

#[allow(
    clippy::cast_possible_truncation,
    clippy::cast_lossless,
    clippy::items_after_statements,
    clippy::match_same_arms,
    clippy::print_stdout,
    clippy::shadow_reuse,
    clippy::shadow_unrelated,
    clippy::trivially_copy_pass_by_ref,
    clippy::uninlined_format_args,
    clippy::unreachable,
    clippy::unwrap_used,
    clippy::use_debug
)]
#[cfg(test)]
mod test {
    use core::unreachable;
    use insta::assert_snapshot;

    use flash_algo::{
        manager::{
            read_header_from_slot, ActiveStatus, AppBootStatus, OldestReport, ScratchRam,
            SlotManager,
        },
        protocol::{
            BootOutcome, FlashRepr, Kind, NumberOfSegments, SegmentSize, SequenceNumber,
            SlotHeader, WriteExtStatus, WriteIntStatus,
        },
        spi_flash::{SpiFlash, SpiFlashError},
    };
    use flash_algo_test::protocol_test_support::make_a_slot;

    #[tokio::test]
    async fn empty_flash() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();
        let mut flash = flash_algo_test::heap_flash::Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        assert_eq!(
            OldestReport {
                slot_idx: 0,
                next_seq_no: 0
            },
            mgr.find_oldest_slot(&mut flash, &mut scratch)
                .await
                .unwrap()
        );
    }

    #[tokio::test]
    async fn partial_empty() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();
        let mut flash = flash_algo_test::heap_flash::Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let mut scr = [0_u8; 1024];
        // 0, 1, 2, X
        for idx in 0..3 {
            #[allow(clippy::cast_possible_truncation)]
            make_a_slot(idx as u32, &mut scr);
            flash
                .write_from(SLOT_SIZE * idx, &scr[..SlotHeader::SIZE])
                .await
                .unwrap();

            let next = mgr
                .find_oldest_slot(&mut flash, &mut scratch)
                .await
                .unwrap();

            assert_eq!(
                next,
                OldestReport {
                    slot_idx: idx + 1,
                    next_seq_no: idx as u32 + 1
                }
            );
        }

        // 0, 1, 2, 3
        make_a_slot(3, &mut scr);
        flash
            .write_from(SLOT_SIZE * 3, &scr[..SlotHeader::SIZE])
            .await
            .unwrap();

        let next = mgr
            .find_oldest_slot(&mut flash, &mut scratch)
            .await
            .unwrap();
        assert_eq!(
            next,
            OldestReport {
                slot_idx: 0,
                next_seq_no: 4
            }
        );

        assert_snapshot!(flash.dump_to_string());
    }

    #[tokio::test]
    async fn basic_start() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();
        let mut flash = flash_algo_test::heap_flash::Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let init = mgr.app_boot_status(&mut flash, &mut scratch).await.unwrap();
        assert_eq!(init, AppBootStatus::Idle);

        let start = mgr.start(&mut flash, &mut scratch, 128, 64).await.unwrap();

        assert_eq!(
            start,
            ActiveStatus {
                segment_size: 128,
                slot_size: SLOT_SIZE,
                firmware_slot_idx: 0,
                total_firmware_segments: 64,
                remaining_firmware_segments: 64,
                parity_slot_idx: 1,
                total_parity_segments: 16384,
                remaining_parity_segments: 16384,
            }
        );

        let blank_to_start = flash.dump_to_string();
        assert_snapshot!(blank_to_start);
    }

    #[tokio::test]
    async fn start_write_all() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();
        let mut flash = flash_algo_test::heap_flash::Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let init = mgr.app_boot_status(&mut flash, &mut scratch).await.unwrap();
        assert_eq!(init, AppBootStatus::Idle);

        let mut start = mgr.start(&mut flash, &mut scratch, 128, 64).await.unwrap();

        assert_eq!(
            start,
            ActiveStatus {
                segment_size: 128,
                slot_size: SLOT_SIZE,
                firmware_slot_idx: 0,
                total_firmware_segments: 64,
                remaining_firmware_segments: 64,
                parity_slot_idx: 1,
                total_parity_segments: 16384,
                remaining_parity_segments: 16384,
            }
        );

        let blank_to_start = flash.dump_to_string();
        assert_snapshot!(blank_to_start);

        let mut rand_val = 1_234_567_890_u32;
        let mut bad_rand = || {
            let a = rand_val.wrapping_mul(31421);
            let b = a.wrapping_add(6927);
            let c = b.rotate_left(7);
            rand_val = c;
            b
        };

        let mut buf = [0_u8; 128];

        for i in 0..(64 + 32) {
            buf.iter_mut().for_each(|b| *b = bad_rand() as u8);
            start
                .write_segment(&mut flash, &mut scratch.firmware_rd_scratch, i + 1, &buf)
                .await
                .unwrap();
        }

        let wrote_all = flash.dump_to_string();
        assert_snapshot!(wrote_all.clone());

        // Reset RNG - write the SAME values to everything, make sure we're still
        // okay
        rand_val = 1_234_567_890_u32;
        let mut bad_rand = || {
            let a = rand_val.wrapping_mul(31421);
            let b = a.wrapping_add(6927);
            let c = b.rotate_left(7);
            rand_val = c;
            b
        };
        for i in 0..(64 + 32) {
            buf.iter_mut().for_each(|b| *b = bad_rand() as u8);
            start
                .write_segment(&mut flash, &mut scratch.firmware_rd_scratch, i + 1, &buf)
                .await
                .unwrap();
        }

        let wrote_all_2 = flash.dump_to_string();
        assert_eq!(wrote_all, wrote_all_2);

        // Writing more data fails
        let res = start
            .write_segment(
                &mut flash,
                &mut scratch.firmware_rd_scratch,
                1 + (64 + 16384),
                &buf,
            )
            .await;
        assert_eq!(res, Err(SpiFlashError::OutOfBounds));
    }

    #[allow(clippy::too_many_lines)]
    #[tokio::test]
    async fn right_boot_status() {
        const SLOT_SIZE: usize = 256 * 1024;

        let mut scratch = ScratchRam::new();

        // Sequence Number: 0-9/a-z like above
        // Firmware or Parity: f or p
        // status: `.`: complete, `~`: in progress, `!`: aborted
        let cases = [
            // Happy path "in progress" case
            (
                "0f. 1f~ 2p~ yf!", // Start
                "0f. 1f~ 2p~ yf!", // After
                // Status
                AppBootStatus::InProgress(ActiveStatus {
                    segment_size: 128,
                    slot_size: SLOT_SIZE,
                    firmware_slot_idx: 1,
                    parity_slot_idx: 2,
                    // TODO!!!!!
                    total_firmware_segments: 456,
                    remaining_firmware_segments: 456,
                    total_parity_segments: 456,
                    remaining_parity_segments: 456,
                }),
            ),
            // In progress around a wraparound
            (
                "2p~ yf! 0f. 1f~", // Start
                "2p~ yf! 0f. 1f~", // After
                // Status
                AppBootStatus::InProgress(ActiveStatus {
                    segment_size: 128,
                    slot_size: SLOT_SIZE,
                    firmware_slot_idx: 3,
                    parity_slot_idx: 0,
                    // TODO!!!!!
                    total_firmware_segments: 456,
                    remaining_firmware_segments: 456,
                    total_parity_segments: 456,
                    remaining_parity_segments: 456,
                }),
            ),
            // In progress with a seq num wraparound
            (
                "yf. 0f~ 1p~ xf!", // Start
                "yf. 0f~ 1p~ xf!", // After
                // Status
                AppBootStatus::InProgress(ActiveStatus {
                    segment_size: 128,
                    slot_size: SLOT_SIZE,
                    firmware_slot_idx: 1,
                    parity_slot_idx: 2,
                    // TODO!!!!!
                    total_firmware_segments: 456,
                    remaining_firmware_segments: 456,
                    total_parity_segments: 456,
                    remaining_parity_segments: 456,
                }),
            ),
            // Bad case (aliased seq no), DOES abort in progress
            // items
            (
                "0f. 1f~ 1p~ xf!", // Start
                "0f. 1f! 1p! xf!", // After
                // Status
                AppBootStatus::Idle,
            ),
        ];

        fn txt2arr(s: &str) -> [Option<SlotHeader>; 4] {
            const ONE: Option<SlotHeader> = None;
            let mut out = [ONE; 4];
            let s = s.split_whitespace().collect::<Vec<_>>();

            for (o, s) in out.iter_mut().zip(s) {
                let &[seq, kind, stat] = s.as_bytes() else {
                    assert_eq!(s, "N");
                    *o = None;
                    continue;
                };
                let seq_no = match seq {
                    v @ b'0'..=b'9' => {
                        // Treat single-digit numbers as u32s.
                        (v - b'0') as u32
                    }
                    v @ b'a'..=b'z' => {
                        // Treat lowercase letters as the last values before wraparound,
                        // so `z` == u32::MAX
                        let n_back = (b'z' - v) as u32;
                        u32::MAX - n_back
                    }
                    _ => unreachable!(),
                };
                let seq_no = SequenceNumber(seq_no);
                let kind = match kind {
                    b'f' => Kind::Firmware,
                    b'p' => Kind::Parity,
                    _ => unreachable!(),
                };
                let ext_status = match stat {
                    b'.' => WriteExtStatus::Complete,
                    b'!' => WriteExtStatus::Aborted,
                    b'~' => WriteExtStatus::InProgress,
                    _ => unreachable!(),
                };

                let int_status = match ext_status {
                    WriteExtStatus::InProgress => WriteIntStatus::InProgress,
                    WriteExtStatus::Aborted => WriteIntStatus::InProgress,
                    WriteExtStatus::Complete => WriteIntStatus::Complete,
                };

                let bootstat = match ext_status {
                    WriteExtStatus::InProgress => BootOutcome::Untested,
                    WriteExtStatus::Aborted => BootOutcome::Untested,
                    WriteExtStatus::Complete => BootOutcome::Successful,
                };

                *o = Some(SlotHeader {
                    kind,
                    seq_no,
                    segment_size: SegmentSize(128),
                    num_segments: NumberOfSegments(456),
                    write_ext_status: ext_status,
                    write_int_status: int_status,
                    boot_outcome: bootstat,
                });
            }

            out
        }

        // let mut scratch = Box::leak(Box::new([0_u8; 1024]));
        let mut flash = flash_algo_test::heap_flash::Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let mut scr = [0_u8; 1024];

        for (init, after, outcome) in cases {
            // println!("{init}");
            let init_cmp = txt2arr(init);

            flash.erase_all().await.unwrap();
            for (i, s) in init_cmp
                .iter()
                .enumerate()
                .filter_map(|(i, s)| s.as_ref().map(|s| (i, s)))
            {
                s.write_to_bytes(&mut scr).unwrap();
                flash
                    .write_from(SLOT_SIZE * i, &scr[..SlotHeader::SIZE])
                    .await
                    .unwrap();
            }

            let bs = mgr.app_boot_status(&mut flash, &mut scratch).await.unwrap();
            println!("{:?} {:?} {:?} {:?}", init, after, outcome, bs);
            assert_eq!(bs, outcome);

            // Now read the headers from flash, and see if they match the expected "after"
            let mut hdrs2: [Option<SlotHeader>; 4] = [None, None, None, None];

            for (i, slot) in hdrs2.iter_mut().enumerate() {
                let address = i * SLOT_SIZE;
                *slot = read_header_from_slot(&mut flash, &mut scratch, address)
                    .await
                    .unwrap();
            }

            let after = txt2arr(after);
            assert_eq!(hdrs2, after);
        }
    }

    #[tokio::test]
    async fn right_placement() {
        const SLOT_SIZE: usize = 256 * 1024;
        let mut scratch = ScratchRam::new();

        // CSpell:disable
        // (Current Status, Next Position, Next Sequence Number)
        let cases = [
            // Some (or all) slots are empty - the first empty slot is used
            (b"NNNN", 0, 0),
            (b"0NNN", 1, 1),
            (b"01NN", 2, 2),
            (b"012N", 3, 3),
            // Works the same whether you have/haven't erased the desired page
            (b"0123", 0, 4),
            (b"N123", 0, 4),
            (b"4123", 1, 5),
            (b"4N23", 1, 5),
            (b"4523", 2, 6),
            (b"45N3", 2, 6),
            (b"4563", 3, 7),
            (b"456N", 3, 7),
            (b"4567", 0, 8),
            (b"N567", 0, 8),
            // Getting close to the seq_no wrapping...
            (b"uvwx", 0, u32::MAX - 1),
            // There it goes!
            (b"yvwx", 1, 0),
            (b"y0wx", 2, 1),
            (b"y01x", 3, 2),
            (b"vwxy", 0, 0),
            // Also, we treat u32::MAX the same as "None"
            (b"zzzz", 0, 0),
            // What if there are two weird blank spots?
            // Show the "healing" progression
            (b"5NaN", 1, 6),
            (b"56aN", 2, 7),
            (b"567N", 3, 8),
            (b"5678", 0, 9),
            (b"9678", 1, 10),
            // Additional weirdo cases
            (b"5NNN", 1, 6),
            (b"N5NN", 2, 6),
            (b"NN5N", 3, 6),
            (b"NNN5", 0, 6),
            // Dupe index
            (b"233N", 2, 4),
        ];
        // CSpell:enable

        // Use text instead of `[None, None, None, Some(4)]` or so to make
        // it more readable
        fn txt2arr(s: &[u8; 4]) -> [Option<u32>; 4] {
            let mut out = [None; 4];
            s.iter().zip(out.iter_mut()).for_each(|(s, o)| {
                *o = match *s {
                    b'N' => {
                        // N => None, same as a totally erased flash page
                        None
                    }
                    v @ b'0'..=b'9' => {
                        // Treat single-digit numbers as u32s.
                        Some((v - b'0') as u32)
                    }
                    v @ b'a'..=b'z' => {
                        // Treat lowercase letters as the last values before wraparound,
                        // so `z` == u32::MAX
                        let n_back = (b'z' - v) as u32;
                        let idx = u32::MAX - n_back;
                        Some(idx)
                    }
                    _ => unreachable!(),
                }
            });
            out
        }

        let mut flash = flash_algo_test::heap_flash::Flash::new(64 * 1024, 1024 * 1024);
        let mut mgr = SlotManager::<4>::new(SLOT_SIZE);

        let mut scr = [0_u8; 1024];

        for (state, next_idx, next_seq) in cases {
            print!("{} - ", core::str::from_utf8(state).unwrap());

            let arr = txt2arr(state);
            // println!("{:08X?}", arr);
            flash.erase_all().await.unwrap();
            for (i, s) in arr
                .into_iter()
                .enumerate()
                .filter_map(|(i, s)| s.map(|s| (i, s)))
            {
                make_a_slot(s, &mut scr);
                flash
                    .write_from(SLOT_SIZE * i, &scr[..SlotHeader::SIZE])
                    .await
                    .unwrap();
            }

            let rpt = mgr
                .find_oldest_slot(&mut flash, &mut scratch)
                .await
                .unwrap();
            // println!("{rpt:?}");
            assert_eq!(
                rpt,
                OldestReport {
                    slot_idx: next_idx,
                    next_seq_no: next_seq
                }
            );
        }
    }
}
