#[allow(
    clippy::print_stdout,
    clippy::use_debug,
    clippy::unwrap_used,
    clippy::indexing_slicing
)]
#[cfg(test)]
pub mod test {
    use flash_algo::protocol::{
        BootOutcome, FlashRepr, Kind, NumberOfSegments, SegmentSize, SequenceNumber, SlotHeader,
        WriteExtStatus, WriteIntStatus,
    };
    use flash_algo_test::protocol_test_support::make_a_slot;
    #[test]
    fn header_layout() {
        //
        // If this test fails: you have reordered or changed the values of the header!
        // EXTREME CARE must be taken, as these values will (or are!) used by the factory
        // burned-in bootloader.
        //
        let mut scratch = [0_u8; SlotHeader::SIZE];
        let val = SlotHeader {
            kind: Kind::Firmware,
            seq_no: SequenceNumber(0x12_34_56_78),
            segment_size: SegmentSize(0x00_00_01_00),
            num_segments: NumberOfSegments(0x00_00_40_00),
            write_ext_status: WriteExtStatus::Complete,
            write_int_status: WriteIntStatus::Complete,
            boot_outcome: BootOutcome::Unsuccessful,
        };

        // (offset, size, value)
        let expected_contents: &[(usize, usize, &[u8])] = &[
            (
                SlotHeader::KIND_OFFSET,
                Kind::SIZE,
                &Kind::FIRMWARE.to_le_bytes(),
            ),
            (
                SlotHeader::SEQUENCE_NUMBER_OFFSET,
                SequenceNumber::SIZE,
                &0x1234_5678_u32.to_le_bytes(),
            ),
            (
                SlotHeader::SEGMENT_SIZE_OFFSET,
                SegmentSize::SIZE,
                &0x00_00_01_00_u32.to_le_bytes(),
            ),
            (
                SlotHeader::NUMBER_OF_SEGMENTS_OFFSET,
                NumberOfSegments::SIZE,
                &0x00_00_40_00_u32.to_le_bytes(),
            ),
            (
                SlotHeader::WRITE_EXT_STATUS_OFFSET,
                WriteExtStatus::SIZE,
                &WriteExtStatus::COMPLETE.to_le_bytes(),
            ),
            (
                SlotHeader::WRITE_INT_STATUS_OFFSET,
                WriteIntStatus::SIZE,
                &WriteIntStatus::COMPLETE.to_le_bytes(),
            ),
            (
                SlotHeader::BOOT_OUTCOME_OFFSET,
                BootOutcome::SIZE,
                &BootOutcome::UNSUCCESSFUL.to_le_bytes(),
            ),
        ];

        val.write_to_bytes(&mut scratch).unwrap();

        for &(offset, size, contents) in expected_contents {
            println!("offset: {offset}, size: {size}, contents: {contents:?}");
            let relevant = &scratch[offset..][..size];
            assert_eq!(relevant, contents);
        }

        insta::assert_snapshot!(&format!("{scratch:02X?}"));

        let (new_value, _) = SlotHeader::take_from_bytes(&scratch).unwrap();
        assert_eq!(val, new_value);
    }

    #[test]
    fn round_trip() {
        let mut scratch = [0_u8; 1024];
        let hdr = make_a_slot(123_456_789_u32, &mut scratch);

        let (hdr2, remain) = SlotHeader::take_from_bytes(&scratch).unwrap();
        let used = 1024 - remain.len();
        assert_eq!(used, SlotHeader::SIZE);
        assert_eq!(hdr, hdr2);
    }

    #[test]
    fn blank_flash_no_slot() {
        let scratch = [0xFF; SlotHeader::SIZE];
        assert_eq!(None, SlotHeader::take_from_bytes(&scratch));
    }
}
