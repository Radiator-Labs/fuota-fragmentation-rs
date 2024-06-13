use crate::protocol::{
    BootOutcome, FlashRepr, Kind, NumberOfSegments, SegmentSize, SequenceNumber, SlotHeader,
    WriteExtStatus, WriteIntStatus,
};

/// # Panics
/// Panic if header conversion fails
pub fn make_a_slot(idx: u32, dst: &mut [u8]) -> SlotHeader {
    let len = dst.len();
    let hdr = SlotHeader {
        kind: Kind::Firmware,
        seq_no: SequenceNumber(idx),
        segment_size: SegmentSize(128),
        num_segments: NumberOfSegments(64),
        write_ext_status: WriteExtStatus::Complete,
        write_int_status: WriteIntStatus::Complete,
        boot_outcome: BootOutcome::Successful,
    };
    #[allow(clippy::unwrap_used)] // TODO: Eliminate unwrap()
    let used = len - hdr.write_to_bytes(dst).unwrap().len();
    assert_eq!(used, SlotHeader::SIZE);
    hdr
}
