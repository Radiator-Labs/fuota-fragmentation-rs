//! Flash File Format
//!
//! This file contains the data types and specification of data written to the
//! external flash.
//!
//! ## Slots
//!
//! The external flash is broken up into N "slots". Each slot contains the following
//! contents:
//!
//! * A 1KiB `[SlotHeader]`, which contains metadata (detailed below)
//! * A 16KiB Segment Status Table, which notes whether we HAVE or HAVEN'T received
//!   a segment
//! * The remaining size of the Slot consists of data Segments
//!
//! The size of each slot is constant, and decided at compile time.
//!
//! ## Segments
//!
//! As we will receive data over the air in small chunks, we divide the slot into
//! segments, each the size of the packet we will receive.
//!
//! For example: If we transfer 64KiB in 128B segments, we will have a total of
//! 512 segments
//!
//! Each segment has a 1-byte field in the Segment Status Table to note whether
//! the segment has been received or not. This is done so that all data segments
//! can be stored contiguously in memory.
//!
//! Segments are variably sized, and can differ from one transfer to another.
//! Note that the fixed 16KiB Segment Status Table means that we can only support
//! up to 16K segments, meaning that the max size of a transfer may be limited
//! by both the Slot Size and Segment Size. For example with 11B segments, a
//! maximum payload of 176KiB is supported, or with 200B segments, a maximum payload
//! of 3.1MiB is supported.
//!
//! ## Firmware Slots and Parity Slots
//!
//! As we support the Forward Error Correction scheme used by `LoRaWAN`, we will be
//! receiving additional parity segments, in addition to the firmware segments.
//!
//! For example, a firmware may consist of 512x 128B data segments, and an additional
//! 64x 128B parity segments, allowing us to typically recover from up to 64 missed
//! packets over the air.
//!
//! In this protocol, we actually store the DATA segments and PARITY segments in
//! separate slots, in order to allow slots to have as many slots as possible. As
//! parity data is not needed after a transfer is complete, these slots can be
//! erased and reused once the transfer is complete.
//!
//! ## Slot Headers
//!
//! Most slot header data is written at the START of a transfer. This defines details
//! such as the segment size, number of segments, etc. Other data such as the status
//! field are only written at the COMPLETION (successful or otherwise) of a transfer.
//!
//! Values used in the Slot Headers are chosen so that the default flash erase value
//! of 0xFF means "not done", or "in progress", to avoid confusion.
//!
//! Most values are stored as either a `u32`, or a `u8`. Values are always stored
//! little-endian.

#[allow(clippy::exhaustive_enums)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[derive(Debug, PartialEq)]
pub enum FlashReprError {
    /// We ran out of bytes to write to
    EndOfBuffer,
}

/// This trait is basically `Serialize + Deserialize` in serde terms.
///
/// This is done to ensure we control the size, placement, and endianness of data.
pub trait FlashRepr: Sized {
    /// The size of the item when in "wire form" on flash
    const SIZE: usize;
    /// Attempt to obtain Self from `bytes`.
    ///
    /// Analogous to `DeserializeOwned`. When successful, returns Self and the remaining,
    /// unread bytes.
    fn take_from_bytes(bytes: &[u8]) -> Option<(Self, &[u8])>;
    /// Attempt to write Self to `buf`.
    ///
    /// Analogous to `Serialize`. When successful, returns the remaining unwritten bytes.
    ///
    /// # Errors
    /// Reports error if unable to write to buffer
    #[allow(clippy::result_unit_err)]
    fn write_to_bytes<'a>(&self, buf: &'a mut [u8]) -> Result<&'a mut [u8], FlashReprError>;
}

/// Slot header
///
/// This represents the in-memory version of the Slot Header. It may be serialized
/// or deserialized via `[FlashRepr]` trait methods.
///
/// # DANGER
///
/// If you add/remove fields, you MUST update the `FlashRepr`!
#[allow(clippy::exhaustive_structs)]
#[derive(Debug, PartialEq)]
pub struct SlotHeader {
    /// What KIND of slot is this? Firmware or Parity?
    pub kind: Kind,
    /// What is the sequence number of this slot? This is used to determine which
    /// slot is "oldest" and should be recycled
    pub seq_no: SequenceNumber,
    /// What is the size of EACH segment?
    pub segment_size: SegmentSize,
    /// This could be number of FIRMWARE segments (if this is `[Kind::Firmware]`),
    /// OR it could be number of PARITY segments (if this is `[Kind::Parity]`).
    pub num_segments: NumberOfSegments,
    /// What is the current status of the transfer? Typically only written at the
    /// completion of transfer.
    pub write_ext_status: WriteExtStatus,
    /// Loading by Bootloader
    pub write_int_status: WriteIntStatus,
    /// Boot status
    pub boot_outcome: BootOutcome,
}

/// Slot Kind
#[allow(clippy::exhaustive_enums)]
#[derive(Debug, PartialEq)]
pub enum Kind {
    /// Firmware Slot
    Firmware,
    /// Parity Slot
    Parity,
}

/// Sequence Number of the Slot
///
/// Wrapper type to allow for trait impls and potential future type changes
#[allow(clippy::exhaustive_structs)]
#[derive(Debug, PartialEq)]
pub struct SequenceNumber(pub u32);

/// Status of the Slot
#[allow(clippy::exhaustive_enums)]
#[derive(Debug, PartialEq)]
pub enum WriteExtStatus {
    /// A transfer has started but not completed
    InProgress,
    /// A transfer has started but failed, and the data contents are in an
    /// unspecified state.
    Aborted,
    /// A transfer has started and succeeded
    Complete,
}

/// Status of the Slot
#[allow(clippy::exhaustive_enums)]
#[derive(Debug, PartialEq)]
pub enum WriteIntStatus {
    /// A transfer has started but not completed
    InProgress,
    /// A transfer has started and succeeded
    Complete,
}

/// The size of each segment in this slot
///
/// Wrapper type to allow for trait impls and potential future type changes
#[allow(clippy::exhaustive_structs)]
#[derive(Debug, PartialEq)]
pub struct SegmentSize(pub u32);

/// How many segments are in this slot?
///
/// Wrapper type to allow for trait impls and potential future type changes
///
/// NOTE: MUST be less than [`segment_status_table::MAX_SEGMENTS`]
#[allow(clippy::exhaustive_structs)]
#[derive(Debug, PartialEq)]
pub struct NumberOfSegments(pub u32);

/// Placeholder cryptographic signature
// TODO: "Clone" is questionable here
#[allow(clippy::exhaustive_structs)]
#[derive(Debug, PartialEq, Clone)]
pub struct Signature(pub [u8; 64]);

/// A CRC32 signature
// TODO: "Clone" is questionable here
#[allow(clippy::exhaustive_structs)]
#[derive(Debug, PartialEq, Clone)]
pub struct Crc32(pub u32);

/// Have we attempted to boot this before?
#[allow(clippy::exhaustive_enums)]
#[derive(Debug, PartialEq)]
pub enum BootOutcome {
    Untested,
    Successful,
    Unsuccessful,
}

/// Stuff for the Segment Status Table
pub mod segment_status_table {
    /// Maximum number of segments of either a firmware or parity slot
    pub const MAX_SEGMENTS: usize = 16 * 1024;
    /// Maximum size in bytes of a segment
    pub const MAX_SEGMENT_SIZE: usize = 256;
    /// Value denoting "segment not received"
    pub const DATA_NOT_WRITTEN: u8 = 0xFF;
    /// Value denoting "segment received"
    pub const DATA_WRITTEN: u8 = 0x33;
}

// Helper function that is basically like `split_at` but returns an option
// instead of panicking if `n` is out of bounds.
#[inline]
pub(crate) fn try_take_n(bytes: &[u8], n: usize) -> Option<(&[u8], &[u8])> {
    (bytes.len() >= n).then(|| bytes.split_at(n))
}

/// Helper function to specifically take a `u32` in a little endian fashion
/// # Errors
/// Can report an error if flash write operation fails
#[inline]
#[must_use]
pub(crate) fn try_take_u32(bytes: &[u8]) -> Option<(u32, &[u8])> {
    let (now, later) = try_take_n(bytes, 4)?;
    let mut u32_bytes = [0_u8; 4];
    u32_bytes.copy_from_slice(now);
    let val = u32::from_le_bytes(u32_bytes);
    Some((val, later))
}

/// Helper function to split off and write a chunk of data.
///
/// Like `split_at_mut()` + `copy_from_slice()`, but doesn't panic if `data` is
/// larger than `buf`.
#[inline]
pub(crate) fn try_write_n<'a>(
    data: &[u8],
    buf: &'a mut [u8],
) -> Result<&'a mut [u8], FlashReprError> {
    if buf.len() < data.len() {
        return Err(FlashReprError::EndOfBuffer);
    }
    let (now, later) = buf.split_at_mut(data.len());
    now.copy_from_slice(data);
    Ok(later)
}

/// Helper function to specifically write a `u32` in little endian fashion
/// # Errors
/// Can report an error if flash write operation fails
#[inline]
pub(crate) fn try_write_u32(val: u32, buf: &mut [u8]) -> Result<&mut [u8], FlashReprError> {
    let bytes = val.to_le_bytes();
    try_write_n(&bytes, buf)
}

// ////////////////////////////////////////////////////////
// FlashRepr impls
//

impl Kind {
    pub const FIRMWARE: u32 = 0;
    pub const PARITY: u32 = 1;
}

impl FlashRepr for Kind {
    const SIZE: usize = 4;

    fn take_from_bytes(bytes: &[u8]) -> Option<(Self, &[u8])> {
        match try_take_u32(bytes)? {
            (Self::FIRMWARE, later) => Some((Kind::Firmware, later)),
            (Self::PARITY, later) => Some((Kind::Parity, later)),
            _ => None,
        }
    }

    fn write_to_bytes<'a>(&self, buf: &'a mut [u8]) -> Result<&'a mut [u8], FlashReprError> {
        match *self {
            Kind::Firmware => try_write_u32(Self::FIRMWARE, buf),
            Kind::Parity => try_write_u32(Self::PARITY, buf),
        }
    }
}

impl SequenceNumber {
    const INVALID: u32 = u32::MAX;
}

impl FlashRepr for SequenceNumber {
    const SIZE: usize = 4;

    fn take_from_bytes(bytes: &[u8]) -> Option<(Self, &[u8])> {
        match try_take_u32(bytes)? {
            (Self::INVALID, _) => None,
            (n, later) => Some((SequenceNumber(n), later)),
        }
    }

    fn write_to_bytes<'a>(&self, buf: &'a mut [u8]) -> Result<&'a mut [u8], FlashReprError> {
        try_write_u32(self.0, buf)
    }
}

impl WriteExtStatus {
    pub const IN_PROGRESS: u32 = u32::MAX;
    pub const ABORTED: u32 = 0xAAAA_AAAA;
    pub const COMPLETE: u32 = 0x4444_4444;
}

impl FlashRepr for WriteExtStatus {
    const SIZE: usize = 4;

    fn take_from_bytes(bytes: &[u8]) -> Option<(Self, &[u8])> {
        match try_take_u32(bytes)? {
            (Self::IN_PROGRESS, later) => Some((WriteExtStatus::InProgress, later)),
            (Self::ABORTED, later) => Some((WriteExtStatus::Aborted, later)),
            (Self::COMPLETE, later) => Some((WriteExtStatus::Complete, later)),
            _ => None,
        }
    }

    fn write_to_bytes<'a>(&self, buf: &'a mut [u8]) -> Result<&'a mut [u8], FlashReprError> {
        let val = match *self {
            WriteExtStatus::InProgress => Self::IN_PROGRESS,
            WriteExtStatus::Aborted => Self::ABORTED,
            WriteExtStatus::Complete => Self::COMPLETE,
        };
        try_write_u32(val, buf)
    }
}

impl WriteIntStatus {
    pub const IN_PROGRESS: u32 = u32::MAX;
    pub const COMPLETE: u32 = 0x1111_1111;
}

impl FlashRepr for WriteIntStatus {
    const SIZE: usize = 4;

    fn take_from_bytes(bytes: &[u8]) -> Option<(Self, &[u8])> {
        match try_take_u32(bytes)? {
            (Self::IN_PROGRESS, later) => Some((WriteIntStatus::InProgress, later)),
            (Self::COMPLETE, later) => Some((WriteIntStatus::Complete, later)),
            _ => None,
        }
    }

    fn write_to_bytes<'a>(&self, buf: &'a mut [u8]) -> Result<&'a mut [u8], FlashReprError> {
        let val = match *self {
            WriteIntStatus::InProgress => Self::IN_PROGRESS,
            WriteIntStatus::Complete => Self::COMPLETE,
        };
        try_write_u32(val, buf)
    }
}

impl FlashRepr for SegmentSize {
    const SIZE: usize = 4;

    fn take_from_bytes(bytes: &[u8]) -> Option<(Self, &[u8])> {
        let (val, later) = try_take_u32(bytes)?;
        let max_segment_size_u32 = segment_status_table::MAX_SEGMENT_SIZE.try_into().ok()?;
        // The Segment Size is only valid if it is nonzero and within
        // the maximum value.
        let good = (val > 0) && (val <= max_segment_size_u32);
        good.then_some((SegmentSize(val), later))
    }

    fn write_to_bytes<'a>(&self, buf: &'a mut [u8]) -> Result<&'a mut [u8], FlashReprError> {
        try_write_u32(self.0, buf)
    }
}

impl FlashRepr for NumberOfSegments {
    const SIZE: usize = 4;

    fn take_from_bytes(bytes: &[u8]) -> Option<(Self, &[u8])> {
        let (val, later) = try_take_u32(bytes)?;
        let max_segments_u32 = segment_status_table::MAX_SEGMENTS.try_into().ok()?;
        // The Number of Segments is only valid if it is nonzero and within
        // the maximum value.
        let good = (val > 0) && (val <= max_segments_u32);
        good.then_some((NumberOfSegments(val), later))
    }

    fn write_to_bytes<'a>(&self, buf: &'a mut [u8]) -> Result<&'a mut [u8], FlashReprError> {
        try_write_u32(self.0, buf)
    }
}

impl FlashRepr for Signature {
    const SIZE: usize = 64;

    fn take_from_bytes(bytes: &[u8]) -> Option<(Self, &[u8])> {
        let (sli, later) = try_take_n(bytes, Self::SIZE)?;
        let mut buf = [0_u8; Self::SIZE];
        buf.copy_from_slice(sli);
        Some((Signature(buf), later))
    }

    fn write_to_bytes<'a>(&self, buf: &'a mut [u8]) -> Result<&'a mut [u8], FlashReprError> {
        try_write_n(&self.0, buf)
    }
}

impl FlashRepr for Crc32 {
    const SIZE: usize = 4;

    fn take_from_bytes(bytes: &[u8]) -> Option<(Self, &[u8])> {
        let (val, later) = try_take_u32(bytes)?;
        Some((Crc32(val), later))
    }

    fn write_to_bytes<'a>(&self, buf: &'a mut [u8]) -> Result<&'a mut [u8], FlashReprError> {
        try_write_u32(self.0, buf)
    }
}

impl BootOutcome {
    pub const UNTESTED: u32 = u32::MAX;
    pub const SUCCESSFUL: u32 = 0xAB_CD_12_34;
    pub const UNSUCCESSFUL: u32 = 0xCD_EF_78_90;
}

impl FlashRepr for BootOutcome {
    const SIZE: usize = 4;

    fn take_from_bytes(bytes: &[u8]) -> Option<(Self, &[u8])> {
        let (val, later) = try_take_u32(bytes)?;
        match val {
            Self::UNTESTED => Some((Self::Untested, later)),
            Self::SUCCESSFUL => Some((Self::Successful, later)),
            Self::UNSUCCESSFUL => Some((Self::Unsuccessful, later)),
            _ => None,
        }
    }

    fn write_to_bytes<'a>(&self, buf: &'a mut [u8]) -> Result<&'a mut [u8], FlashReprError> {
        let val = match *self {
            BootOutcome::Untested => BootOutcome::UNTESTED,
            BootOutcome::Successful => BootOutcome::SUCCESSFUL,
            BootOutcome::Unsuccessful => BootOutcome::UNSUCCESSFUL,
        };
        try_write_u32(val, buf)
    }
}

#[allow(dead_code)]
impl SlotHeader {
    pub const KIND_OFFSET: usize = 0;
    pub const SEQUENCE_NUMBER_OFFSET: usize = 4;
    pub const SEGMENT_SIZE_OFFSET: usize = 8;
    pub const NUMBER_OF_SEGMENTS_OFFSET: usize = 12;
    pub const WRITE_EXT_STATUS_OFFSET: usize = 16;
    pub const WRITE_INT_STATUS_OFFSET: usize = 20;
    pub const BOOT_OUTCOME_OFFSET: usize = 24;
}

impl FlashRepr for SlotHeader {
    const SIZE: usize = (Kind::SIZE
        + SequenceNumber::SIZE
        + SegmentSize::SIZE
        + NumberOfSegments::SIZE
        + WriteExtStatus::SIZE
        + WriteIntStatus::SIZE
        + BootOutcome::SIZE);

    #[allow(clippy::shadow_reuse)] // TODO: eliminate this allow
    fn take_from_bytes(bytes: &[u8]) -> Option<(Self, &[u8])> {
        let (kind, later) = Kind::take_from_bytes(bytes)?;
        let (seq_no, later) = SequenceNumber::take_from_bytes(later)?;
        let (segment_size, later) = SegmentSize::take_from_bytes(later)?;
        let (num_segments, later) = NumberOfSegments::take_from_bytes(later)?;
        let (write_ext_status, later) = WriteExtStatus::take_from_bytes(later)?;
        let (write_int_status, later) = WriteIntStatus::take_from_bytes(later)?;
        let (boot_status, later) = BootOutcome::take_from_bytes(later)?;

        let me = Self {
            kind,
            seq_no,
            segment_size,
            num_segments,
            write_ext_status,
            write_int_status,
            boot_outcome: boot_status,
        };

        Some((me, later))
    }

    #[allow(clippy::shadow_reuse)] // TODO: eliminate this allow
    fn write_to_bytes<'a>(&self, buf: &'a mut [u8]) -> Result<&'a mut [u8], FlashReprError> {
        let later = Kind::write_to_bytes(&self.kind, buf)?;
        let later = SequenceNumber::write_to_bytes(&self.seq_no, later)?;
        let later = SegmentSize::write_to_bytes(&self.segment_size, later)?;
        let later = NumberOfSegments::write_to_bytes(&self.num_segments, later)?;
        let later = WriteExtStatus::write_to_bytes(&self.write_ext_status, later)?;
        let later = WriteIntStatus::write_to_bytes(&self.write_int_status, later)?;
        let later = BootOutcome::write_to_bytes(&self.boot_outcome, later)?;
        Ok(later)
    }
}

#[allow(clippy::exhaustive_enums)]
#[derive(Debug, PartialEq)]
pub enum TotalStatus {
    // WriteExt: InProgress, WriteInt: InProgress, Outcome: Untested
    // NOTE: Must HAVE-FFFF_FFFF slot ID
    BlankSlot,
    // WriteExt: InProgress, WriteInt: InProgress, Outcome: Untested
    // NOTE: Must have NON-FFFF_FFFF slot ID
    AppWriteInProgress,
    // WriteExt: Aborted, WriteInt: InProgress, Outcome: Untested
    AppWriteAborted,
    // WriteExt: Complete, WriteInt: InProgress, Outcome: Untested
    BootloadWriteInProgress,
    // WriteExt: Complete, WriteInt: Complete, Outcome: Untested
    FirstBootPendingAck,
    // WriteExt: Complete, WriteInt: Complete, Outcome: Successful
    ConfirmedImage,
    // WriteExt: Complete, WriteInt: Complete, Outcome: Unsuccessful
    RejectedImage,

    // Any Other Status
    InvalidNeedsErase,
}

#[must_use]
pub(crate) fn total_status(hdr: &SlotHeader) -> TotalStatus {
    let slot_id_valid = hdr.seq_no.0 != u32::MAX;

    #[allow(clippy::pattern_type_mismatch)] // TODO: eliminate this allow
    match (
        slot_id_valid,
        &hdr.write_ext_status,
        &hdr.write_int_status,
        &hdr.boot_outcome,
    ) {
        // All default values
        (false, WriteExtStatus::InProgress, WriteIntStatus::InProgress, BootOutcome::Untested) => {
            TotalStatus::BlankSlot
        }

        // WriteExt: InProgress, WriteInt: InProgress, Outcome: Untested
        // NOTE: Must have NON-FFFF_FFFF slot ID
        (true, WriteExtStatus::InProgress, WriteIntStatus::InProgress, BootOutcome::Untested) => {
            TotalStatus::AppWriteInProgress
        }

        // WriteExt: Aborted, WriteInt: InProgress, Outcome: Untested
        (true, WriteExtStatus::Aborted, WriteIntStatus::InProgress, BootOutcome::Untested) => {
            TotalStatus::AppWriteAborted
        }

        // WriteExt: Complete, WriteInt: InProgress, Outcome: Untested
        (true, WriteExtStatus::Complete, WriteIntStatus::InProgress, BootOutcome::Untested) => {
            TotalStatus::BootloadWriteInProgress
        }

        // WriteExt: Complete, WriteInt: Complete, Outcome: Untested
        (true, WriteExtStatus::Complete, WriteIntStatus::Complete, BootOutcome::Untested) => {
            TotalStatus::FirstBootPendingAck
        }

        // WriteExt: Complete, WriteInt: Complete, Outcome: Successful
        (true, WriteExtStatus::Complete, WriteIntStatus::Complete, BootOutcome::Successful) => {
            TotalStatus::ConfirmedImage
        }

        // WriteExt: Complete, WriteInt: Complete, Outcome: Unsuccessful
        (true, WriteExtStatus::Complete, WriteIntStatus::Complete, BootOutcome::Unsuccessful) => {
            TotalStatus::RejectedImage
        }

        // Any Other Status
        _ => TotalStatus::InvalidNeedsErase,
    }
}

#[cfg(test)]
pub mod test {
    // NOTE: Unwraps are idiomatic in tests
    #![allow(clippy::unwrap_used)]

    use super::{
        segment_status_table::{MAX_SEGMENTS, MAX_SEGMENT_SIZE},
        *,
    };

    #[test]
    fn sanity_check_max_size() {
        let mss_u32 = u32::try_from(MAX_SEGMENT_SIZE).unwrap();
        let bad_sizes: &[u32] = &[0, mss_u32 + 1];

        for bad in bad_sizes {
            let val = (*bad).to_le_bytes();
            let res = SegmentSize::take_from_bytes(&val);
            assert!(res.is_none());
        }

        for good in 1..=mss_u32 {
            let val = good.to_le_bytes();
            let res = SegmentSize::take_from_bytes(&val);
            assert!(res.is_some());
        }
    }

    #[test]
    fn sanity_check_max_segments() {
        let ms_u32 = u32::try_from(MAX_SEGMENTS).unwrap();
        let bad_sizes: &[u32] = &[0, ms_u32 + 1];

        for bad in bad_sizes {
            let val = (*bad).to_le_bytes();
            let res = NumberOfSegments::take_from_bytes(&val);
            assert!(res.is_none());
        }

        for good in 1..=ms_u32 {
            let val = good.to_le_bytes();
            let res = NumberOfSegments::take_from_bytes(&val);
            assert!(res.is_some());
        }
    }
}
