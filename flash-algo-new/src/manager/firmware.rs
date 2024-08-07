use crc::{Crc, CRC_32_CKSUM};

use crate::spi_flash::SpiFlash;

use super::{
    layout::{
        segment_status_table::{MAX_SEGMENTS, MAX_SEGMENT_SIZE},
        Crc32, FlashRepr, Kind, Signature, SlotHeader, WriteExtStatus, DATA_REGION_OFFSET,
    },
    ManagerError, ScratchRam, Slot,
};

impl Slot {
    /// Check whether the slot contains a valid firmware image
    ///
    /// # Errors
    /// Reports if SPI Flash activities fail
    pub async fn is_valid_firmware<T: SpiFlash>(
        &self,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<(), ManagerError<T::Error>> {
        let header = self
            .load_header(flash, scratch)
            .await?
            .ok_or(ManagerError::UnexpectedMissingHeader)?;

        if header.kind != Kind::Firmware {
            return Err(ManagerError::CheckFailNotFirmware);
        }
        if header.write_ext_status != WriteExtStatus::Complete {
            return Err(ManagerError::CheckFailNotDone);
        }

        self.crc_valid(&header, flash, scratch).await
    }

    /// Check whether the CRC and signature
    /// are valid, assuming the slot contains firmware
    pub(crate) async fn crc_valid<T: SpiFlash>(
        &self,
        header: &SlotHeader,
        flash: &mut T,
        scratch: &mut ScratchRam,
    ) -> Result<(), ManagerError<T::Error>> {
        let segments = header.num_segments.0 as usize;
        let segment_size = header.segment_size.0 as usize;

        if segments > MAX_SEGMENTS {
            return Err(ManagerError::TooManySegments);
        }
        if segment_size > MAX_SEGMENT_SIZE {
            return Err(ManagerError::SegmentsTooLarge);
        }

        // Now read the payload header
        flash
            .read_to(
                self.idx * self.size + DATA_REGION_OFFSET,
                &mut scratch.firmware_rd_scratch[..Crc32::SIZE + Signature::SIZE],
            )
            .await?;
        let (expected_crc32, later) =
            Crc32::take_from_bytes(&scratch.firmware_rd_scratch).ok_or(ManagerError::Fatal)?;
        let (_expected_sig, _) = Signature::take_from_bytes(later).ok_or(ManagerError::Fatal)?;

        // When calculating the CRC32, we need to skip the prepended
        // CRC32 and signature for calculations.
        //
        // TODO: Phil - you'll need to calculate the ed25519 signature over
        // the same payload bytes as the CRC32 here, and check it against
        // the `_expected_sig` variable we decoded above!
        let mut skip_remain = Some(Crc32::SIZE + Signature::SIZE);
        let calc_crc = Crc::<u32>::new(&CRC_32_CKSUM);
        let mut digest = calc_crc.digest();

        // Take a properly sized scratch buffer for our segment sizes
        #[allow(clippy::indexing_slicing)]
        let segment_buf = &mut scratch.firmware_rd_scratch[..segment_size];

        for idx in 0..segments {
            // Decide if any header bytes still need to be skipped
            #[allow(clippy::indexing_slicing)]
            let to_skip = match skip_remain.take() {
                // Nope, no remaining header bytes!
                None => 0,
                // Yes, AND it's larger than a whole segment, so just skip
                // reading the whole segment, and decrement by the number
                // to skip by a whole segment
                Some(n) if n >= segment_size => {
                    skip_remain = Some(n - segment_size);
                    continue;
                }
                // Yes, and it's NOT larger than a whole segment, so just
                // skip the remaining bytes (and don't store back any
                // more to be skipped). It's okay if `n == 0` at this point.
                Some(n) => n,
            };

            let data_start = self.idx * self.size + DATA_REGION_OFFSET + (idx * segment_size);

            flash.read_to(data_start, segment_buf).await?;
            #[allow(clippy::indexing_slicing)]
            digest.update(&segment_buf[to_skip..]);
        }

        let calc_crc32 = digest.finalize();

        if expected_crc32.0 == calc_crc32 {
            Ok(())
        } else {
            #[cfg(feature = "defmt")]
            defmt::error!(
                "Crc32Mismatch expected {=u32:x} calc {=u32:x}",
                expected_crc32.0,
                calc_crc32
            );

            Err(ManagerError::Crc32Mismatch)
        }
    }
}
