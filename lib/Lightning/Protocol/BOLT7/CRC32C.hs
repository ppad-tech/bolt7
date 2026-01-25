{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT7.CRC32C
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- CRC-32C (Castagnoli) implementation for BOLT #7 checksums.
--
-- This is an internal helper module implementing CRC-32C as specified
-- in RFC 3720. CRC-32C uses the Castagnoli polynomial 0x1EDC6F41.

module Lightning.Protocol.BOLT7.CRC32C (
    crc32c
  ) where

import Data.Bits (shiftR, xor, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)

-- | CRC-32C polynomial (Castagnoli): 0x1EDC6F41 reflected = 0x82F63B78
crc32cPoly :: Word32
crc32cPoly = 0x82F63B78
{-# INLINE crc32cPoly #-}

-- | Compute CRC-32C of a bytestring.
--
-- >>> crc32c "123456789"
-- 0xe3069283
crc32c :: ByteString -> Word32
crc32c = xor 0xFFFFFFFF . BS.foldl' updateByte 0xFFFFFFFF
{-# INLINE crc32c #-}

-- | Update CRC with a single byte.
updateByte :: Word32 -> Word8 -> Word32
updateByte !crc !byte =
  let crc' = crc `xor` fromIntegral byte
  in  go 8 crc'
  where
    go :: Int -> Word32 -> Word32
    go 0 !c = c
    go !n !c =
      let c' = if c .&. 1 /= 0
               then (c `shiftR` 1) `xor` crc32cPoly
               else c `shiftR` 1
      in  go (n - 1) c'
{-# INLINE updateByte #-}
