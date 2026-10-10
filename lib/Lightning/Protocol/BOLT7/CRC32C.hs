{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT7.CRC32C
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- CRC-32C (Castagnoli), per RFC 3720, for @channel_update@ checksums.

module Lightning.Protocol.BOLT7.CRC32C (
    crc32c
  ) where

import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8, Word32)

-- | The CRC-32C of a byte string.
crc32c :: BS.ByteString -> Word32
crc32c = complement . BS.foldl' step 0xFFFFFFFF
  where
    step !c !b = (c `shiftR` 8) `xor` entry (fromIntegral c `xor` b)
{-# INLINE crc32c #-}

-- the table entry for a byte, read from 'table'
entry :: Word8 -> Word32
entry i =
  let !o = 4 * fromIntegral i  -- at most 1020, within the 1024-byte table
      byte k = fromIntegral (BU.unsafeIndex table (o + k)) :: Word32
  in  byte 0 .|. (byte 1 `shiftL` 8) .|. (byte 2 `shiftL` 16)
        .|. (byte 3 `shiftL` 24)
{-# INLINE entry #-}

-- the 256 table entries, little-endian
table :: BS.ByteString
table = fst (BS.unfoldrN 1024 byte 0)
  where
    byte :: Int -> Maybe (Word8, Int)
    byte k = Just
      (fromIntegral (go (8 :: Int) (fromIntegral (k `shiftR` 2))
                       `shiftR` (8 * (k .&. 3))), k + 1)
    -- the Castagnoli polynomial 0x1EDC6F41, bit-reflected
    poly = 0x82F63B78 :: Word32
    go 0 !c = c
    go n !c = go (n - 1) ((c `shiftR` 1) `xor` (poly .&. negate (c .&. 1)))
{-# NOINLINE table #-}
