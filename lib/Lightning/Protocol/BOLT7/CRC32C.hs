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

import Data.Bits (complement, shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)

-- the Castagnoli polynomial 0x1EDC6F41, bit-reflected
poly :: Word32
poly = 0x82F63B78

-- | The CRC-32C of a byte string.
crc32c :: BS.ByteString -> Word32
crc32c = complement . BS.foldl' step 0xFFFFFFFF
{-# INLINE crc32c #-}

step :: Word32 -> Word8 -> Word32
step !crc !byte = go (8 :: Int) (crc `xor` fromIntegral byte)
  where
    go 0 !c = c
    go n !c = go (n - 1) ((c `shiftR` 1) `xor` (poly .&. negate (c .&. 1)))
{-# INLINE step #-}
