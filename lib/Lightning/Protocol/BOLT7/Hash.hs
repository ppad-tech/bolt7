{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT7.Hash
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Signature hash computation for BOLT #7 messages.
--
-- These functions compute the double-SHA256 hash that is signed in each
-- message type. The hash covers the message content excluding the
-- signature field(s).
--
-- Each function takes the message payload as received: the bytes after
-- the 2-byte message type. The signatures cover every byte of that
-- payload, so hash the received bytes rather than a re-encoding of a
-- decoded message. Each function returns 'Nothing' if the input is too
-- short to hold the message.

module Lightning.Protocol.BOLT7.Hash (
  -- * Signature hashes
    channelAnnouncementHash
  , nodeAnnouncementHash
  , channelUpdateHash

  -- * Checksums
  , channelUpdateChecksum
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Lightning.Protocol.BOLT1.Prim as Prim
import Lightning.Protocol.BOLT7.CRC32C (crc32c)
import Lightning.Protocol.BOLT7.Types
  ( signatureLen, chainHashLen, shortChannelIdLen, pointLen, nodeIdLen
  , rgbColorLen, aliasLen )

-- | Double SHA-256 hash (used for Lightning message signing).
doubleSha256 :: ByteString -> ByteString
doubleSha256 = SHA256.hash . SHA256.hash
{-# INLINE doubleSha256 #-}

-- | Read the big-endian u16 at an offset, if present.
u16At :: Int -> ByteString -> Maybe Int
u16At off bs = fmap (fromIntegral . fst) (Prim.decodeU16 (BS.drop off bs))
{-# INLINE u16At #-}

-- | Length of a channel_update without extension bytes (136).
channelUpdateLen :: Int
channelUpdateLen = signatureLen + chainHashLen + shortChannelIdLen + 32
{-# INLINE channelUpdateLen #-}

-- | Compute signature hash for channel_announcement.
--
-- The hash covers the message starting at byte offset 256, which is after
-- the four 64-byte signatures (node_sig_1, node_sig_2, bitcoin_sig_1,
-- bitcoin_sig_2).
--
-- Returns the double-SHA256 hash (32 bytes), or 'Nothing' if the input
-- cannot hold the fields of a channel_announcement with its declared
-- feature length.
channelAnnouncementHash :: ByteString -> Maybe ByteString
channelAnnouncementHash !msg = do
  let !offset = 4 * signatureLen
  flen <- u16At offset msg
  -- features, chain_hash, short_channel_id, node_ids, bitcoin_keys
  let !minLen = offset + 2 + flen + chainHashLen + shortChannelIdLen
                + 2 * nodeIdLen + 2 * pointLen
  if BS.length msg < minLen
    then Nothing
    else Just (doubleSha256 (BS.drop offset msg))
{-# INLINE channelAnnouncementHash #-}

-- | Compute signature hash for node_announcement.
--
-- The hash covers the message starting after the signature field (64 bytes).
--
-- Returns the double-SHA256 hash (32 bytes), or 'Nothing' if the input
-- cannot hold the fields of a node_announcement with its declared
-- feature and address lengths.
nodeAnnouncementHash :: ByteString -> Maybe ByteString
nodeAnnouncementHash !msg = do
  flen <- u16At signatureLen msg
  -- features, timestamp, node_id, rgb_color, alias
  let !addrOff = signatureLen + 2 + flen + 4 + nodeIdLen + rgbColorLen
                 + aliasLen
  addrlen <- u16At addrOff msg
  if BS.length msg < addrOff + 2 + addrlen
    then Nothing
    else Just (doubleSha256 (BS.drop signatureLen msg))
{-# INLINE nodeAnnouncementHash #-}

-- | Compute signature hash for channel_update.
--
-- The hash covers the message starting after the signature field (64 bytes).
--
-- Returns the double-SHA256 hash (32 bytes), or 'Nothing' if the input
-- is shorter than a channel_update (136 bytes).
channelUpdateHash :: ByteString -> Maybe ByteString
channelUpdateHash !msg
  | BS.length msg < channelUpdateLen = Nothing
  | otherwise = Just (doubleSha256 (BS.drop signatureLen msg))
{-# INLINE channelUpdateHash #-}

-- | Compute checksum for channel_update.
--
-- This is the CRC-32C of the channel_update message excluding the
-- signature field (bytes 0-63) and timestamp field (bytes 104-107).
--
-- The checksum is used in the checksums_tlv of reply_channel_range.
--
-- Returns 'Nothing' if the input is shorter than a channel_update
-- (136 bytes).
--
-- Message layout after signature:
--   - chain_hash: 32 bytes (offset 64-95)
--   - short_channel_id: 8 bytes (offset 96-103)
--   - timestamp: 4 bytes (offset 104-107) -- EXCLUDED
--   - message_flags: 1 byte (offset 108)
--   - channel_flags: 1 byte (offset 109)
--   - cltv_expiry_delta: 2 bytes (offset 110-111)
--   - htlc_minimum_msat: 8 bytes (offset 112-119)
--   - fee_base_msat: 4 bytes (offset 120-123)
--   - fee_proportional_millionths: 4 bytes (offset 124-127)
--   - htlc_maximum_msat: 8 bytes (offset 128-135)
channelUpdateChecksum :: ByteString -> Maybe Word32
channelUpdateChecksum !msg
  | BS.length msg < channelUpdateLen = Nothing
  | otherwise = Just (crc32c checksumData)
  where
    -- Offset 64: chain_hash (32 bytes)
    chainHash = BS.take chainHashLen (BS.drop signatureLen msg)
    -- Offset 96: short_channel_id (8 bytes)
    scid = BS.take 8 (BS.drop (signatureLen + chainHashLen) msg)
    -- Skip timestamp (4 bytes at offset 104)
    -- Offset 108 to end: rest of message
    restOffset = signatureLen + chainHashLen + 8 + 4  -- 64 + 32 + 8 + 4 = 108
    rest = BS.drop restOffset msg
    -- Concatenate: chain_hash + scid + (message without sig and timestamp)
    checksumData = BS.concat [chainHash, scid, rest]
{-# INLINE channelUpdateChecksum #-}
