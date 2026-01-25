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
import Lightning.Protocol.BOLT7.CRC32C (crc32c)
import Lightning.Protocol.BOLT7.Types (signatureLen, chainHashLen)

-- | Compute signature hash for channel_announcement.
--
-- The hash covers the message starting at byte offset 256, which is after
-- the four 64-byte signatures (node_sig_1, node_sig_2, bitcoin_sig_1,
-- bitcoin_sig_2).
--
-- Returns the double-SHA256 hash (32 bytes).
channelAnnouncementHash :: ByteString -> ByteString
channelAnnouncementHash !msg =
  let offset = 4 * signatureLen  -- 4 signatures * 64 bytes = 256
      payload = BS.drop offset msg
  in  SHA256.hash (SHA256.hash payload)
{-# INLINE channelAnnouncementHash #-}

-- | Compute signature hash for node_announcement.
--
-- The hash covers the message starting after the signature field (64 bytes).
--
-- Returns the double-SHA256 hash (32 bytes).
nodeAnnouncementHash :: ByteString -> ByteString
nodeAnnouncementHash !msg =
  let payload = BS.drop signatureLen msg
  in  SHA256.hash (SHA256.hash payload)
{-# INLINE nodeAnnouncementHash #-}

-- | Compute signature hash for channel_update.
--
-- The hash covers the message starting after the signature field (64 bytes).
--
-- Returns the double-SHA256 hash (32 bytes).
channelUpdateHash :: ByteString -> ByteString
channelUpdateHash !msg =
  let payload = BS.drop signatureLen msg
  in  SHA256.hash (SHA256.hash payload)
{-# INLINE channelUpdateHash #-}

-- | Compute checksum for channel_update.
--
-- This is the CRC-32C of the channel_update message excluding the
-- signature field (bytes 0-63) and timestamp field (bytes 96-99).
--
-- The checksum is used in the checksums_tlv of reply_channel_range.
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
--   - htlc_maximum_msat: 8 bytes (offset 128-135, if present)
channelUpdateChecksum :: ByteString -> Word32
channelUpdateChecksum !msg =
  let -- Offset 64: chain_hash (32 bytes)
      chainHash = BS.take chainHashLen (BS.drop signatureLen msg)
      -- Offset 96: short_channel_id (8 bytes)
      scid = BS.take 8 (BS.drop (signatureLen + chainHashLen) msg)
      -- Skip timestamp (4 bytes at offset 104)
      -- Offset 108 to end: rest of message
      restOffset = signatureLen + chainHashLen + 8 + 4  -- 64 + 32 + 8 + 4 = 108
      rest = BS.drop restOffset msg
      -- Concatenate: chain_hash + scid + (message without sig and timestamp)
      checksumData = BS.concat [chainHash, scid, rest]
  in  crc32c checksumData
{-# INLINE channelUpdateChecksum #-}
