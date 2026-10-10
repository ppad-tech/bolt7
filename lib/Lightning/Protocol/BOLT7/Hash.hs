{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT7.Hash
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Signature digests, signature verification and checksums for BOLT #7
-- messages.
--
-- Signatures cover the encoded message after its signature fields,
-- extension included. Since decoding keeps every byte, the digest of a
-- decoded message is the digest of the bytes received.

module Lightning.Protocol.BOLT7.Hash (
    channel_announcement_hash
  , node_announcement_hash
  , channel_update_hash
  , verify_channel_announcement
  , verify_node_announcement
  , verify_channel_update
  , channel_update_checksum
  ) where

import qualified Crypto.Curve.Secp256k1 as Secp256k1
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import Data.Word (Word32)
import Lightning.Protocol.BOLT1 (Point, Signature)
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT7.CRC32C (crc32c)
import Lightning.Protocol.BOLT7.Codec
import Lightning.Protocol.BOLT7.Messages

-- the signed bytes of each message
announcement_signed :: ChannelAnnouncement -> Either EncodeError BS.ByteString
announcement_signed = fmap (BS.drop 256) . encode_channel_announcement
{-# INLINE announcement_signed #-}

node_signed :: NodeAnnouncement -> Either EncodeError BS.ByteString
node_signed = fmap (BS.drop 64) . encode_node_announcement
{-# INLINE node_signed #-}

update_signed :: ChannelUpdate -> BS.ByteString
update_signed = BS.drop 64 . encode_channel_update
{-# INLINE update_signed #-}

double_sha256 :: BS.ByteString -> BS.ByteString
double_sha256 = SHA256.hash . SHA256.hash
{-# INLINE double_sha256 #-}

-- | The digest the four signatures of a 'ChannelAnnouncement' sign: the
--   double SHA-256 of its encoding from offset 256 (after the
--   signatures) to the end. Fails as 'encode_channel_announcement' does.
channel_announcement_hash
  :: ChannelAnnouncement -> Either EncodeError BS.ByteString
channel_announcement_hash = fmap double_sha256 . announcement_signed

-- | The digest the signature of a 'NodeAnnouncement' signs: the double
--   SHA-256 of its encoding after the signature. Fails as
--   'encode_node_announcement' does.
node_announcement_hash
  :: NodeAnnouncement -> Either EncodeError BS.ByteString
node_announcement_hash = fmap double_sha256 . node_signed

-- | The digest the signature of a 'ChannelUpdate' signs: the double
--   SHA-256 of its encoding after the signature.
channel_update_hash :: ChannelUpdate -> BS.ByteString
channel_update_hash = double_sha256 . update_signed

-- | Verify the four signatures of a 'ChannelAnnouncement': both node
--   signatures and both bitcoin signatures, each over
--   'channel_announcement_hash' under the corresponding key.
--
--   As with libsecp256k1, signatures must be in low-s form. A key that
--   isn't a point on the curve fails verification.
verify_channel_announcement :: ChannelAnnouncement -> Bool
verify_channel_announcement m = case announcement_signed m of
  Left _ -> False
  Right signed ->
    let !d = SHA256.hash signed
    in  verify d (ca_node_id_1 m) (ca_node_signature_1 m)
          && verify d (ca_node_id_2 m) (ca_node_signature_2 m)
          && verify d (ca_bitcoin_key_1 m) (ca_bitcoin_signature_1 m)
          && verify d (ca_bitcoin_key_2 m) (ca_bitcoin_signature_2 m)

-- | Verify the signature of a 'NodeAnnouncement' under its node id.
--
--   As with libsecp256k1, the signature must be in low-s form.
verify_node_announcement :: NodeAnnouncement -> Bool
verify_node_announcement m = case node_signed m of
  Left _       -> False
  Right signed -> verify (SHA256.hash signed) (na_node_id m) (na_signature m)

-- | Verify the signature of a 'ChannelUpdate' under the given node id.
--
--   The key is that of the channel's 'ChannelAnnouncement' selected by
--   the update's direction: 'ca_node_id_1' for
--   'Lightning.Protocol.BOLT7.NodeOne', 'ca_node_id_2' for
--   'Lightning.Protocol.BOLT7.NodeTwo'. As with libsecp256k1, the
--   signature must be in low-s form.
verify_channel_update :: Point -> ChannelUpdate -> Bool
verify_channel_update k m =
  verify (SHA256.hash (update_signed m)) k (cu_signature m)

-- verify a signature over the double SHA-256 of some bytes, given their
-- single SHA-256 (secp256k1's verify_ecdsa hashes once more)
verify :: BS.ByteString -> Point -> Signature -> Bool
verify d k s =
  case ( Secp256k1.parse_point (BOLT1.un_point k)
       , Secp256k1.parse_sig (BOLT1.un_signature s) ) of
    (Just p, Just sig) -> Secp256k1.verify_ecdsa' Secp256k1.precompute d p sig
    _                  -> False

-- | The checksum of a 'ChannelUpdate', as used in the @checksums_tlv@ of
--   @reply_channel_range@: the CRC-32C (RFC 3720) of the update without
--   its signature and timestamp. Extension records are included.
channel_update_checksum :: ChannelUpdate -> Word32
channel_update_checksum m =
  let !bs = encode_channel_update m
  in  crc32c (BS.take 40 (BS.drop 64 bs) <> BS.drop 108 bs)
