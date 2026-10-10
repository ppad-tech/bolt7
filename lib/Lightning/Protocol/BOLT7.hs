{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Lightning.Protocol.BOLT7
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Routing gossip for the Lightning Network, per
-- [BOLT #7](https://github.com/lightning/bolts/blob/master/07-routing-gossip.md):
-- the gossip messages and queries, their signature digests and
-- checksums, signature verification, and stateless validation.
--
-- Fundamental types (chain hashes, points, signatures, short channel
-- ids, amounts, TLV streams) come from ppad-bolt1, and feature vectors
-- from ppad-bolt9.
--
-- Decoders take a message payload, without its 2-byte type, and consume
-- all of it. Every byte is kept, including unassigned flag bits,
-- unknown address descriptors and unknown odd TLV records, so encoding
-- a decoded message reproduces its payload exactly and its signatures
-- still verify.

module Lightning.Protocol.BOLT7 (
  -- * Messages
    Message(..)
  , message_type
  , ChannelAnnouncement(..)
  , NodeAnnouncement(..)
  , ChannelUpdate(..)
  , AnnouncementSignatures(..)
  , QueryShortChannelIds(..)
  , ReplyShortChannelIdsEnd(..)
  , QueryChannelRange(..)
  , ReplyChannelRange(..)
  , GossipTimestampFilter(..)

  -- * Encoding and decoding
  , encode_message
  , decode_message
  , EncodeError(..)
  , DecodeError(..)

  -- ** Payloads
  , encode_channel_announcement
  , decode_channel_announcement
  , encode_node_announcement
  , decode_node_announcement
  , encode_channel_update
  , decode_channel_update
  , encode_announcement_signatures
  , decode_announcement_signatures
  , encode_query_short_channel_ids
  , decode_query_short_channel_ids
  , encode_reply_short_channel_ids_end
  , decode_reply_short_channel_ids_end
  , encode_query_channel_range
  , decode_query_channel_range
  , encode_reply_channel_range
  , decode_reply_channel_range
  , encode_gossip_timestamp_filter
  , decode_gossip_timestamp_filter

  -- * Signatures and checksums
  , channel_announcement_hash
  , node_announcement_hash
  , channel_update_hash
  , verify_channel_announcement
  , verify_node_announcement
  , verify_channel_update
  , channel_update_checksum

  -- * Validation
  , ValidationError(..)
  , validate_channel_announcement
  , validate_node_announcement
  , validate_channel_update
  , validate_query_short_channel_ids
  , validate_query_channel_range
  , validate_reply_channel_range

  -- * channel_update flags
  , MessageFlags(..)
  , Forwarding(..)
  , message_flags
  , message_flags_forwarding
  , has_must_be_one
  , ChannelFlags(..)
  , Direction(..)
  , ChannelStatus(..)
  , channel_flags
  , channel_flags_direction
  , channel_flags_status

  -- * node_announcement fields
  , RgbColor(..)
  , Alias
  , alias
  , un_alias

  -- ** Addresses
  , Address(..)
  , usable_addresses
  , IPv4Addr
  , ipv4_addr
  , un_ipv4_addr
  , IPv6Addr
  , ipv6_addr
  , un_ipv6_addr
  , TorV2Addr
  , tor_v2_addr
  , un_tor_v2_addr
  , TorV3Addr
  , tor_v3_addr
  , un_tor_v3_addr
  , Hostname
  , hostname
  , un_hostname
  , is_ascii_hostname

  -- * Query fields
  , QueryFlags(..)
  , QueryFlag(..)
  , query_flags
  , has_query_flag
  , QueryOption(..)
  , QueryOptionFlag(..)
  , query_option
  , has_query_option
  , ChannelUpdateTimestamps(..)
  , ChannelUpdateChecksums(..)

  -- * Short channel ids
  , format_short_channel_id
  ) where

import Lightning.Protocol.BOLT7.Codec
import Lightning.Protocol.BOLT7.Hash
import Lightning.Protocol.BOLT7.Messages
import Lightning.Protocol.BOLT7.Types
import Lightning.Protocol.BOLT7.Validate
