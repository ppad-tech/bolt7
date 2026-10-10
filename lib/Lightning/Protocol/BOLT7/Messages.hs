{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT7.Messages
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- The messages defined by BOLT #7.

module Lightning.Protocol.BOLT7.Messages (
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
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1
  ( ChainHash, ChannelId, MilliSatoshi, Point, ShortChannelId
  , Signature, TlvStream )
import qualified Lightning.Protocol.BOLT9 as BOLT9
import Lightning.Protocol.BOLT7.Types

-- | The @channel_announcement@ message (type 256).
data ChannelAnnouncement = ChannelAnnouncement
  { ca_node_signature_1    :: !Signature
  , ca_node_signature_2    :: !Signature
  , ca_bitcoin_signature_1 :: !Signature
  , ca_bitcoin_signature_2 :: !Signature
  , ca_features            :: !BOLT9.FeatureVector
  , ca_chain_hash          :: !ChainHash
  , ca_short_channel_id    :: {-# UNPACK #-} !ShortChannelId
  , ca_node_id_1           :: !Point
    -- ^ the lexicographically lesser node id
  , ca_node_id_2           :: !Point
  , ca_bitcoin_key_1       :: !Point
  , ca_bitcoin_key_2       :: !Point
  , ca_tlvs                :: !TlvStream
    -- ^ extension TLV records (all unknown odd)
  } deriving (Eq, Show, Generic)

instance NFData ChannelAnnouncement

-- | The @node_announcement@ message (type 257).
data NodeAnnouncement = NodeAnnouncement
  { na_signature         :: !Signature
  , na_features          :: !BOLT9.FeatureVector
  , na_timestamp         :: {-# UNPACK #-} !Word32
  , na_node_id           :: !Point
  , na_rgb_color         :: !RgbColor
  , na_alias             :: !Alias
  , na_addresses         :: ![Address]
    -- ^ address descriptors of known types, in order
  , na_unknown_addresses :: !BS.ByteString
    -- ^ the address bytes from the first descriptor of an unknown type
    --   on (empty if there is none), kept as received
  , na_tlvs              :: !TlvStream
    -- ^ extension TLV records (all unknown odd)
  } deriving (Eq, Show, Generic)

instance NFData NodeAnnouncement

-- | The @channel_update@ message (type 258).
data ChannelUpdate = ChannelUpdate
  { cu_signature                   :: !Signature
  , cu_chain_hash                  :: !ChainHash
  , cu_short_channel_id            :: {-# UNPACK #-} !ShortChannelId
  , cu_timestamp                   :: {-# UNPACK #-} !Word32
  , cu_message_flags               :: {-# UNPACK #-} !MessageFlags
  , cu_channel_flags               :: {-# UNPACK #-} !ChannelFlags
  , cu_cltv_expiry_delta           :: {-# UNPACK #-} !Word16
  , cu_htlc_minimum_msat           :: {-# UNPACK #-} !MilliSatoshi
  , cu_fee_base_msat               :: {-# UNPACK #-} !Word32
  , cu_fee_proportional_millionths :: {-# UNPACK #-} !Word32
  , cu_htlc_maximum_msat           :: {-# UNPACK #-} !MilliSatoshi
  , cu_tlvs                        :: !TlvStream
    -- ^ extension TLV records (all unknown odd)
  } deriving (Eq, Show, Generic)

instance NFData ChannelUpdate

-- | The @announcement_signatures@ message (type 259).
data AnnouncementSignatures = AnnouncementSignatures
  { as_channel_id        :: !ChannelId
  , as_short_channel_id  :: {-# UNPACK #-} !ShortChannelId
  , as_node_signature    :: !Signature
  , as_bitcoin_signature :: !Signature
  , as_tlvs              :: !TlvStream
    -- ^ extension TLV records (all unknown odd)
  } deriving (Eq, Show, Generic)

instance NFData AnnouncementSignatures

-- | The @query_short_channel_ids@ message (type 261).
data QueryShortChannelIds = QueryShortChannelIds
  { qsci_chain_hash        :: !ChainHash
  , qsci_short_channel_ids :: ![ShortChannelId]
  , qsci_query_flags       :: !(Maybe [QueryFlags])
    -- ^ @query_flags@ (TLV type 1): one per short channel id
  , qsci_tlvs              :: !TlvStream
    -- ^ unknown odd TLV records
  } deriving (Eq, Show, Generic)

instance NFData QueryShortChannelIds

-- | The @reply_short_channel_ids_end@ message (type 262).
data ReplyShortChannelIdsEnd = ReplyShortChannelIdsEnd
  { rsce_chain_hash       :: !ChainHash
  , rsce_full_information :: !Bool
  , rsce_tlvs             :: !TlvStream
    -- ^ extension TLV records (all unknown odd)
  } deriving (Eq, Show, Generic)

instance NFData ReplyShortChannelIdsEnd

-- | The @query_channel_range@ message (type 263).
data QueryChannelRange = QueryChannelRange
  { qcr_chain_hash       :: !ChainHash
  , qcr_first_blocknum   :: {-# UNPACK #-} !Word32
  , qcr_number_of_blocks :: {-# UNPACK #-} !Word32
  , qcr_query_option     :: !(Maybe QueryOption)
    -- ^ @query_option@ (TLV type 1)
  , qcr_tlvs             :: !TlvStream
    -- ^ unknown odd TLV records
  } deriving (Eq, Show, Generic)

instance NFData QueryChannelRange

-- | The @reply_channel_range@ message (type 264).
data ReplyChannelRange = ReplyChannelRange
  { rcr_chain_hash        :: !ChainHash
  , rcr_first_blocknum    :: {-# UNPACK #-} !Word32
  , rcr_number_of_blocks  :: {-# UNPACK #-} !Word32
  , rcr_sync_complete     :: !Bool
  , rcr_short_channel_ids :: ![ShortChannelId]
  , rcr_timestamps        :: !(Maybe [ChannelUpdateTimestamps])
    -- ^ @timestamps_tlv@ (TLV type 1): one per short channel id
  , rcr_checksums         :: !(Maybe [ChannelUpdateChecksums])
    -- ^ @checksums_tlv@ (TLV type 3): one per short channel id
  , rcr_tlvs              :: !TlvStream
    -- ^ unknown odd TLV records
  } deriving (Eq, Show, Generic)

instance NFData ReplyChannelRange

-- | The @gossip_timestamp_filter@ message (type 265).
data GossipTimestampFilter = GossipTimestampFilter
  { gtf_chain_hash      :: !ChainHash
  , gtf_first_timestamp :: {-# UNPACK #-} !Word32
  , gtf_timestamp_range :: {-# UNPACK #-} !Word32
  , gtf_tlvs            :: !TlvStream
    -- ^ extension TLV records (all unknown odd)
  } deriving (Eq, Show, Generic)

instance NFData GossipTimestampFilter

-- | A BOLT #7 message.
data Message
  = MsgChannelAnnouncement !ChannelAnnouncement
  | MsgNodeAnnouncement !NodeAnnouncement
  | MsgChannelUpdate !ChannelUpdate
  | MsgAnnouncementSignatures !AnnouncementSignatures
  | MsgQueryShortChannelIds !QueryShortChannelIds
  | MsgReplyShortChannelIdsEnd !ReplyShortChannelIdsEnd
  | MsgQueryChannelRange !QueryChannelRange
  | MsgReplyChannelRange !ReplyChannelRange
  | MsgGossipTimestampFilter !GossipTimestampFilter
  deriving (Eq, Show, Generic)

instance NFData Message

-- | The message type of a 'Message'.
message_type :: Message -> Word16
message_type m = case m of
  MsgChannelAnnouncement _     -> 256
  MsgNodeAnnouncement _        -> 257
  MsgChannelUpdate _           -> 258
  MsgAnnouncementSignatures _  -> 259
  MsgQueryShortChannelIds _    -> 261
  MsgReplyShortChannelIdsEnd _ -> 262
  MsgQueryChannelRange _       -> 263
  MsgReplyChannelRange _       -> 264
  MsgGossipTimestampFilter _   -> 265
