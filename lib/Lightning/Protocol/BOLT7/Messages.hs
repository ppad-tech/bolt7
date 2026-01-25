{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT7.Messages
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- BOLT #7 gossip message type definitions.

module Lightning.Protocol.BOLT7.Messages (
  -- * Message types
    MsgType(..)
  , msgTypeCode

  -- * Channel announcement
  , ChannelAnnouncement(..)

  -- * Node announcement
  , NodeAnnouncement(..)

  -- * Channel update
  , ChannelUpdate(..)

  -- * Announcement signatures
  , AnnouncementSignatures(..)

  -- * Query messages
  , QueryShortChannelIds(..)
  , ReplyShortChannelIdsEnd(..)
  , QueryChannelRange(..)
  , ReplyChannelRange(..)
  , GossipTimestampFilter(..)

  -- * Union type
  , Message(..)
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1 (TlvStream)
import Lightning.Protocol.BOLT7.Types

-- Message type codes ----------------------------------------------------------

-- | BOLT #7 message type codes.
data MsgType
  = MsgChannelAnnouncement       -- ^ 256
  | MsgNodeAnnouncement          -- ^ 257
  | MsgChannelUpdate             -- ^ 258
  | MsgAnnouncementSignatures    -- ^ 259
  | MsgQueryShortChannelIds      -- ^ 261
  | MsgReplyShortChannelIdsEnd   -- ^ 262
  | MsgQueryChannelRange         -- ^ 263
  | MsgReplyChannelRange         -- ^ 264
  | MsgGossipTimestampFilter     -- ^ 265
  deriving (Eq, Show, Generic)

instance NFData MsgType

-- | Get numeric code for message type.
msgTypeCode :: MsgType -> Word16
msgTypeCode MsgChannelAnnouncement     = 256
msgTypeCode MsgNodeAnnouncement        = 257
msgTypeCode MsgChannelUpdate           = 258
msgTypeCode MsgAnnouncementSignatures  = 259
msgTypeCode MsgQueryShortChannelIds    = 261
msgTypeCode MsgReplyShortChannelIdsEnd = 262
msgTypeCode MsgQueryChannelRange       = 263
msgTypeCode MsgReplyChannelRange       = 264
msgTypeCode MsgGossipTimestampFilter   = 265
{-# INLINE msgTypeCode #-}

-- Channel announcement --------------------------------------------------------

-- | channel_announcement message (type 256).
--
-- Announces a public channel to the network.
data ChannelAnnouncement = ChannelAnnouncement
  { channelAnnNodeSig1     :: !Signature     -- ^ Signature from node_id_1
  , channelAnnNodeSig2     :: !Signature     -- ^ Signature from node_id_2
  , channelAnnBitcoinSig1  :: !Signature     -- ^ Signature from bitcoin_key_1
  , channelAnnBitcoinSig2  :: !Signature     -- ^ Signature from bitcoin_key_2
  , channelAnnFeatures     :: !FeatureBits   -- ^ Feature bits
  , channelAnnChainHash    :: !ChainHash     -- ^ Chain identifier
  , channelAnnShortChanId  :: !ShortChannelId -- ^ Short channel ID
  , channelAnnNodeId1      :: !NodeId        -- ^ First node (lexicographically)
  , channelAnnNodeId2      :: !NodeId        -- ^ Second node
  , channelAnnBitcoinKey1  :: !Point         -- ^ Bitcoin key for node_id_1
  , channelAnnBitcoinKey2  :: !Point         -- ^ Bitcoin key for node_id_2
  }
  deriving (Eq, Show, Generic)

instance NFData ChannelAnnouncement

-- Node announcement -----------------------------------------------------------

-- | node_announcement message (type 257).
--
-- Advertises node metadata to the network.
data NodeAnnouncement = NodeAnnouncement
  { nodeAnnSignature  :: !Signature      -- ^ Signature of message
  , nodeAnnFeatures   :: !FeatureBits    -- ^ Feature bits
  , nodeAnnTimestamp  :: !Timestamp      -- ^ Unix timestamp
  , nodeAnnNodeId     :: !NodeId         -- ^ Node public key
  , nodeAnnRgbColor   :: !RgbColor       -- ^ RGB color
  , nodeAnnAlias      :: !Alias          -- ^ Node alias (32 bytes UTF-8)
  , nodeAnnAddresses  :: ![Address]      -- ^ List of addresses
  }
  deriving (Eq, Show, Generic)

instance NFData NodeAnnouncement

-- Channel update --------------------------------------------------------------

-- | channel_update message (type 258).
--
-- Communicates per-direction routing parameters.
data ChannelUpdate = ChannelUpdate
  { chanUpdateSignature      :: !Signature       -- ^ Signature of message
  , chanUpdateChainHash      :: !ChainHash       -- ^ Chain identifier
  , chanUpdateShortChanId    :: !ShortChannelId  -- ^ Short channel ID
  , chanUpdateTimestamp      :: !Timestamp       -- ^ Unix timestamp
  , chanUpdateMsgFlags       :: !Word8           -- ^ Message flags
  , chanUpdateChanFlags      :: !Word8           -- ^ Channel flags
  , chanUpdateCltvExpDelta   :: !CltvExpiryDelta -- ^ CLTV expiry delta
  , chanUpdateHtlcMinMsat    :: !HtlcMinimumMsat -- ^ Minimum HTLC msat
  , chanUpdateFeeBaseMsat    :: !FeeBaseMsat     -- ^ Base fee msat
  , chanUpdateFeeProportional :: !FeeProportionalMillionths -- ^ Prop fee
  , chanUpdateHtlcMaxMsat    :: !(Maybe HtlcMaximumMsat) -- ^ Max HTLC (optional)
  }
  deriving (Eq, Show, Generic)

instance NFData ChannelUpdate

-- Announcement signatures -----------------------------------------------------

-- | announcement_signatures message (type 259).
--
-- Sent between channel peers to enable channel announcement.
data AnnouncementSignatures = AnnouncementSignatures
  { annSigChannelId     :: !ChannelId       -- ^ Channel ID
  , annSigShortChanId   :: !ShortChannelId  -- ^ Short channel ID
  , annSigNodeSig       :: !Signature       -- ^ Node signature
  , annSigBitcoinSig    :: !Signature       -- ^ Bitcoin signature
  }
  deriving (Eq, Show, Generic)

instance NFData AnnouncementSignatures

-- Query messages --------------------------------------------------------------

-- | query_short_channel_ids message (type 261).
--
-- Requests information about specific channels.
data QueryShortChannelIds = QueryShortChannelIds
  { queryScidsChainHash :: !ChainHash    -- ^ Chain identifier
  , queryScidsData      :: !ByteString   -- ^ Encoded short_channel_ids
  , queryScidsTlvs      :: !TlvStream    -- ^ Optional TLV (query_flags)
  }
  deriving (Eq, Show, Generic)

instance NFData QueryShortChannelIds

-- | reply_short_channel_ids_end message (type 262).
--
-- Concludes response to query_short_channel_ids.
data ReplyShortChannelIdsEnd = ReplyShortChannelIdsEnd
  { replyScidsChainHash    :: !ChainHash  -- ^ Chain identifier
  , replyScidsFullInfo     :: !Word8      -- ^ 1 if complete, 0 otherwise
  }
  deriving (Eq, Show, Generic)

instance NFData ReplyShortChannelIdsEnd

-- | query_channel_range message (type 263).
--
-- Queries channels within a block range.
data QueryChannelRange = QueryChannelRange
  { queryRangeChainHash     :: !ChainHash  -- ^ Chain identifier
  , queryRangeFirstBlock    :: !Word32     -- ^ First block number
  , queryRangeNumBlocks     :: !Word32     -- ^ Number of blocks
  , queryRangeTlvs          :: !TlvStream  -- ^ Optional TLV (query_option)
  }
  deriving (Eq, Show, Generic)

instance NFData QueryChannelRange

-- | reply_channel_range message (type 264).
--
-- Responds to query_channel_range with channel IDs.
data ReplyChannelRange = ReplyChannelRange
  { replyRangeChainHash   :: !ChainHash    -- ^ Chain identifier
  , replyRangeFirstBlock  :: !Word32       -- ^ First block number
  , replyRangeNumBlocks   :: !Word32       -- ^ Number of blocks
  , replyRangeSyncComplete :: !Word8       -- ^ 1 if sync complete
  , replyRangeData        :: !ByteString   -- ^ Encoded short_channel_ids
  , replyRangeTlvs        :: !TlvStream    -- ^ Optional TLVs
  }
  deriving (Eq, Show, Generic)

instance NFData ReplyChannelRange

-- | gossip_timestamp_filter message (type 265).
--
-- Constrains which gossip messages are relayed.
data GossipTimestampFilter = GossipTimestampFilter
  { gossipFilterChainHash     :: !ChainHash  -- ^ Chain identifier
  , gossipFilterFirstTimestamp :: !Word32    -- ^ First timestamp
  , gossipFilterTimestampRange :: !Word32    -- ^ Timestamp range
  }
  deriving (Eq, Show, Generic)

instance NFData GossipTimestampFilter

-- Union type ------------------------------------------------------------------

-- | Union of all BOLT #7 message types.
data Message
  = MsgChanAnn !ChannelAnnouncement
  | MsgNodeAnn !NodeAnnouncement
  | MsgChanUpd !ChannelUpdate
  | MsgAnnSig  !AnnouncementSignatures
  | MsgQueryScids !QueryShortChannelIds
  | MsgReplyScids !ReplyShortChannelIdsEnd
  | MsgQueryRange !QueryChannelRange
  | MsgReplyRange !ReplyChannelRange
  | MsgGossipFilter !GossipTimestampFilter
  deriving (Eq, Show, Generic)

instance NFData Message
