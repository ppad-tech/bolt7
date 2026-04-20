{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Main
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Criterion timing benchmarks for BOLT #7 gossip codecs.

module Main where

import Criterion.Main
import qualified Data.ByteString as BS
import Lightning.Protocol.BOLT1 (TlvStream, unsafeTlvStream)
import Lightning.Protocol.BOLT7

-- Test data construction ------------------------------------------------------

-- | 32 zero bytes for chain hashes, etc.
zeroBytes32 :: BS.ByteString
zeroBytes32 = BS.replicate 32 0x00
{-# NOINLINE zeroBytes32 #-}

-- | 8 zero bytes for short channel IDs.
zeroBytes8 :: BS.ByteString
zeroBytes8 = BS.replicate 8 0x00
{-# NOINLINE zeroBytes8 #-}

-- | 64-byte signature.
testSignature :: Signature
testSignature = case signature (BS.replicate 64 0x01) of
  Just s  -> s
  Nothing -> error "testSignature: invalid"
{-# NOINLINE testSignature #-}

-- | 33-byte compressed public key (02 prefix + 32 zero bytes).
testPoint :: Point
testPoint = case point (BS.cons 0x02 zeroBytes32) of
  Just p  -> p
  Nothing -> error "testPoint: invalid"
{-# NOINLINE testPoint #-}

-- | 32-byte chain hash.
testChainHash :: ChainHash
testChainHash = case chainHash zeroBytes32 of
  Just h  -> h
  Nothing -> error "testChainHash: invalid"
{-# NOINLINE testChainHash #-}

-- | 8-byte short channel ID.
testShortChannelId :: ShortChannelId
testShortChannelId = case scidFromBytes zeroBytes8 of
  Just s  -> s
  Nothing -> error "testShortChannelId: invalid"
{-# NOINLINE testShortChannelId #-}

-- | 32-byte channel ID.
testChannelId :: ChannelId
testChannelId = case channelId zeroBytes32 of
  Just c  -> c
  Nothing -> error "testChannelId: invalid"
{-# NOINLINE testChannelId #-}

-- | 33-byte node ID (03 prefix).
testNodeId :: NodeId
testNodeId = case nodeId (BS.cons 0x03 zeroBytes32) of
  Just n  -> n
  Nothing -> error "testNodeId: invalid"
{-# NOINLINE testNodeId #-}

-- | Second node ID (02 prefix, lexicographically smaller).
testNodeId2 :: NodeId
testNodeId2 = case nodeId (BS.cons 0x02 zeroBytes32) of
  Just n  -> n
  Nothing -> error "testNodeId2: invalid"
{-# NOINLINE testNodeId2 #-}

-- | RGB color.
testRgbColor :: RgbColor
testRgbColor = case rgbColor (BS.pack [0xff, 0x00, 0x00]) of
  Just c  -> c
  Nothing -> error "testRgbColor: invalid"
{-# NOINLINE testRgbColor #-}

-- | 32-byte alias.
testAlias :: Alias
testAlias = case alias zeroBytes32 of
  Just a  -> a
  Nothing -> error "testAlias: invalid"
{-# NOINLINE testAlias #-}

-- | IPv4 address.
testIPv4 :: IPv4Addr
testIPv4 = case ipv4Addr (BS.pack [127, 0, 0, 1]) of
  Just a  -> a
  Nothing -> error "testIPv4: invalid"
{-# NOINLINE testIPv4 #-}

-- | Empty TLV stream.
emptyTlvs :: TlvStream
emptyTlvs = unsafeTlvStream []
{-# NOINLINE emptyTlvs #-}

-- | Empty feature bits.
emptyFeatures :: FeatureBits
emptyFeatures = featureBits BS.empty
{-# NOINLINE emptyFeatures #-}

-- | List of test SCIDs for list encoding benchmarks.
testScidList :: [ShortChannelId]
testScidList = map mkScid [1..100]
  where
    mkScid n = case scidFromBytes (BS.pack [0, 0, 0, n, 0, 0, 0, n]) of
      Just s  -> s
      Nothing -> error "mkScid: invalid"
{-# NOINLINE testScidList #-}

-- | Encoded SCID list for decode benchmarks.
encodedScidList :: BS.ByteString
encodedScidList = encodeShortChannelIdList testScidList
{-# NOINLINE encodedScidList #-}

-- Test messages ---------------------------------------------------------------

-- | Test ChannelAnnouncement message.
testChannelAnnouncement :: ChannelAnnouncement
testChannelAnnouncement = ChannelAnnouncement
  { channelAnnNodeSig1     = testSignature
  , channelAnnNodeSig2     = testSignature
  , channelAnnBitcoinSig1  = testSignature
  , channelAnnBitcoinSig2  = testSignature
  , channelAnnFeatures     = emptyFeatures
  , channelAnnChainHash    = testChainHash
  , channelAnnShortChanId  = testShortChannelId
  , channelAnnNodeId1      = testNodeId2  -- 02... (smaller)
  , channelAnnNodeId2      = testNodeId   -- 03... (larger)
  , channelAnnBitcoinKey1  = testPoint
  , channelAnnBitcoinKey2  = testPoint
  }
{-# NOINLINE testChannelAnnouncement #-}

-- | Encoded ChannelAnnouncement for decode benchmarks.
encodedChannelAnnouncement :: BS.ByteString
encodedChannelAnnouncement = encodeChannelAnnouncement testChannelAnnouncement
{-# NOINLINE encodedChannelAnnouncement #-}

-- | Test NodeAnnouncement message.
testNodeAnnouncement :: NodeAnnouncement
testNodeAnnouncement = NodeAnnouncement
  { nodeAnnSignature = testSignature
  , nodeAnnFeatures  = emptyFeatures
  , nodeAnnTimestamp = 1234567890
  , nodeAnnNodeId    = testNodeId
  , nodeAnnRgbColor  = testRgbColor
  , nodeAnnAlias     = testAlias
  , nodeAnnAddresses = [AddrIPv4 testIPv4 9735]
  }
{-# NOINLINE testNodeAnnouncement #-}

-- | Encoded NodeAnnouncement for decode benchmarks.
encodedNodeAnnouncement :: BS.ByteString
encodedNodeAnnouncement = case encodeNodeAnnouncement testNodeAnnouncement of
  Right bs -> bs
  Left _   -> error "encodedNodeAnnouncement: encode failed"
{-# NOINLINE encodedNodeAnnouncement #-}

-- | Test ChannelUpdate message.
testChannelUpdate :: ChannelUpdate
testChannelUpdate = ChannelUpdate
  { chanUpdateSignature       = testSignature
  , chanUpdateChainHash       = testChainHash
  , chanUpdateShortChanId     = testShortChannelId
  , chanUpdateTimestamp       = 1234567890
  , chanUpdateChanFlags       = ChannelFlags NodeOne Enabled
  , chanUpdateCltvExpDelta    = CltvExpiryDelta 144
  , chanUpdateHtlcMinMsat     = HtlcMinimumMsat 1000
  , chanUpdateFeeBaseMsat     = FeeBaseMsat 1000
  , chanUpdateFeeProportional = FeeProportionalMillionths 100
  , chanUpdateHtlcMaxMsat     = Just (HtlcMaximumMsat 1000000000)
  }
{-# NOINLINE testChannelUpdate #-}

-- | Encoded ChannelUpdate for decode benchmarks.
encodedChannelUpdate :: BS.ByteString
encodedChannelUpdate = encodeChannelUpdate testChannelUpdate
{-# NOINLINE encodedChannelUpdate #-}

-- | Test AnnouncementSignatures message.
testAnnouncementSignatures :: AnnouncementSignatures
testAnnouncementSignatures = AnnouncementSignatures
  { annSigChannelId   = testChannelId
  , annSigShortChanId = testShortChannelId
  , annSigNodeSig     = testSignature
  , annSigBitcoinSig  = testSignature
  }
{-# NOINLINE testAnnouncementSignatures #-}

-- | Encoded AnnouncementSignatures for decode benchmarks.
encodedAnnouncementSignatures :: BS.ByteString
encodedAnnouncementSignatures =
  encodeAnnouncementSignatures testAnnouncementSignatures
{-# NOINLINE encodedAnnouncementSignatures #-}

-- | Test QueryShortChannelIds message.
testQueryShortChannelIds :: QueryShortChannelIds
testQueryShortChannelIds = QueryShortChannelIds
  { queryScidsChainHash = testChainHash
  , queryScidsData      = encodeShortChannelIdList [testShortChannelId]
  , queryScidsTlvs      = emptyTlvs
  }
{-# NOINLINE testQueryShortChannelIds #-}

-- | Encoded QueryShortChannelIds for decode benchmarks.
encodedQueryShortChannelIds :: BS.ByteString
encodedQueryShortChannelIds =
  case encodeQueryShortChannelIds testQueryShortChannelIds of
    Right bs -> bs
    Left _   -> error "encodedQueryShortChannelIds: encode failed"
{-# NOINLINE encodedQueryShortChannelIds #-}

-- | Test ReplyShortChannelIdsEnd message.
testReplyShortChannelIdsEnd :: ReplyShortChannelIdsEnd
testReplyShortChannelIdsEnd = ReplyShortChannelIdsEnd
  { replyScidsChainHash = testChainHash
  , replyScidsFullInfo  = 1
  }
{-# NOINLINE testReplyShortChannelIdsEnd #-}

-- | Encoded ReplyShortChannelIdsEnd for decode benchmarks.
encodedReplyShortChannelIdsEnd :: BS.ByteString
encodedReplyShortChannelIdsEnd =
  encodeReplyShortChannelIdsEnd testReplyShortChannelIdsEnd
{-# NOINLINE encodedReplyShortChannelIdsEnd #-}

-- | Test QueryChannelRange message.
testQueryChannelRange :: QueryChannelRange
testQueryChannelRange = QueryChannelRange
  { queryRangeChainHash  = testChainHash
  , queryRangeFirstBlock = BlockHeight 700000
  , queryRangeNumBlocks  = BlockCount 10000
  , queryRangeTlvs       = emptyTlvs
  }
{-# NOINLINE testQueryChannelRange #-}

-- | Encoded QueryChannelRange for decode benchmarks.
encodedQueryChannelRange :: BS.ByteString
encodedQueryChannelRange = encodeQueryChannelRange testQueryChannelRange
{-# NOINLINE encodedQueryChannelRange #-}

-- | Test ReplyChannelRange message.
testReplyChannelRange :: ReplyChannelRange
testReplyChannelRange = ReplyChannelRange
  { replyRangeChainHash    = testChainHash
  , replyRangeFirstBlock   = BlockHeight 700000
  , replyRangeNumBlocks    = BlockCount 10000
  , replyRangeSyncComplete = 1
  , replyRangeData         = encodeShortChannelIdList [testShortChannelId]
  , replyRangeTlvs         = emptyTlvs
  }
{-# NOINLINE testReplyChannelRange #-}

-- | Encoded ReplyChannelRange for decode benchmarks.
encodedReplyChannelRange :: BS.ByteString
encodedReplyChannelRange = case encodeReplyChannelRange testReplyChannelRange of
  Right bs -> bs
  Left _   -> error "encodedReplyChannelRange: encode failed"
{-# NOINLINE encodedReplyChannelRange #-}

-- | Test GossipTimestampFilter message.
testGossipTimestampFilter :: GossipTimestampFilter
testGossipTimestampFilter = GossipTimestampFilter
  { gossipFilterChainHash      = testChainHash
  , gossipFilterFirstTimestamp = 1609459200
  , gossipFilterTimestampRange = 86400
  }
{-# NOINLINE testGossipTimestampFilter #-}

-- | Encoded GossipTimestampFilter for decode benchmarks.
encodedGossipTimestampFilter :: BS.ByteString
encodedGossipTimestampFilter =
  encodeGossipTimestampFilter testGossipTimestampFilter
{-# NOINLINE encodedGossipTimestampFilter #-}

-- Benchmark groups ------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "channel_announcement"
      [ bench "encode" $ nf encodeChannelAnnouncement testChannelAnnouncement
      , bench "decode" $ nf decodeChannelAnnouncement encodedChannelAnnouncement
      ]
  , bgroup "node_announcement"
      [ bench "encode" $ nf encodeNodeAnnouncement testNodeAnnouncement
      , bench "decode" $ nf decodeNodeAnnouncement encodedNodeAnnouncement
      ]
  , bgroup "channel_update"
      [ bench "encode" $ nf encodeChannelUpdate testChannelUpdate
      , bench "decode" $ nf decodeChannelUpdate encodedChannelUpdate
      ]
  , bgroup "announcement_signatures"
      [ bench "encode" $
          nf encodeAnnouncementSignatures testAnnouncementSignatures
      , bench "decode" $
          nf decodeAnnouncementSignatures encodedAnnouncementSignatures
      ]
  , bgroup "query_short_channel_ids"
      [ bench "encode" $
          nf encodeQueryShortChannelIds testQueryShortChannelIds
      , bench "decode" $
          nf decodeQueryShortChannelIds encodedQueryShortChannelIds
      ]
  , bgroup "reply_short_channel_ids_end"
      [ bench "encode" $
          nf encodeReplyShortChannelIdsEnd testReplyShortChannelIdsEnd
      , bench "decode" $
          nf decodeReplyShortChannelIdsEnd encodedReplyShortChannelIdsEnd
      ]
  , bgroup "query_channel_range"
      [ bench "encode" $ nf encodeQueryChannelRange testQueryChannelRange
      , bench "decode" $ nf decodeQueryChannelRange encodedQueryChannelRange
      ]
  , bgroup "reply_channel_range"
      [ bench "encode" $ nf encodeReplyChannelRange testReplyChannelRange
      , bench "decode" $ nf decodeReplyChannelRange encodedReplyChannelRange
      ]
  , bgroup "gossip_timestamp_filter"
      [ bench "encode" $
          nf encodeGossipTimestampFilter testGossipTimestampFilter
      , bench "decode" $
          nf decodeGossipTimestampFilter encodedGossipTimestampFilter
      ]
  , bgroup "scid_list"
      [ bench "encode (100)" $ nf encodeShortChannelIdList testScidList
      , bench "decode (100)" $ nf decodeShortChannelIdList encodedScidList
      ]
  , bgroup "hash"
      [ bench "channelAnnouncementHash" $
          nf channelAnnouncementHash encodedChannelAnnouncement
      , bench "nodeAnnouncementHash" $
          nf nodeAnnouncementHash encodedNodeAnnouncement
      , bench "channelUpdateHash" $
          nf channelUpdateHash encodedChannelUpdate
      , bench "channelUpdateChecksum" $
          nf channelUpdateChecksum encodedChannelUpdate
      ]
  , bgroup "validate"
      [ bench "channelAnnouncement" $
          nf validateChannelAnnouncement testChannelAnnouncement
      , bench "nodeAnnouncement" $
          nf validateNodeAnnouncement testNodeAnnouncement
      , bench "channelUpdate" $
          nf validateChannelUpdate testChannelUpdate
      , bench "queryChannelRange" $
          nf validateQueryChannelRange testQueryChannelRange
      , bench "replyChannelRange" $
          nf validateReplyChannelRange testReplyChannelRange
      ]
  ]
