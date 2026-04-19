{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Main
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Weigh allocation benchmarks for BOLT #7 gossip codecs.

module Main where

import qualified Data.ByteString as BS
import Lightning.Protocol.BOLT1 (TlvStream, unsafeTlvStream)
import Lightning.Protocol.BOLT7
import Weigh

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

-- | Empty feature bits.
emptyFeatures :: FeatureBits
emptyFeatures = featureBits BS.empty
{-# NOINLINE emptyFeatures #-}

-- | Empty TLV stream.
emptyTlvs :: TlvStream
emptyTlvs = unsafeTlvStream []
{-# NOINLINE emptyTlvs #-}

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

-- Message constructors --------------------------------------------------------

-- | Construct ChannelAnnouncement message.
mkChannelAnnouncement
  :: Signature -> Signature -> ChainHash -> ShortChannelId
  -> NodeId -> NodeId -> Point -> Point -> FeatureBits
  -> ChannelAnnouncement
mkChannelAnnouncement !ns1 !ns2 !ch !scid !nid1 !nid2 !bk1 !bk2 !feat =
  ChannelAnnouncement
    { channelAnnNodeSig1     = ns1
    , channelAnnNodeSig2     = ns2
    , channelAnnBitcoinSig1  = ns1
    , channelAnnBitcoinSig2  = ns2
    , channelAnnFeatures     = feat
    , channelAnnChainHash    = ch
    , channelAnnShortChanId  = scid
    , channelAnnNodeId1      = nid1
    , channelAnnNodeId2      = nid2
    , channelAnnBitcoinKey1  = bk1
    , channelAnnBitcoinKey2  = bk2
    }

-- | Construct NodeAnnouncement message.
mkNodeAnnouncement
  :: Signature -> FeatureBits -> NodeId -> RgbColor -> Alias
  -> [Address] -> NodeAnnouncement
mkNodeAnnouncement !sig !feat !nid !col !al !addrs = NodeAnnouncement
  { nodeAnnSignature = sig
  , nodeAnnFeatures  = feat
  , nodeAnnTimestamp = 1234567890
  , nodeAnnNodeId    = nid
  , nodeAnnRgbColor  = col
  , nodeAnnAlias     = al
  , nodeAnnAddresses = addrs
  }

-- | Construct ChannelUpdate message.
mkChannelUpdate :: Signature -> ChainHash -> ShortChannelId -> ChannelUpdate
mkChannelUpdate !sig !ch !scid = ChannelUpdate
  { chanUpdateSignature       = sig
  , chanUpdateChainHash       = ch
  , chanUpdateShortChanId     = scid
  , chanUpdateTimestamp       = 1234567890
  , chanUpdateMsgFlags        = MessageFlags True
  , chanUpdateChanFlags       = ChannelFlags False False
  , chanUpdateCltvExpDelta    = CltvExpiryDelta 144
  , chanUpdateHtlcMinMsat     = HtlcMinimumMsat 1000
  , chanUpdateFeeBaseMsat     = FeeBaseMsat 1000
  , chanUpdateFeeProportional = FeeProportionalMillionths 100
  , chanUpdateHtlcMaxMsat     = Just (HtlcMaximumMsat 1000000000)
  }

-- | Construct AnnouncementSignatures message.
mkAnnouncementSignatures
  :: ChannelId -> ShortChannelId -> Signature -> AnnouncementSignatures
mkAnnouncementSignatures !cid !scid !sig = AnnouncementSignatures
  { annSigChannelId   = cid
  , annSigShortChanId = scid
  , annSigNodeSig     = sig
  , annSigBitcoinSig  = sig
  }

-- | Construct GossipTimestampFilter message.
mkGossipTimestampFilter :: ChainHash -> GossipTimestampFilter
mkGossipTimestampFilter !ch = GossipTimestampFilter
  { gossipFilterChainHash      = ch
  , gossipFilterFirstTimestamp = 1609459200
  , gossipFilterTimestampRange = 86400
  }

-- | Construct QueryChannelRange message.
mkQueryChannelRange :: ChainHash -> TlvStream -> QueryChannelRange
mkQueryChannelRange !ch !tlvs = QueryChannelRange
  { queryRangeChainHash  = ch
  , queryRangeFirstBlock = 700000
  , queryRangeNumBlocks  = 10000
  , queryRangeTlvs       = tlvs
  }

-- | Construct ReplyChannelRange message.
mkReplyChannelRange :: ChainHash -> TlvStream -> ReplyChannelRange
mkReplyChannelRange !ch !tlvs = ReplyChannelRange
  { replyRangeChainHash    = ch
  , replyRangeFirstBlock   = 700000
  , replyRangeNumBlocks    = 10000
  , replyRangeSyncComplete = 1
  , replyRangeData         = encodeShortChannelIdList [testShortChannelId]
  , replyRangeTlvs         = tlvs
  }

-- Pre-constructed messages ----------------------------------------------------

-- | Test ChannelAnnouncement message.
testChannelAnnouncement :: ChannelAnnouncement
testChannelAnnouncement = mkChannelAnnouncement
  testSignature testSignature testChainHash testShortChannelId
  testNodeId2 testNodeId testPoint testPoint emptyFeatures
{-# NOINLINE testChannelAnnouncement #-}

-- | Encoded ChannelAnnouncement for decode benchmarks.
encodedChannelAnnouncement :: BS.ByteString
encodedChannelAnnouncement = encodeChannelAnnouncement testChannelAnnouncement
{-# NOINLINE encodedChannelAnnouncement #-}

-- | Test NodeAnnouncement message.
testNodeAnnouncement :: NodeAnnouncement
testNodeAnnouncement = mkNodeAnnouncement
  testSignature emptyFeatures testNodeId testRgbColor testAlias
  [AddrIPv4 testIPv4 9735]
{-# NOINLINE testNodeAnnouncement #-}

-- | Encoded NodeAnnouncement for decode benchmarks.
encodedNodeAnnouncement :: BS.ByteString
encodedNodeAnnouncement = case encodeNodeAnnouncement testNodeAnnouncement of
  Right bs -> bs
  Left _   -> error "encodedNodeAnnouncement: encode failed"
{-# NOINLINE encodedNodeAnnouncement #-}

-- | Test ChannelUpdate message.
testChannelUpdate :: ChannelUpdate
testChannelUpdate = mkChannelUpdate testSignature testChainHash testShortChannelId
{-# NOINLINE testChannelUpdate #-}

-- | Encoded ChannelUpdate for decode benchmarks.
encodedChannelUpdate :: BS.ByteString
encodedChannelUpdate = encodeChannelUpdate testChannelUpdate
{-# NOINLINE encodedChannelUpdate #-}

-- | Test AnnouncementSignatures message.
testAnnouncementSignatures :: AnnouncementSignatures
testAnnouncementSignatures =
  mkAnnouncementSignatures testChannelId testShortChannelId testSignature
{-# NOINLINE testAnnouncementSignatures #-}

-- | Encoded AnnouncementSignatures for decode benchmarks.
encodedAnnouncementSignatures :: BS.ByteString
encodedAnnouncementSignatures =
  encodeAnnouncementSignatures testAnnouncementSignatures
{-# NOINLINE encodedAnnouncementSignatures #-}

-- | Test GossipTimestampFilter message.
testGossipTimestampFilter :: GossipTimestampFilter
testGossipTimestampFilter = mkGossipTimestampFilter testChainHash
{-# NOINLINE testGossipTimestampFilter #-}

-- | Encoded GossipTimestampFilter for decode benchmarks.
encodedGossipTimestampFilter :: BS.ByteString
encodedGossipTimestampFilter =
  encodeGossipTimestampFilter testGossipTimestampFilter
{-# NOINLINE encodedGossipTimestampFilter #-}

-- | Test QueryChannelRange message.
testQueryChannelRange :: QueryChannelRange
testQueryChannelRange = mkQueryChannelRange testChainHash emptyTlvs
{-# NOINLINE testQueryChannelRange #-}

-- | Encoded QueryChannelRange for decode benchmarks.
encodedQueryChannelRange :: BS.ByteString
encodedQueryChannelRange = encodeQueryChannelRange testQueryChannelRange
{-# NOINLINE encodedQueryChannelRange #-}

-- | Test ReplyChannelRange message.
testReplyChannelRange :: ReplyChannelRange
testReplyChannelRange = mkReplyChannelRange testChainHash emptyTlvs
{-# NOINLINE testReplyChannelRange #-}

-- | Encoded ReplyChannelRange for decode benchmarks.
encodedReplyChannelRange :: BS.ByteString
encodedReplyChannelRange = case encodeReplyChannelRange testReplyChannelRange of
  Right bs -> bs
  Left _   -> error "encodedReplyChannelRange: encode failed"
{-# NOINLINE encodedReplyChannelRange #-}

-- Weigh benchmarks ------------------------------------------------------------

main :: IO ()
main = mainWith $ do
  wgroup "channel_announcement" $ do
    func "construct" (mkChannelAnnouncement
      testSignature testSignature testChainHash testShortChannelId
      testNodeId2 testNodeId testPoint testPoint) emptyFeatures
    func "encode" encodeChannelAnnouncement testChannelAnnouncement
    func "decode" decodeChannelAnnouncement encodedChannelAnnouncement

  wgroup "node_announcement" $ do
    func "construct" (mkNodeAnnouncement
      testSignature emptyFeatures testNodeId testRgbColor testAlias)
      [AddrIPv4 testIPv4 9735]
    func "encode" encodeNodeAnnouncement testNodeAnnouncement
    func "decode" decodeNodeAnnouncement encodedNodeAnnouncement

  wgroup "channel_update" $ do
    func "construct" (mkChannelUpdate testSignature testChainHash)
      testShortChannelId
    func "encode" encodeChannelUpdate testChannelUpdate
    func "decode" decodeChannelUpdate encodedChannelUpdate

  wgroup "announcement_signatures" $ do
    func "construct" (mkAnnouncementSignatures testChannelId testShortChannelId)
      testSignature
    func "encode" encodeAnnouncementSignatures testAnnouncementSignatures
    func "decode" decodeAnnouncementSignatures encodedAnnouncementSignatures

  wgroup "query_channel_range" $ do
    func "construct" (mkQueryChannelRange testChainHash) emptyTlvs
    func "encode" encodeQueryChannelRange testQueryChannelRange
    func "decode" decodeQueryChannelRange encodedQueryChannelRange

  wgroup "reply_channel_range" $ do
    func "construct" (mkReplyChannelRange testChainHash) emptyTlvs
    func "encode" encodeReplyChannelRange testReplyChannelRange
    func "decode" decodeReplyChannelRange encodedReplyChannelRange

  wgroup "gossip_timestamp_filter" $ do
    func "construct" mkGossipTimestampFilter testChainHash
    func "encode" encodeGossipTimestampFilter testGossipTimestampFilter
    func "decode" decodeGossipTimestampFilter encodedGossipTimestampFilter

  wgroup "scid_list" $ do
    func "encode (100)" encodeShortChannelIdList testScidList
    func "decode (100)" decodeShortChannelIdList encodedScidList

  wgroup "hash" $ do
    func "channelAnnouncementHash" channelAnnouncementHash
      encodedChannelAnnouncement
    func "nodeAnnouncementHash" nodeAnnouncementHash encodedNodeAnnouncement
    func "channelUpdateHash" channelUpdateHash encodedChannelUpdate
    func "channelUpdateChecksum" channelUpdateChecksum encodedChannelUpdate

  wgroup "validate" $ do
    func "channelAnnouncement" validateChannelAnnouncement testChannelAnnouncement
    func "nodeAnnouncement" validateNodeAnnouncement testNodeAnnouncement
    func "channelUpdate" validateChannelUpdate testChannelUpdate
    func "queryChannelRange" validateQueryChannelRange testQueryChannelRange
    func "replyChannelRange" validateReplyChannelRange testReplyChannelRange
