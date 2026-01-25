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
testShortChannelId = case shortChannelId zeroBytes8 of
  Just s  -> s
  Nothing -> error "testShortChannelId: invalid"
{-# NOINLINE testShortChannelId #-}

-- | 32-byte channel ID.
testChannelId :: ChannelId
testChannelId = case channelId zeroBytes32 of
  Just c  -> c
  Nothing -> error "testChannelId: invalid"
{-# NOINLINE testChannelId #-}

-- | 33-byte node ID.
testNodeId :: NodeId
testNodeId = case nodeId (BS.cons 0x03 zeroBytes32) of
  Just n  -> n
  Nothing -> error "testNodeId: invalid"
{-# NOINLINE testNodeId #-}

-- | Second node ID.
testNodeId2 :: NodeId
testNodeId2 = case nodeId (BS.cons 0x02 zeroBytes32) of
  Just n  -> n
  Nothing -> error "testNodeId2: invalid"
{-# NOINLINE testNodeId2 #-}

-- | Empty feature bits.
emptyFeatures :: FeatureBits
emptyFeatures = featureBits BS.empty
{-# NOINLINE emptyFeatures #-}

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

-- | Construct ChannelUpdate message.
mkChannelUpdate :: Signature -> ChainHash -> ShortChannelId -> ChannelUpdate
mkChannelUpdate !sig !ch !scid = ChannelUpdate
  { chanUpdateSignature       = sig
  , chanUpdateChainHash       = ch
  , chanUpdateShortChanId     = scid
  , chanUpdateTimestamp       = 1234567890
  , chanUpdateMsgFlags        = 0x01
  , chanUpdateChanFlags       = 0x00
  , chanUpdateCltvExpDelta    = 144
  , chanUpdateHtlcMinMsat     = 1000
  , chanUpdateFeeBaseMsat     = 1000
  , chanUpdateFeeProportional = 100
  , chanUpdateHtlcMaxMsat     = Just 1000000000
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

-- Pre-constructed messages ----------------------------------------------------

-- | Test ChannelAnnouncement message.
testChannelAnnouncement :: ChannelAnnouncement
testChannelAnnouncement = mkChannelAnnouncement
  testSignature testSignature testChainHash testShortChannelId
  testNodeId testNodeId2 testPoint testPoint emptyFeatures
{-# NOINLINE testChannelAnnouncement #-}

-- | Encoded ChannelAnnouncement for decode benchmarks.
encodedChannelAnnouncement :: BS.ByteString
encodedChannelAnnouncement = encodeChannelAnnouncement testChannelAnnouncement
{-# NOINLINE encodedChannelAnnouncement #-}

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

-- Weigh benchmarks ------------------------------------------------------------

main :: IO ()
main = mainWith $ do
  wgroup "channel_announcement" $ do
    func "construct" (mkChannelAnnouncement
      testSignature testSignature testChainHash testShortChannelId
      testNodeId testNodeId2 testPoint testPoint) emptyFeatures
    func "encode" encodeChannelAnnouncement testChannelAnnouncement
    func "decode" decodeChannelAnnouncement encodedChannelAnnouncement

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

  wgroup "gossip_timestamp_filter" $ do
    func "construct" mkGossipTimestampFilter testChainHash
    func "encode" encodeGossipTimestampFilter testGossipTimestampFilter
    func "decode" decodeGossipTimestampFilter encodedGossipTimestampFilter
