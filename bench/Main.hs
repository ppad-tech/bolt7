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

-- | Empty TLV stream.
emptyTlvs :: TlvStream
emptyTlvs = unsafeTlvStream []
{-# NOINLINE emptyTlvs #-}

-- | Empty feature bits.
emptyFeatures :: FeatureBits
emptyFeatures = featureBits BS.empty
{-# NOINLINE emptyFeatures #-}

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
  , channelAnnNodeId1      = testNodeId
  , channelAnnNodeId2      = testNodeId2
  , channelAnnBitcoinKey1  = testPoint
  , channelAnnBitcoinKey2  = testPoint
  }
{-# NOINLINE testChannelAnnouncement #-}

-- | Encoded ChannelAnnouncement for decode benchmarks.
encodedChannelAnnouncement :: BS.ByteString
encodedChannelAnnouncement = encodeChannelAnnouncement testChannelAnnouncement
{-# NOINLINE encodedChannelAnnouncement #-}

-- | Test ChannelUpdate message.
testChannelUpdate :: ChannelUpdate
testChannelUpdate = ChannelUpdate
  { chanUpdateSignature       = testSignature
  , chanUpdateChainHash       = testChainHash
  , chanUpdateShortChanId     = testShortChannelId
  , chanUpdateTimestamp       = 1234567890
  , chanUpdateMsgFlags        = 0x01
  , chanUpdateChanFlags       = 0x00
  , chanUpdateCltvExpDelta    = 144
  , chanUpdateHtlcMinMsat     = 1000
  , chanUpdateFeeBaseMsat     = 1000
  , chanUpdateFeeProportional = 100
  , chanUpdateHtlcMaxMsat     = Just 1000000000
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
  , bgroup "gossip_timestamp_filter"
      [ bench "encode" $
          nf encodeGossipTimestampFilter testGossipTimestampFilter
      , bench "decode" $
          nf decodeGossipTimestampFilter encodedGossipTimestampFilter
      ]
  ]
