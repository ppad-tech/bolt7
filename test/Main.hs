{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Data.Word (Word16, Word32)
import Lightning.Protocol.BOLT1 (TlvStream, unsafeTlvStream)
import Lightning.Protocol.BOLT7
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "ppad-bolt7" [
    type_tests
  , channel_announcement_tests
  , node_announcement_tests
  , channel_update_tests
  , announcement_signatures_tests
  , query_tests
  , error_tests
  , property_tests
  ]

-- Test data helpers -----------------------------------------------------------

-- | Create a valid ChainHash (32 bytes).
testChainHash :: ChainHash
testChainHash = fromJust $ chainHash (BS.replicate 32 0x01)

-- | Create a valid ShortChannelId (8 bytes).
testShortChannelId :: ShortChannelId
testShortChannelId = fromJust $ shortChannelId (BS.replicate 8 0xab)

-- | Create a valid ChannelId (32 bytes).
testChannelId :: ChannelId
testChannelId = fromJust $ channelId (BS.replicate 32 0xcd)

-- | Create a valid Signature (64 bytes).
testSignature :: Signature
testSignature = fromJust $ signature (BS.replicate 64 0xee)

-- | Create a valid Point (33 bytes).
testPoint :: Point
testPoint = fromJust $ point (BS.pack $ 0x02 : replicate 32 0xff)

-- | Create a valid NodeId (33 bytes).
testNodeId :: NodeId
testNodeId = fromJust $ nodeId (BS.pack $ 0x03 : replicate 32 0xaa)

-- | Create a second valid NodeId (33 bytes).
testNodeId2 :: NodeId
testNodeId2 = fromJust $ nodeId (BS.pack $ 0x02 : replicate 32 0xbb)

-- | Create a valid RgbColor (3 bytes).
testRgbColor :: RgbColor
testRgbColor = fromJust $ rgbColor (BS.pack [0xff, 0x00, 0x00])

-- | Create a valid Alias (32 bytes).
testAlias :: Alias
testAlias = fromJust $ alias (BS.pack $ replicate 32 0x00)

-- | Empty TLV stream for messages.
emptyTlvs :: TlvStream
emptyTlvs = unsafeTlvStream []

-- | Empty feature bits.
emptyFeatures :: FeatureBits
emptyFeatures = featureBits BS.empty

-- Type Tests ------------------------------------------------------------------

type_tests :: TestTree
type_tests = testGroup "Types" [
    testGroup "ShortChannelId" [
      testCase "scidBlockHeight" $ do
        -- 8 bytes: block=0x123456, tx=0x789abc, output=0xdef0
        let scid = fromJust $ shortChannelId (BS.pack
              [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0])
        scidBlockHeight scid @?= 0x123456
    , testCase "scidTxIndex" $ do
        let scid = fromJust $ shortChannelId (BS.pack
              [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0])
        scidTxIndex scid @?= 0x789abc
    , testCase "scidOutputIndex" $ do
        let scid = fromJust $ shortChannelId (BS.pack
              [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0])
        scidOutputIndex scid @?= 0xdef0
    ]
  , testGroup "Smart constructors" [
      testCase "chainHash rejects wrong length" $ do
        chainHash (BS.replicate 31 0x00) @?= Nothing
        chainHash (BS.replicate 33 0x00) @?= Nothing
    , testCase "shortChannelId rejects wrong length" $ do
        shortChannelId (BS.replicate 7 0x00) @?= Nothing
        shortChannelId (BS.replicate 9 0x00) @?= Nothing
    , testCase "signature rejects wrong length" $ do
        signature (BS.replicate 63 0x00) @?= Nothing
        signature (BS.replicate 65 0x00) @?= Nothing
    , testCase "point rejects wrong length" $ do
        point (BS.replicate 32 0x00) @?= Nothing
        point (BS.replicate 34 0x00) @?= Nothing
    ]
  ]

-- Channel Announcement Tests --------------------------------------------------

channel_announcement_tests :: TestTree
channel_announcement_tests = testGroup "ChannelAnnouncement" [
    testCase "encode/decode roundtrip" $ do
      let msg = ChannelAnnouncement
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
          encoded = encodeChannelAnnouncement msg
      case decodeChannelAnnouncement encoded of
        Right (decoded, _) -> decoded @?= msg
        Left e -> assertFailure $ "decode failed: " ++ show e
  ]

-- Node Announcement Tests -----------------------------------------------------

node_announcement_tests :: TestTree
node_announcement_tests = testGroup "NodeAnnouncement" [
    testCase "encode/decode roundtrip with no addresses" $ do
      let msg = NodeAnnouncement
            { nodeAnnSignature = testSignature
            , nodeAnnFeatures  = emptyFeatures
            , nodeAnnTimestamp = 1234567890
            , nodeAnnNodeId    = testNodeId
            , nodeAnnRgbColor  = testRgbColor
            , nodeAnnAlias     = testAlias
            , nodeAnnAddresses = []
            }
      case encodeNodeAnnouncement msg of
        Left e -> assertFailure $ "encode failed: " ++ show e
        Right encoded -> case decodeNodeAnnouncement encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
  , testCase "encode/decode roundtrip with IPv4 address" $ do
      let ipv4 = fromJust $ ipv4Addr (BS.pack [127, 0, 0, 1])
          msg = NodeAnnouncement
            { nodeAnnSignature = testSignature
            , nodeAnnFeatures  = emptyFeatures
            , nodeAnnTimestamp = 1234567890
            , nodeAnnNodeId    = testNodeId
            , nodeAnnRgbColor  = testRgbColor
            , nodeAnnAlias     = testAlias
            , nodeAnnAddresses = [AddrIPv4 ipv4 9735]
            }
      case encodeNodeAnnouncement msg of
        Left e -> assertFailure $ "encode failed: " ++ show e
        Right encoded -> case decodeNodeAnnouncement encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
  ]

-- Channel Update Tests --------------------------------------------------------

channel_update_tests :: TestTree
channel_update_tests = testGroup "ChannelUpdate" [
    testCase "encode/decode roundtrip without htlc_maximum_msat" $ do
      let msg = ChannelUpdate
            { chanUpdateSignature      = testSignature
            , chanUpdateChainHash      = testChainHash
            , chanUpdateShortChanId    = testShortChannelId
            , chanUpdateTimestamp      = 1234567890
            , chanUpdateMsgFlags       = 0x00
            , chanUpdateChanFlags      = 0x01
            , chanUpdateCltvExpDelta   = 144
            , chanUpdateHtlcMinMsat    = 1000
            , chanUpdateFeeBaseMsat    = 1000
            , chanUpdateFeeProportional = 100
            , chanUpdateHtlcMaxMsat    = Nothing
            }
          encoded = encodeChannelUpdate msg
      case decodeChannelUpdate encoded of
        Right (decoded, _) -> decoded @?= msg
        Left e -> assertFailure $ "decode failed: " ++ show e
  , testCase "encode/decode roundtrip with htlc_maximum_msat" $ do
      let msg = ChannelUpdate
            { chanUpdateSignature      = testSignature
            , chanUpdateChainHash      = testChainHash
            , chanUpdateShortChanId    = testShortChannelId
            , chanUpdateTimestamp      = 1234567890
            , chanUpdateMsgFlags       = 0x01  -- bit 0 set
            , chanUpdateChanFlags      = 0x00
            , chanUpdateCltvExpDelta   = 40
            , chanUpdateHtlcMinMsat    = 1000
            , chanUpdateFeeBaseMsat    = 500
            , chanUpdateFeeProportional = 50
            , chanUpdateHtlcMaxMsat    = Just 1000000000
            }
          encoded = encodeChannelUpdate msg
      case decodeChannelUpdate encoded of
        Right (decoded, _) -> decoded @?= msg
        Left e -> assertFailure $ "decode failed: " ++ show e
  ]

-- Announcement Signatures Tests -----------------------------------------------

announcement_signatures_tests :: TestTree
announcement_signatures_tests = testGroup "AnnouncementSignatures" [
    testCase "encode/decode roundtrip" $ do
      let msg = AnnouncementSignatures
            { annSigChannelId   = testChannelId
            , annSigShortChanId = testShortChannelId
            , annSigNodeSig     = testSignature
            , annSigBitcoinSig  = testSignature
            }
          encoded = encodeAnnouncementSignatures msg
      case decodeAnnouncementSignatures encoded of
        Right (decoded, _) -> decoded @?= msg
        Left e -> assertFailure $ "decode failed: " ++ show e
  ]

-- Query Tests -----------------------------------------------------------------

query_tests :: TestTree
query_tests = testGroup "Query Messages" [
    testGroup "QueryShortChannelIds" [
      testCase "encode/decode roundtrip" $ do
        let msg = QueryShortChannelIds
              { queryScidsChainHash = testChainHash
              , queryScidsData      = BS.replicate 24 0xab  -- 3 SCIDs
              , queryScidsTlvs      = emptyTlvs
              }
        case encodeQueryShortChannelIds msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeQueryShortChannelIds encoded of
            Right (decoded, _) -> do
              queryScidsChainHash decoded @?= queryScidsChainHash msg
              queryScidsData decoded @?= queryScidsData msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "ReplyShortChannelIdsEnd" [
      testCase "encode/decode roundtrip" $ do
        let msg = ReplyShortChannelIdsEnd
              { replyScidsChainHash = testChainHash
              , replyScidsFullInfo  = 1
              }
            encoded = encodeReplyShortChannelIdsEnd msg
        case decodeReplyShortChannelIdsEnd encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "QueryChannelRange" [
      testCase "encode/decode roundtrip" $ do
        let msg = QueryChannelRange
              { queryRangeChainHash  = testChainHash
              , queryRangeFirstBlock = 600000
              , queryRangeNumBlocks  = 10000
              , queryRangeTlvs       = emptyTlvs
              }
            encoded = encodeQueryChannelRange msg
        case decodeQueryChannelRange encoded of
          Right (decoded, _) -> do
            queryRangeChainHash decoded @?= queryRangeChainHash msg
            queryRangeFirstBlock decoded @?= queryRangeFirstBlock msg
            queryRangeNumBlocks decoded @?= queryRangeNumBlocks msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "ReplyChannelRange" [
      testCase "encode/decode roundtrip" $ do
        let msg = ReplyChannelRange
              { replyRangeChainHash    = testChainHash
              , replyRangeFirstBlock   = 600000
              , replyRangeNumBlocks    = 10000
              , replyRangeSyncComplete = 1
              , replyRangeData         = BS.replicate 16 0xcd
              , replyRangeTlvs         = emptyTlvs
              }
        case encodeReplyChannelRange msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeReplyChannelRange encoded of
            Right (decoded, _) -> do
              replyRangeChainHash decoded @?= replyRangeChainHash msg
              replyRangeFirstBlock decoded @?= replyRangeFirstBlock msg
              replyRangeNumBlocks decoded @?= replyRangeNumBlocks msg
              replyRangeSyncComplete decoded @?= replyRangeSyncComplete msg
              replyRangeData decoded @?= replyRangeData msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "GossipTimestampFilter" [
      testCase "encode/decode roundtrip" $ do
        let msg = GossipTimestampFilter
              { gossipFilterChainHash      = testChainHash
              , gossipFilterFirstTimestamp = 1609459200
              , gossipFilterTimestampRange = 86400
              }
            encoded = encodeGossipTimestampFilter msg
        case decodeGossipTimestampFilter encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  ]

-- Error Tests -----------------------------------------------------------------

error_tests :: TestTree
error_tests = testGroup "Error Conditions" [
    testGroup "Insufficient Bytes" [
      testCase "decodeChannelAnnouncement empty" $ do
        case decodeChannelAnnouncement BS.empty of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeChannelUpdate too short" $ do
        case decodeChannelUpdate (BS.replicate 50 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeAnnouncementSignatures too short" $ do
        case decodeAnnouncementSignatures (BS.replicate 50 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeGossipTimestampFilter too short" $ do
        case decodeGossipTimestampFilter (BS.replicate 30 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    ]
  ]

-- Property Tests --------------------------------------------------------------

property_tests :: TestTree
property_tests = testGroup "Properties" [
    testProperty "ChannelAnnouncement roundtrip" propChannelAnnouncementRoundtrip
  , testProperty "ChannelUpdate roundtrip" propChannelUpdateRoundtrip
  , testProperty "AnnouncementSignatures roundtrip"
      propAnnouncementSignaturesRoundtrip
  , testProperty "GossipTimestampFilter roundtrip"
      propGossipTimestampFilterRoundtrip
  ]

-- Property: ChannelAnnouncement roundtrip
propChannelAnnouncementRoundtrip :: Property
propChannelAnnouncementRoundtrip = property $ do
  let msg = ChannelAnnouncement
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
      encoded = encodeChannelAnnouncement msg
  case decodeChannelAnnouncement encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: ChannelUpdate roundtrip
propChannelUpdateRoundtrip :: Word32 -> Word16 -> Property
propChannelUpdateRoundtrip timestamp cltvDelta = property $ do
  let msg = ChannelUpdate
        { chanUpdateSignature      = testSignature
        , chanUpdateChainHash      = testChainHash
        , chanUpdateShortChanId    = testShortChannelId
        , chanUpdateTimestamp      = timestamp
        , chanUpdateMsgFlags       = 0x00
        , chanUpdateChanFlags      = 0x00
        , chanUpdateCltvExpDelta   = cltvDelta
        , chanUpdateHtlcMinMsat    = 1000
        , chanUpdateFeeBaseMsat    = 1000
        , chanUpdateFeeProportional = 100
        , chanUpdateHtlcMaxMsat    = Nothing
        }
      encoded = encodeChannelUpdate msg
  case decodeChannelUpdate encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: AnnouncementSignatures roundtrip
propAnnouncementSignaturesRoundtrip :: Property
propAnnouncementSignaturesRoundtrip = property $ do
  let msg = AnnouncementSignatures
        { annSigChannelId   = testChannelId
        , annSigShortChanId = testShortChannelId
        , annSigNodeSig     = testSignature
        , annSigBitcoinSig  = testSignature
        }
      encoded = encodeAnnouncementSignatures msg
  case decodeAnnouncementSignatures encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: GossipTimestampFilter roundtrip
propGossipTimestampFilterRoundtrip :: Word32 -> Word32 -> Property
propGossipTimestampFilterRoundtrip firstTs tsRange = property $ do
  let msg = GossipTimestampFilter
        { gossipFilterChainHash      = testChainHash
        , gossipFilterFirstTimestamp = firstTs
        , gossipFilterTimestampRange = tsRange
        }
      encoded = encodeGossipTimestampFilter msg
  case decodeGossipTimestampFilter encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False
