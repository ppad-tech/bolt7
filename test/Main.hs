{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Data.Word (Word8, Word16, Word32)
import Lightning.Protocol.BOLT1
  (TlvRecord(..), TlvStream, unTlvStream, unsafeTlvStream)
import Lightning.Protocol.BOLT7
import Lightning.Protocol.BOLT7.CRC32C (crc32c)
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
  , scid_list_tests
  , hash_tests
  , validation_tests
  , error_tests
  , property_tests
  ]

-- Test data helpers -----------------------------------------------------------

-- | Create a valid ChainHash (32 bytes).
testChainHash :: ChainHash
testChainHash = fromJust $ chainHash (BS.replicate 32 0x01)

-- | Create a valid ShortChannelId (8 bytes).
testShortChannelId :: ShortChannelId
testShortChannelId = fromJust $ scidFromBytes (BS.replicate 8 0xab)

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

-- | Total ShortChannelId constructor for test fixtures.
mkScid :: Word32 -> Word32 -> Word16 -> ShortChannelId
mkScid b t o = case shortChannelId b t o of
  Just s  -> s
  Nothing -> error "mkScid: invalid test fixture"

-- | A channel_update fixture.
testChannelUpdate :: ChannelUpdate
testChannelUpdate = ChannelUpdate
  { chanUpdateSignature       = testSignature
  , chanUpdateChainHash       = testChainHash
  , chanUpdateShortChanId     = testShortChannelId
  , chanUpdateTimestamp       = 1234567890
  , chanUpdateChanFlags       = ChannelFlags
      { cfDirection = NodeOne, cfStatus = Enabled }
  , chanUpdateCltvExpDelta    = CltvExpiryDelta 144
  , chanUpdateHtlcMinMsat     = HtlcMinimumMsat 1000
  , chanUpdateFeeBaseMsat     = FeeBaseMsat 1000
  , chanUpdateFeeProportional = FeeProportionalMillionths 100
  , chanUpdateHtlcMaxMsat     = HtlcMaximumMsat 1000000000
  }

-- | Unwrap a Maybe, failing the test on Nothing.
expectJust :: String -> Maybe a -> IO a
expectJust what = maybe (assertFailure ("unexpected Nothing: " ++ what)) pure

-- | A node_announcement fixture with the given addresses.
testNodeAnn :: [Address] -> NodeAnnouncement
testNodeAnn addrs = NodeAnnouncement
  { nodeAnnSignature = testSignature
  , nodeAnnFeatures  = emptyFeatures
  , nodeAnnTimestamp = 1234567890
  , nodeAnnNodeId    = testNodeId
  , nodeAnnRgbColor  = testRgbColor
  , nodeAnnAlias     = testAlias
  , nodeAnnAddresses = addrs
  }

-- | A channel_announcement fixture with the given features.
testChannelAnn :: FeatureBits -> ChannelAnnouncement
testChannelAnn feats = ChannelAnnouncement
  { channelAnnNodeSig1    = testSignature
  , channelAnnNodeSig2    = testSignature
  , channelAnnBitcoinSig1 = testSignature
  , channelAnnBitcoinSig2 = testSignature
  , channelAnnFeatures    = feats
  , channelAnnChainHash   = testChainHash
  , channelAnnShortChanId = testShortChannelId
  , channelAnnNodeId1     = testNodeId2
  , channelAnnNodeId2     = testNodeId
  , channelAnnBitcoinKey1 = testPoint
  , channelAnnBitcoinKey2 = testPoint
  }

-- | Replace the byte at an offset (no-op past the end).
setByte :: Int -> Word8 -> BS.ByteString -> BS.ByteString
setByte i w bs
  | i < 0 || i >= BS.length bs = bs
  | otherwise = BS.take i bs <> BS.singleton w <> BS.drop (i + 1) bs

-- Type Tests ------------------------------------------------------------------

type_tests :: TestTree
type_tests = testGroup "Types" [
    testGroup "ShortChannelId" [
      testCase "scidBlockHeight" $ do
        -- 8 bytes: block=0x123456, tx=0x789abc, output=0xdef0
        let scid = fromJust $ scidFromBytes (BS.pack
              [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0])
        scidBlockHeight scid @?= 0x123456
    , testCase "scidTxIndex" $ do
        let scid = fromJust $ scidFromBytes (BS.pack
              [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0])
        scidTxIndex scid @?= 0x789abc
    , testCase "scidOutputIndex" $ do
        let scid = fromJust $ scidFromBytes (BS.pack
              [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0])
        scidOutputIndex scid @?= 0xdef0
    , testCase "shortChannelId roundtrip" $ do
        let scid = fromJust $ shortChannelId 539268 845 1
        scidBlockHeight scid @?= 539268
        scidTxIndex scid @?= 845
        scidOutputIndex scid @?= 1
    , testCase "formatScid" $ do
        let scid = fromJust $ shortChannelId 539268 845 1
        formatScid scid @?= "539268x845x1"
    , testCase "formatScid zero values" $ do
        let scid = fromJust $ shortChannelId 0 0 0
        formatScid scid @?= "0x0x0"
    ]
  , testGroup "Smart constructors" [
      testCase "chainHash rejects wrong length" $ do
        chainHash (BS.replicate 31 0x00) @?= Nothing
        chainHash (BS.replicate 33 0x00) @?= Nothing
    , testCase "scidFromBytes rejects wrong length" $ do
        scidFromBytes (BS.replicate 7 0x00) @?= Nothing
        scidFromBytes (BS.replicate 9 0x00) @?= Nothing
    , testCase "signature rejects wrong length" $ do
        signature (BS.replicate 63 0x00) @?= Nothing
        signature (BS.replicate 65 0x00) @?= Nothing
    , testCase "point rejects wrong length" $ do
        point (BS.replicate 32 0x00) @?= Nothing
        point (BS.replicate 34 0x00) @?= Nothing
    ]
  , testGroup "Constants" [
      testCase "mainnetChainHash has correct length" $ do
        BS.length (unChainHash mainnetChainHash) @?= 32
    ]
  , testGroup "NodeId ordering" [
      testCase "NodeId Ord is lexicographic" $ do
        let n1 = fromJust $ nodeId (BS.pack $ 0x02 : replicate 32 0x00)
            n2 = fromJust $ nodeId (BS.pack $ 0x03 : replicate 32 0x00)
        n1 < n2 @?= True
        n2 < n1 @?= False
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
      case encodeChannelAnnouncement msg of
        Left e -> assertFailure $ "encode failed: " ++ show e
        Right encoded -> case decodeChannelAnnouncement encoded of
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
  , testGroup "address descriptors" [
      testCase "skips deprecated Tor v2 descriptors" $ do
        v4 <- addrV4
        v6 <- addrV6
        decodeAddrs (descV4 <> descTorV2 <> descV6) @?= Right [v4, v6]
    , testCase "stops at the first unknown type" $ do
        v4 <- addrV4
        let wire = descV4 <> BS.pack [6, 1, 2, 3] <> descV6
        decodeAddrs wire @?= Right [v4]
        -- the ignored bytes are still consumed as part of addrlen
        fmap snd (decodeNodeAnnouncement (nodeAnnWire wire))
          @?= Right BS.empty
    , testCase "stops at a type-0 descriptor" $
        decodeAddrs (BS.singleton 0 <> descV4) @?= Right []
    , testCase "skips descriptors with port 0" $ do
        v6 <- addrV6
        let v4port0 = BS.pack [1, 127, 0, 0, 1, 0, 0]
        decodeAddrs (v4port0 <> descV6) @?= Right [v6]
    , testCase "skips empty and non-ASCII hostnames" $ do
        h <- expectJust "hostname" (hostname "ln.example.com")
        let wire = descDns "" <> descDns "\xff\&ab"
              <> descDns "ln.example.com"
        decodeAddrs wire @?= Right [AddrDNS h 9735]
    , testCase "rejects a truncated descriptor of a known type" $ do
        decodeAddrs (BS.pack [1, 127, 0, 0])
          @?= Left DecodeInsufficientBytes
        decodeAddrs (BS.pack [3, 1, 2, 3])
          @?= Left DecodeInsufficientBytes
        decodeAddrs (BS.pack [5, 4, 0x61])
          @?= Left DecodeInsufficientBytes
    , testCase "hostname rejects non-ASCII bytes" $ do
        hostname "\0\255" @?= Nothing
        hostname "caf\233" @?= Nothing
        fmap getHostname (hostname "xn--caf-dma") @?= Just "xn--caf-dma"
    ]
  ]

-- | node_announcement payload carrying the given address bytes.
nodeAnnWire :: BS.ByteString -> BS.ByteString
nodeAnnWire addrs = mconcat
  [ BS.replicate 64 0xee              -- signature
  , BS.pack [0, 0]                    -- flen
  , BS.pack [0x49, 0x96, 0x02, 0xd2]  -- timestamp
  , getNodeId testNodeId
  , getRgbColor testRgbColor
  , getAlias testAlias
  , u16be (BS.length addrs)
  , addrs
  ]

-- | Decode the addresses of a node_announcement built by 'nodeAnnWire'.
decodeAddrs :: BS.ByteString -> Either DecodeError [Address]
decodeAddrs = fmap (nodeAnnAddresses . fst) . decodeNodeAnnouncement
  . nodeAnnWire

-- | Big-endian u16 (low 16 bits).
u16be :: Int -> BS.ByteString
u16be n = BS.pack [fromIntegral (n `div` 256), fromIntegral n]

-- | Address descriptors (port 9735) and the values they decode to.
descV4, descV6, descTorV2 :: BS.ByteString
descV4 = BS.pack [1, 127, 0, 0, 1, 0x26, 0x07]
descV6 = BS.pack (2 : replicate 15 0 ++ [1, 0x26, 0x07])
descTorV2 = BS.cons 3 (BS.replicate 12 0xaa)

descDns :: BS.ByteString -> BS.ByteString
descDns h = BS.pack [5, fromIntegral (BS.length h)] <> h <> u16be 9735

addrV4, addrV6 :: IO Address
addrV4 = fmap (`AddrIPv4` 9735)
  (expectJust "ipv4" (ipv4Addr (BS.pack [127, 0, 0, 1])))
addrV6 = fmap (`AddrIPv6` 9735)
  (expectJust "ipv6" (ipv6Addr (BS.pack (replicate 15 0 ++ [1]))))

-- Channel Update Tests --------------------------------------------------------

channel_update_tests :: TestTree
channel_update_tests = testGroup "ChannelUpdate" [
    testCase "encode/decode roundtrip (NodeTwo)" $ do
      let msg = ChannelUpdate
            { chanUpdateSignature      = testSignature
            , chanUpdateChainHash      = testChainHash
            , chanUpdateShortChanId    = testShortChannelId
            , chanUpdateTimestamp      = 1234567890
            , chanUpdateChanFlags      = ChannelFlags
                { cfDirection = NodeTwo, cfStatus = Enabled }
            , chanUpdateCltvExpDelta   = CltvExpiryDelta 144
            , chanUpdateHtlcMinMsat    = HtlcMinimumMsat 1000
            , chanUpdateFeeBaseMsat    = FeeBaseMsat 1000
            , chanUpdateFeeProportional = FeeProportionalMillionths 100
            , chanUpdateHtlcMaxMsat    = HtlcMaximumMsat 1000000000
            }
          encoded = encodeChannelUpdate msg
      case decodeChannelUpdate encoded of
        Right (decoded, _) -> decoded @?= msg
        Left e -> assertFailure $ "decode failed: " ++ show e
  , testCase "encode/decode roundtrip (NodeOne)" $ do
      let msg = ChannelUpdate
            { chanUpdateSignature      = testSignature
            , chanUpdateChainHash      = testChainHash
            , chanUpdateShortChanId    = testShortChannelId
            , chanUpdateTimestamp      = 1234567890
            , chanUpdateChanFlags      = ChannelFlags
                { cfDirection = NodeOne, cfStatus = Enabled }
            , chanUpdateCltvExpDelta   = CltvExpiryDelta 40
            , chanUpdateHtlcMinMsat    = HtlcMinimumMsat 1000
            , chanUpdateFeeBaseMsat    = FeeBaseMsat 500
            , chanUpdateFeeProportional = FeeProportionalMillionths 50
            , chanUpdateHtlcMaxMsat    = HtlcMaximumMsat 1000000000
            }
          encoded = encodeChannelUpdate msg
      case decodeChannelUpdate encoded of
        Right (decoded, _) -> decoded @?= msg
        Left e -> assertFailure $ "decode failed: " ++ show e
  , testCase "encoder sets must_be_one and emits htlc_maximum_msat" $ do
      let encoded = encodeChannelUpdate testChannelUpdate
      BS.length encoded @?= 136
      BS.take 1 (BS.drop 108 encoded) @?= BS.singleton 0x01
      BS.drop 128 encoded @?= BS.pack [0, 0, 0, 0, 0x3b, 0x9a, 0xca, 0]
  , testCase "decodes htlc_maximum_msat when must_be_one is clear" $ do
      -- receivers ignore must_be_one; the field is always present
      let wire = setByte 108 0x00 (encodeChannelUpdate testChannelUpdate)
      case decodeChannelUpdate wire of
        Right (decoded, rest) -> do
          decoded @?= testChannelUpdate
          rest @?= BS.empty
        Left e -> assertFailure $ "decode failed: " ++ show e
  , testCase "rejects update without htlc_maximum_msat" $ do
      let wire = BS.take 128 (encodeChannelUpdate testChannelUpdate)
      decodeChannelUpdate wire @?= Left DecodeInsufficientBytes
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
              , queryRangeFirstBlock = BlockHeight 600000
              , queryRangeNumBlocks  = BlockCount 10000
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
              , replyRangeFirstBlock   = BlockHeight 600000
              , replyRangeNumBlocks    = BlockCount 10000
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
  , tlvStreamTests "query_short_channel_ids TLVs" [1] $ \tlvs ->
      fmap (unTlvStream . queryScidsTlvs . fst)
        (decodeQueryShortChannelIds (mconcat
          [ unChainHash testChainHash
          , u16be 9
          , encodeShortChannelIdList [testShortChannelId]
          , tlvs
          ]))
  , tlvStreamTests "query_channel_range TLVs" [1] $ \tlvs ->
      fmap (unTlvStream . queryRangeTlvs . fst)
        (decodeQueryChannelRange (mconcat
          [ unChainHash testChainHash
          , BS.pack [0, 9, 0x27, 0xc0]  -- first_blocknum 600000
          , BS.pack [0, 0, 0x27, 0x10]  -- number_of_blocks 10000
          , tlvs
          ]))
  , tlvStreamTests "reply_channel_range TLVs" [1, 3] $ \tlvs ->
      fmap (unTlvStream . replyRangeTlvs . fst)
        (decodeReplyChannelRange (mconcat
          [ unChainHash testChainHash
          , BS.pack [0, 9, 0x27, 0xc0]  -- first_blocknum 600000
          , BS.pack [0, 0, 0x27, 0x10]  -- number_of_blocks 10000
          , BS.singleton 1              -- sync_complete
          , u16be 9
          , encodeShortChannelIdList [testShortChannelId]
          , tlvs
          ]))
  ]

-- | TLV stream tests for a query decoder, given its known TLV types
-- and a function decoding a message that ends in the given TLV bytes.
tlvStreamTests
  :: String
  -> [Word8]
  -> (BS.ByteString -> Either DecodeError [TlvRecord])
  -> TestTree
tlvStreamTests name known dec = testGroup name [
    testCase "empty stream" $
      dec BS.empty @?= Right []
  , testCase "keeps known types" $
      dec (BS.pack (concatMap (\t -> [t, 1, 0x2a]) known))
        @?= Right [TlvRecord (fromIntegral t) "\x2a" | t <- known]
  , testCase "skips unknown odd types" $
      dec (BS.pack [5, 0]) @?= Right []
  , testCase "rejects unknown even types" $
      dec (BS.pack [2, 0]) @?= Left DecodeTlvError
  , testCase "rejects a length past the end" $
      dec (BS.pack [1, 5, 0xaa]) @?= Left DecodeTlvError
  , testCase "rejects repeated types" $
      dec (BS.pack [1, 0, 1, 0]) @?= Left DecodeTlvError
  , testCase "rejects a non-minimal BigSize" $
      dec (BS.pack [0xfd, 0x00, 0x01, 0x00]) @?= Left DecodeTlvError
  ]

-- SCID List Tests ------------------------------------------------------------

scid_list_tests :: TestTree
scid_list_tests = testGroup "SCID List Encoding" [
    testCase "encode/decode roundtrip empty list" $ do
      let encoded = encodeShortChannelIdList []
      case decodeShortChannelIdList encoded of
        Right decoded -> decoded @?= []
        Left e -> assertFailure $ "decode failed: " ++ show e
  , testCase "encode/decode roundtrip single SCID" $ do
      let scids = [mkScid 539268 845 1]
          encoded = encodeShortChannelIdList scids
      case decodeShortChannelIdList encoded of
        Right decoded -> decoded @?= scids
        Left e -> assertFailure $ "decode failed: " ++ show e
  , testCase "encode/decode roundtrip multiple SCIDs" $ do
      let scids = [ mkScid 100000 1 0
                  , mkScid 200000 2 1
                  , mkScid 300000 3 2
                  ]
          encoded = encodeShortChannelIdList scids
      case decodeShortChannelIdList encoded of
        Right decoded -> decoded @?= scids
        Left e -> assertFailure $ "decode failed: " ++ show e
  , testCase "encoding has correct format" $ do
      let scids = [mkScid 1 2 3]
          encoded = encodeShortChannelIdList scids
      -- First byte should be 0 (encoding type)
      BS.index encoded 0 @?= 0
      -- Total length: 1 (type) + 8 (SCID) = 9
      BS.length encoded @?= 9
  , testCase "decode rejects unknown encoding type" $ do
      -- Encoding type 1 (zlib compressed) is not supported
      let badEncoded = BS.cons 1 (scidToBytes testShortChannelId)
      case decodeShortChannelIdList badEncoded of
        Left _ -> pure ()
        Right _ -> assertFailure "should reject encoding type 1"
  ]

-- Hash Tests -----------------------------------------------------------------

hash_tests :: TestTree
hash_tests = testGroup "Hash Functions" [
    testGroup "CRC32C" [
      testCase "known test vector '123456789'" $ do
        -- Standard CRC-32C test vector
        crc32c "123456789" @?= 0xe3069283
    , testCase "empty string" $ do
        crc32c "" @?= 0x00000000
    ]
  , testGroup "Signature Hashes" [
      testCase "channelAnnouncementHash produces 32 bytes" $ do
        -- Create a minimal valid encoded message
        case encodeChannelAnnouncement ChannelAnnouncement
              { channelAnnNodeSig1    = testSignature
              , channelAnnNodeSig2    = testSignature
              , channelAnnBitcoinSig1 = testSignature
              , channelAnnBitcoinSig2 = testSignature
              , channelAnnFeatures    = emptyFeatures
              , channelAnnChainHash   = testChainHash
              , channelAnnShortChanId = testShortChannelId
              , channelAnnNodeId1     = testNodeId
              , channelAnnNodeId2     = testNodeId2
              , channelAnnBitcoinKey1 = testPoint
              , channelAnnBitcoinKey2 = testPoint
              } of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right msg -> do
            let hashVal = channelAnnouncementHash msg
            fmap BS.length hashVal @?= Just 32
    , testCase "nodeAnnouncementHash produces 32 bytes" $ do
        case encodeNodeAnnouncement NodeAnnouncement
              { nodeAnnSignature = testSignature
              , nodeAnnFeatures  = emptyFeatures
              , nodeAnnTimestamp = 1234567890
              , nodeAnnNodeId    = testNodeId
              , nodeAnnRgbColor  = testRgbColor
              , nodeAnnAlias     = testAlias
              , nodeAnnAddresses = []
              } of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right msg -> do
            let hashVal = nodeAnnouncementHash msg
            fmap BS.length hashVal @?= Just 32
    , testCase "channelUpdateHash produces 32 bytes" $ do
        let msg = encodeChannelUpdate ChannelUpdate
              { chanUpdateSignature       = testSignature
              , chanUpdateChainHash       = testChainHash
              , chanUpdateShortChanId     = testShortChannelId
              , chanUpdateTimestamp       = 1234567890
              , chanUpdateChanFlags       = ChannelFlags
                  { cfDirection = NodeOne
                  , cfStatus = Enabled }
              , chanUpdateCltvExpDelta    = CltvExpiryDelta 144
              , chanUpdateHtlcMinMsat     = HtlcMinimumMsat 1000
              , chanUpdateFeeBaseMsat     = FeeBaseMsat 1000
              , chanUpdateFeeProportional = FeeProportionalMillionths 100
              , chanUpdateHtlcMaxMsat     = HtlcMaximumMsat 1000000000
              }
            hashVal = channelUpdateHash msg
        fmap BS.length hashVal @?= Just 32
    ]
  , testGroup "Checksum" [
      testCase "channelUpdateChecksum produces consistent result" $ do
        -- The checksum should be deterministic
        let msg = encodeChannelUpdate ChannelUpdate
              { chanUpdateSignature       = testSignature
              , chanUpdateChainHash       = testChainHash
              , chanUpdateShortChanId     = testShortChannelId
              , chanUpdateTimestamp       = 1234567890
              , chanUpdateChanFlags       = ChannelFlags
                  { cfDirection = NodeOne
                  , cfStatus = Enabled }
              , chanUpdateCltvExpDelta    = CltvExpiryDelta 144
              , chanUpdateHtlcMinMsat     = HtlcMinimumMsat 1000
              , chanUpdateFeeBaseMsat     = FeeBaseMsat 1000
              , chanUpdateFeeProportional = FeeProportionalMillionths 100
              , chanUpdateHtlcMaxMsat     = HtlcMaximumMsat 1000000000
              }
            cs1 = channelUpdateChecksum msg
            cs2 = channelUpdateChecksum msg
        cs1 @?= cs2
    , testCase "different timestamps produce same checksum" $ do
        -- Checksum excludes timestamp field
        let msg1 = encodeChannelUpdate ChannelUpdate
              { chanUpdateSignature       = testSignature
              , chanUpdateChainHash       = testChainHash
              , chanUpdateShortChanId     = testShortChannelId
              , chanUpdateTimestamp       = 1000000000
              , chanUpdateChanFlags       = ChannelFlags
                  { cfDirection = NodeOne
                  , cfStatus = Enabled }
              , chanUpdateCltvExpDelta    = CltvExpiryDelta 144
              , chanUpdateHtlcMinMsat     = HtlcMinimumMsat 1000
              , chanUpdateFeeBaseMsat     = FeeBaseMsat 1000
              , chanUpdateFeeProportional = FeeProportionalMillionths 100
              , chanUpdateHtlcMaxMsat     = HtlcMaximumMsat 1000000000
              }
            msg2 = encodeChannelUpdate ChannelUpdate
              { chanUpdateSignature       = testSignature
              , chanUpdateChainHash       = testChainHash
              , chanUpdateShortChanId     = testShortChannelId
              , chanUpdateTimestamp       = 2000000000
              , chanUpdateChanFlags       = ChannelFlags
                  { cfDirection = NodeOne
                  , cfStatus = Enabled }
              , chanUpdateCltvExpDelta    = CltvExpiryDelta 144
              , chanUpdateHtlcMinMsat     = HtlcMinimumMsat 1000
              , chanUpdateFeeBaseMsat     = FeeBaseMsat 1000
              , chanUpdateFeeProportional = FeeProportionalMillionths 100
              , chanUpdateHtlcMaxMsat     = HtlcMaximumMsat 1000000000
              }
        channelUpdateChecksum msg1 @?= channelUpdateChecksum msg2
    ]
  , testGroup "Input length" [
      testCase "channel_update needs 136 bytes" $ do
        let wire = encodeChannelUpdate testChannelUpdate
        channelUpdateHash (BS.take 135 wire) @?= Nothing
        channelUpdateChecksum (BS.take 135 wire) @?= Nothing
        channelUpdateChecksum "short" @?= Nothing
        fmap BS.length (channelUpdateHash wire) @?= Just 32
        assertBool "checksum" (channelUpdateChecksum wire /= Nothing)
    , testCase "channel_announcement needs its declared features" $
        case encodeChannelAnnouncement
               (testChannelAnn (featureBits (BS.pack [1, 2]))) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right wire -> do
            fmap BS.length (channelAnnouncementHash wire) @?= Just 32
            channelAnnouncementHash (BS.take (BS.length wire - 1) wire)
              @?= Nothing
            -- claim one more feature byte than present
            channelAnnouncementHash (setByte 257 3 wire) @?= Nothing
            channelAnnouncementHash (BS.take 257 wire) @?= Nothing
    , testCase "node_announcement needs its declared addresses" $ do
        v4 <- addrV4
        case encodeNodeAnnouncement (testNodeAnn [v4]) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right wire -> do
            fmap BS.length (nodeAnnouncementHash wire) @?= Just 32
            nodeAnnouncementHash (BS.take (BS.length wire - 1) wire)
              @?= Nothing
            -- claim one more address byte than present (addrlen 8)
            nodeAnnouncementHash (setByte 139 8 wire) @?= Nothing
            nodeAnnouncementHash (BS.take 139 wire) @?= Nothing
    ]
  ]

-- Validation Tests -----------------------------------------------------------

validation_tests :: TestTree
validation_tests = testGroup "Validation" [
    testGroup "ChannelAnnouncement" [
      testCase "valid announcement passes" $ do
        let msg = ChannelAnnouncement
              { channelAnnNodeSig1    = testSignature
              , channelAnnNodeSig2    = testSignature
              , channelAnnBitcoinSig1 = testSignature
              , channelAnnBitcoinSig2 = testSignature
              , channelAnnFeatures    = emptyFeatures
              , channelAnnChainHash   = testChainHash
              , channelAnnShortChanId = testShortChannelId
              , channelAnnNodeId1     = testNodeId2  -- 0x02... < 0x03...
              , channelAnnNodeId2     = testNodeId   -- 0x03...
              , channelAnnBitcoinKey1 = testPoint
              , channelAnnBitcoinKey2 = testPoint
              }
        validateChannelAnnouncement msg @?= Right ()
    , testCase "rejects wrong node_id order" $ do
        let msg = ChannelAnnouncement
              { channelAnnNodeSig1    = testSignature
              , channelAnnNodeSig2    = testSignature
              , channelAnnBitcoinSig1 = testSignature
              , channelAnnBitcoinSig2 = testSignature
              , channelAnnFeatures    = emptyFeatures
              , channelAnnChainHash   = testChainHash
              , channelAnnShortChanId = testShortChannelId
              , channelAnnNodeId1     = testNodeId   -- 0x03... > 0x02...
              , channelAnnNodeId2     = testNodeId2  -- 0x02...
              , channelAnnBitcoinKey1 = testPoint
              , channelAnnBitcoinKey2 = testPoint
              }
        validateChannelAnnouncement msg @?= Left ValidateNodeIdOrdering
    ]
  , testGroup "NodeAnnouncement" [
      testCase "one DNS hostname passes" $ do
        h <- expectJust "hostname" (hostname "ln.example.com")
        v4 <- addrV4
        validateNodeAnnouncement (testNodeAnn [v4, AddrDNS h 9735])
          @?= Right ()
    , testCase "rejects more than one DNS hostname" $ do
        h1 <- expectJust "hostname" (hostname "a.example.com")
        h2 <- expectJust "hostname" (hostname "b.example.com")
        validateNodeAnnouncement
          (testNodeAnn [AddrDNS h1 9735, AddrDNS h2 9735])
          @?= Left ValidateMultipleDns
    ]
  , testGroup "ChannelUpdate" [
      testCase "valid update passes" $ do
        let msg = ChannelUpdate
              { chanUpdateSignature       = testSignature
              , chanUpdateChainHash       = testChainHash
              , chanUpdateShortChanId     = testShortChannelId
              , chanUpdateTimestamp       = 1234567890
              , chanUpdateChanFlags       = ChannelFlags
                  { cfDirection = NodeOne
                  , cfStatus = Enabled }
              , chanUpdateCltvExpDelta    = CltvExpiryDelta 144
              , chanUpdateHtlcMinMsat     = HtlcMinimumMsat 1000
              , chanUpdateFeeBaseMsat     = FeeBaseMsat 1000
              , chanUpdateFeeProportional = FeeProportionalMillionths 100
              , chanUpdateHtlcMaxMsat     = HtlcMaximumMsat 1000000000
              }
        validateChannelUpdate msg @?= Right ()
    , testCase "rejects htlc_min > htlc_max" $ do
        let msg = ChannelUpdate
              { chanUpdateSignature       = testSignature
              , chanUpdateChainHash       = testChainHash
              , chanUpdateShortChanId     = testShortChannelId
              , chanUpdateTimestamp       = 1234567890
              , chanUpdateChanFlags       = ChannelFlags
                  { cfDirection = NodeOne
                  , cfStatus = Enabled }
              , chanUpdateCltvExpDelta    = CltvExpiryDelta 144
              , chanUpdateHtlcMinMsat     = HtlcMinimumMsat 2000000000  -- > htlcMax
              , chanUpdateFeeBaseMsat     = FeeBaseMsat 1000
              , chanUpdateFeeProportional = FeeProportionalMillionths 100
              , chanUpdateHtlcMaxMsat     = HtlcMaximumMsat 1000000000
              }
        validateChannelUpdate msg @?= Left ValidateHtlcAmounts
    ]
  , testGroup "QueryChannelRange" [
      testCase "valid range passes" $ do
        let msg = QueryChannelRange
              { queryRangeChainHash  = testChainHash
              , queryRangeFirstBlock = BlockHeight 600000
              , queryRangeNumBlocks  = BlockCount 10000
              , queryRangeTlvs       = emptyTlvs
              }
        validateQueryChannelRange msg @?= Right ()
    , testCase "rejects overflow" $ do
        let msg = QueryChannelRange
              { queryRangeChainHash  = testChainHash
              , queryRangeFirstBlock = BlockHeight maxBound
              , queryRangeNumBlocks  = BlockCount 10
              , queryRangeTlvs       = emptyTlvs
              }
        validateQueryChannelRange msg @?= Left ValidateBlockOverflow
    ]
  , testGroup "ReplyChannelRange" [
      testCase "ascending short_channel_ids pass" $
        validateReplyChannelRange (testReplyRange
          (encodeShortChannelIdList [mkScid 1 0 0, mkScid 2 0 0]))
          @?= Right ()
    , testCase "rejects descending short_channel_ids" $
        validateReplyChannelRange (testReplyRange
          (encodeShortChannelIdList [mkScid 2 0 0, mkScid 1 0 0]))
          @?= Left ValidateScidNotAscending
    , testCase "rejects zlib encoding (type 1)" $
        validateReplyChannelRange (testReplyRange (BS.cons 1
          (scidToBytes (mkScid 2 0 0) <> scidToBytes (mkScid 1 0 0))))
          @?= Left ValidateScidEncoding
    , testCase "rejects an unknown encoding type" $
        validateReplyChannelRange (testReplyRange (BS.cons 2
          (scidToBytes (mkScid 1 0 0))))
          @?= Left ValidateScidEncoding
    , testCase "rejects a partial short_channel_id" $
        validateReplyChannelRange (testReplyRange (BS.cons 0
          (scidToBytes (mkScid 1 0 0) <> BS.replicate 7 0)))
          @?= Left ValidateScidEncoding
    ]
  ]

-- | A reply_channel_range fixture with the given encoded_short_ids.
testReplyRange :: BS.ByteString -> ReplyChannelRange
testReplyRange scids = ReplyChannelRange
  { replyRangeChainHash    = testChainHash
  , replyRangeFirstBlock   = BlockHeight 0
  , replyRangeNumBlocks    = BlockCount 10
  , replyRangeSyncComplete = 1
  , replyRangeData         = scids
  , replyRangeTlvs         = emptyTlvs
  }

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
  , testGroup "Length overflow" [
      testCase "channel_announcement features" $ do
        let feats n = featureBits (BS.replicate n 0x01)
        encodeChannelAnnouncement (testChannelAnn (feats 65536))
          @?= Left EncodeLengthOverflow
        case encodeChannelAnnouncement (testChannelAnn (feats 65535)) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right wire -> fmap fst (decodeChannelAnnouncement wire)
            @?= Right (testChannelAnn (feats 65535))
    , testCase "node_announcement features" $ do
        let msg = (testNodeAnn [])
              { nodeAnnFeatures = featureBits (BS.replicate 65536 0x01) }
        encodeNodeAnnouncement msg @?= Left EncodeLengthOverflow
    , testCase "node_announcement addresses" $ do
        v4 <- addrV4
        -- 10923 IPv4 descriptors take 65538 bytes
        encodeNodeAnnouncement (testNodeAnn (replicate 10923 v4))
          @?= Left EncodeLengthOverflow
        case encodeNodeAnnouncement (testNodeAnn (replicate 9362 v4)) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right wire -> fmap (length . nodeAnnAddresses . fst)
            (decodeNodeAnnouncement wire) @?= Right 9362
    , testCase "query_short_channel_ids encoded_short_ids" $ do
        let msg = QueryShortChannelIds
              { queryScidsChainHash = testChainHash
              , queryScidsData      = BS.replicate 65536 0x00
              , queryScidsTlvs      = emptyTlvs
              }
        encodeQueryShortChannelIds msg @?= Left EncodeLengthOverflow
    , testCase "reply_channel_range encoded_short_ids" $ do
        let msg = ReplyChannelRange
              { replyRangeChainHash    = testChainHash
              , replyRangeFirstBlock   = BlockHeight 600000
              , replyRangeNumBlocks    = BlockCount 10000
              , replyRangeSyncComplete = 1
              , replyRangeData         = BS.replicate 65536 0x00
              , replyRangeTlvs         = emptyTlvs
              }
        encodeReplyChannelRange msg @?= Left EncodeLengthOverflow
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
  , testProperty "ChannelFlags encode/decode roundtrip"
      propChannelFlagsRoundtrip
  , testProperty "ShortChannelId bytes roundtrip"
      propScidBytesRoundtrip
  , testProperty "Hostname validates length bounds"
      propHostnameValidation
  , testProperty "SCID list encode/decode roundtrip"
      propScidListRoundtrip
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
  case encodeChannelAnnouncement msg of
    Left _ -> False
    Right encoded -> case decodeChannelAnnouncement encoded of
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
        , chanUpdateChanFlags      = ChannelFlags
            { cfDirection = NodeOne
            , cfStatus = Enabled }
        , chanUpdateCltvExpDelta   = CltvExpiryDelta cltvDelta
        , chanUpdateHtlcMinMsat    = HtlcMinimumMsat 1000
        , chanUpdateFeeBaseMsat    = FeeBaseMsat 1000
        , chanUpdateFeeProportional = FeeProportionalMillionths 100
        , chanUpdateHtlcMaxMsat    = HtlcMaximumMsat 1000000000
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

-- Property: ChannelFlags encode/decode roundtrip
propChannelFlagsRoundtrip :: Property
propChannelFlagsRoundtrip =
  forAll genChannelFlags $ \cf ->
    decodeChannelFlags (encodeChannelFlags cf) == cf
  where
    genChannelFlags :: Gen ChannelFlags
    genChannelFlags = ChannelFlags
      <$> elements [NodeOne, NodeTwo]
      <*> elements [Enabled, Disabled]

-- Property: ShortChannelId bytes roundtrip
propScidBytesRoundtrip :: Property
propScidBytesRoundtrip =
  forAll (choose (0, 0xFFFFFF)) $ \bh ->
  forAll (choose (0, 0xFFFFFF)) $ \ti ->
  forAll arbitrary $ \oi ->
    case shortChannelId bh ti oi of
      Nothing -> False
      Just scid ->
        scidFromBytes (scidToBytes scid)
        == Just scid

-- Property: Hostname validates length bounds
propHostnameValidation :: NonNegative Int -> Property
propHostnameValidation (NonNegative n) = property $
  let len = n `mod` 300
      bs = BS.replicate len 0x61
  in  case hostname bs of
        Just _  -> len >= 1 && len <= 255
        Nothing -> len == 0 || len > 255

-- Property: SCID list encode/decode roundtrip
propScidListRoundtrip :: Property
propScidListRoundtrip =
  forAll (listOf genScid) $ \scids ->
    case decodeShortChannelIdList
           (encodeShortChannelIdList scids) of
      Right decoded -> decoded == scids
      Left _ -> False
  where
    genScid :: Gen ShortChannelId
    genScid = do
      bh <- choose (0, 0xFFFFFF)
      ti <- choose (0, 0xFFFFFF)
      oi <- arbitrary
      case shortChannelId bh ti oi of
        Just s  -> pure s
        Nothing -> pure testShortChannelId
