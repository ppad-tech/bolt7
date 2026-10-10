{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT7.Codec
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Encoding and decoding for BOLT #7 gossip messages.

module Lightning.Protocol.BOLT7.Codec (
  -- * Error types
    EncodeError(..)
  , DecodeError(..)

  -- * Channel announcement
  , encodeChannelAnnouncement
  , decodeChannelAnnouncement

  -- * Node announcement
  , encodeNodeAnnouncement
  , decodeNodeAnnouncement

  -- * Channel update
  , encodeChannelUpdate
  , decodeChannelUpdate

  -- * Announcement signatures
  , encodeAnnouncementSignatures
  , decodeAnnouncementSignatures

  -- * Query messages
  , encodeQueryShortChannelIds
  , decodeQueryShortChannelIds
  , encodeReplyShortChannelIdsEnd
  , decodeReplyShortChannelIdsEnd
  , encodeQueryChannelRange
  , decodeQueryChannelRange
  , encodeReplyChannelRange
  , decodeReplyChannelRange
  , encodeGossipTimestampFilter
  , decodeGossipTimestampFilter

  -- * Short channel ID encoding
  , encodeShortChannelIdList
  , decodeShortChannelIdList
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1 (TlvStream)
import qualified Lightning.Protocol.BOLT1.Prim as Prim
import qualified Lightning.Protocol.BOLT1.TLV as TLV
import Lightning.Protocol.BOLT7.Messages
import Lightning.Protocol.BOLT7.Types

-- Error types -----------------------------------------------------------------

-- | Encoding errors.
data EncodeError
  = EncodeLengthOverflow  -- ^ Field too large for u16 length prefix
  deriving (Eq, Show, Generic)

instance NFData EncodeError

-- | Decoding errors.
data DecodeError
  = DecodeInsufficientBytes          -- ^ Not enough bytes
  | DecodeInvalidSignature           -- ^ Invalid signature field
  | DecodeInvalidChainHash           -- ^ Invalid chain hash field
  | DecodeInvalidShortChannelId      -- ^ Invalid short channel ID field
  | DecodeInvalidChannelId           -- ^ Invalid channel ID field
  | DecodeInvalidNodeId              -- ^ Invalid node ID field
  | DecodeInvalidPoint               -- ^ Invalid point field
  | DecodeInvalidRgbColor            -- ^ Invalid RGB color field
  | DecodeInvalidAlias               -- ^ Invalid alias field
  | DecodeInvalidAddress             -- ^ Invalid address encoding
  | DecodeTlvError                   -- ^ TLV decoding error
  deriving (Eq, Show, Generic)

instance NFData DecodeError

-- Primitive helpers -----------------------------------------------------------

-- | Decode u8.
decodeU8 :: ByteString -> Either DecodeError (Word8, ByteString)
decodeU8 bs
  | BS.null bs = Left DecodeInsufficientBytes
  | otherwise = Right (BS.index bs 0, BS.drop 1 bs)
{-# INLINE decodeU8 #-}

-- | Decode u16 (big-endian).
decodeU16 :: ByteString -> Either DecodeError (Word16, ByteString)
decodeU16 bs = case Prim.decodeU16 bs of
  Nothing -> Left DecodeInsufficientBytes
  Just r  -> Right r
{-# INLINE decodeU16 #-}

-- | Decode u32 (big-endian).
decodeU32 :: ByteString -> Either DecodeError (Word32, ByteString)
decodeU32 bs = case Prim.decodeU32 bs of
  Nothing -> Left DecodeInsufficientBytes
  Just r  -> Right r
{-# INLINE decodeU32 #-}

-- | Decode u64 (big-endian).
decodeU64 :: ByteString -> Either DecodeError (Word64, ByteString)
decodeU64 bs = case Prim.decodeU64 bs of
  Nothing -> Left DecodeInsufficientBytes
  Just r  -> Right r
{-# INLINE decodeU64 #-}

-- | Decode fixed-length bytes.
decodeBytes :: Int -> ByteString -> Either DecodeError (ByteString, ByteString)
decodeBytes n bs
  | BS.length bs < n = Left DecodeInsufficientBytes
  | otherwise = Right (BS.splitAt n bs)
{-# INLINE decodeBytes #-}

-- | Decode length-prefixed bytes (u16 prefix).
decodeLenPrefixed :: ByteString
                  -> Either DecodeError (ByteString, ByteString)
decodeLenPrefixed bs = do
  (len, rest) <- decodeU16 bs
  let n = fromIntegral len
  if BS.length rest < n
    then Left DecodeInsufficientBytes
    else Right (BS.splitAt n rest)
{-# INLINE decodeLenPrefixed #-}

-- | Encode with u16 length prefix.
encodeLenPrefixed :: ByteString -> ByteString
encodeLenPrefixed bs = Prim.encodeU16 (fromIntegral $ BS.length bs) <> bs
{-# INLINE encodeLenPrefixed #-}

-- | Decode fixed-length validated type.
decodeFixed :: Int -> DecodeError -> (ByteString -> Maybe a)
            -> ByteString -> Either DecodeError (a, ByteString)
decodeFixed len err mkVal bs = do
  (bytes, rest) <- decodeBytes len bs
  case mkVal bytes of
    Nothing -> Left err
    Just v  -> Right (v, rest)
{-# INLINE decodeFixed #-}

-- | Decode a TLV stream per BOLT #1, given the known types: a
-- malformed stream or an unknown even type is an error, and unknown
-- odd types are skipped.
decodeTlvs :: [Word64] -> ByteString -> Either DecodeError TlvStream
decodeTlvs known bs = case TLV.decodeTlvStreamWith (`elem` known) bs of
  Left _  -> Left DecodeTlvError
  Right t -> Right t
{-# INLINE decodeTlvs #-}

-- Type-specific decoders ------------------------------------------------------

-- | Decode Signature (64 bytes).
decodeSignature :: ByteString -> Either DecodeError (Signature, ByteString)
decodeSignature = decodeFixed signatureLen DecodeInvalidSignature signature
{-# INLINE decodeSignature #-}

-- | Decode ChainHash (32 bytes).
decodeChainHash :: ByteString -> Either DecodeError (ChainHash, ByteString)
decodeChainHash = decodeFixed chainHashLen DecodeInvalidChainHash chainHash
{-# INLINE decodeChainHash #-}

-- | Decode ShortChannelId (8 bytes).
decodeShortChannelId :: ByteString
                     -> Either DecodeError (ShortChannelId, ByteString)
decodeShortChannelId =
  decodeFixed shortChannelIdLen DecodeInvalidShortChannelId scidFromBytes
{-# INLINE decodeShortChannelId #-}

-- | Decode ChannelId (32 bytes).
decodeChannelId :: ByteString -> Either DecodeError (ChannelId, ByteString)
decodeChannelId = decodeFixed channelIdLen DecodeInvalidChannelId channelId
{-# INLINE decodeChannelId #-}

-- | Decode NodeId (33 bytes).
decodeNodeId :: ByteString -> Either DecodeError (NodeId, ByteString)
decodeNodeId = decodeFixed nodeIdLen DecodeInvalidNodeId nodeId
{-# INLINE decodeNodeId #-}

-- | Decode Point (33 bytes).
decodePoint :: ByteString -> Either DecodeError (Point, ByteString)
decodePoint = decodeFixed pointLen DecodeInvalidPoint point
{-# INLINE decodePoint #-}

-- | Decode RgbColor (3 bytes).
decodeRgbColor :: ByteString -> Either DecodeError (RgbColor, ByteString)
decodeRgbColor = decodeFixed rgbColorLen DecodeInvalidRgbColor rgbColor
{-# INLINE decodeRgbColor #-}

-- | Decode Alias (32 bytes).
decodeAlias :: ByteString -> Either DecodeError (Alias, ByteString)
decodeAlias = decodeFixed aliasLen DecodeInvalidAlias alias
{-# INLINE decodeAlias #-}

-- | Decode FeatureBits (length-prefixed).
decodeFeatureBits :: ByteString -> Either DecodeError (FeatureBits, ByteString)
decodeFeatureBits bs = do
  (bytes, rest) <- decodeLenPrefixed bs
  Right (featureBits bytes, rest)
{-# INLINE decodeFeatureBits #-}

-- | Decode addresses list (length-prefixed).
--
-- Follows the BOLT #7 receiver rules for address descriptors:
--
-- * parsing stops at the first descriptor of an unknown type, and
--   the remaining address bytes are ignored;
-- * deprecated Tor v2 descriptors (type 3) are skipped;
-- * descriptors with port 0 are skipped;
-- * DNS descriptors whose hostname is empty or not ASCII are
--   skipped.
--
-- A descriptor of a known type that @addrlen@ cannot hold is a
-- decoding error.
decodeAddresses :: ByteString -> Either DecodeError ([Address], ByteString)
decodeAddresses bs = do
  (addrData, rest) <- decodeLenPrefixed bs
  addrs <- parseAddrs addrData
  Right (addrs, rest)
  where
    parseAddrs :: ByteString -> Either DecodeError [Address]
    parseAddrs !d
      | BS.null d = Right []
      | otherwise = do
          (typ, d1) <- decodeU8 d
          case typ of
            1 -> fixedAddr ipv4AddrLen (fmap AddrIPv4 . ipv4Addr) d1
            2 -> fixedAddr ipv6AddrLen (fmap AddrIPv6 . ipv6Addr) d1
            3 -> do  -- Tor v2 (deprecated; 10-byte onion + port)
              (_, d2) <- decodeBytes 12 d1
              parseAddrs d2
            4 -> fixedAddr torV3AddrLen (fmap AddrTorV3 . torV3Addr) d1
            5 -> do  -- DNS hostname
              (hostLen, d2) <- decodeU8 d1
              (hostBytes, d3) <- decodeBytes (fromIntegral hostLen) d2
              (port, d4) <- decodeU16 d3
              keep (fmap AddrDNS (hostname hostBytes)) port d4
            _ -> Right []  -- unknown type: ignore the rest

    fixedAddr :: Int -> (ByteString -> Maybe (Word16 -> Address))
              -> ByteString -> Either DecodeError [Address]
    fixedAddr len mk d = do
      (addrBytes, d1) <- decodeBytes len d
      (port, d2) <- decodeU16 d1
      keep (mk addrBytes) port d2

    keep :: Maybe (Word16 -> Address) -> Word16 -> ByteString
         -> Either DecodeError [Address]
    keep mk port d = case mk of
      Just f | port /= 0 -> fmap (f port :) (parseAddrs d)
      _ -> parseAddrs d

-- Channel announcement --------------------------------------------------------

-- | Encode channel_announcement message.
encodeChannelAnnouncement :: ChannelAnnouncement -> ByteString
encodeChannelAnnouncement msg = mconcat
  [ unSignature (channelAnnNodeSig1 msg)
  , unSignature (channelAnnNodeSig2 msg)
  , unSignature (channelAnnBitcoinSig1 msg)
  , unSignature (channelAnnBitcoinSig2 msg)
  , encodeLenPrefixed (getFeatureBits (channelAnnFeatures msg))
  , unChainHash (channelAnnChainHash msg)
  , scidToBytes (channelAnnShortChanId msg)
  , getNodeId (channelAnnNodeId1 msg)
  , getNodeId (channelAnnNodeId2 msg)
  , unPoint (channelAnnBitcoinKey1 msg)
  , unPoint (channelAnnBitcoinKey2 msg)
  ]

-- | Decode channel_announcement message.
decodeChannelAnnouncement :: ByteString
                          -> Either DecodeError (ChannelAnnouncement, ByteString)
decodeChannelAnnouncement bs = do
  (nodeSig1, bs1)    <- decodeSignature bs
  (nodeSig2, bs2)    <- decodeSignature bs1
  (btcSig1, bs3)     <- decodeSignature bs2
  (btcSig2, bs4)     <- decodeSignature bs3
  (features, bs5)    <- decodeFeatureBits bs4
  (chainH, bs6)      <- decodeChainHash bs5
  (scid, bs7)        <- decodeShortChannelId bs6
  (nid1, bs8)        <- decodeNodeId bs7
  (nid2, bs9)        <- decodeNodeId bs8
  (btcKey1, bs10)    <- decodePoint bs9
  (btcKey2, rest)    <- decodePoint bs10
  let msg = ChannelAnnouncement
        { channelAnnNodeSig1    = nodeSig1
        , channelAnnNodeSig2    = nodeSig2
        , channelAnnBitcoinSig1 = btcSig1
        , channelAnnBitcoinSig2 = btcSig2
        , channelAnnFeatures    = features
        , channelAnnChainHash   = chainH
        , channelAnnShortChanId = scid
        , channelAnnNodeId1     = nid1
        , channelAnnNodeId2     = nid2
        , channelAnnBitcoinKey1 = btcKey1
        , channelAnnBitcoinKey2 = btcKey2
        }
  Right (msg, rest)

-- Node announcement -----------------------------------------------------------

-- | Encode node_announcement message.
encodeNodeAnnouncement :: NodeAnnouncement -> Either EncodeError ByteString
encodeNodeAnnouncement msg = do
  addrData <- encodeAddresses (nodeAnnAddresses msg)
  let features = getFeatureBits (nodeAnnFeatures msg)
  if BS.length features > 65535
    then Left EncodeLengthOverflow
    else Right $ mconcat
      [ unSignature (nodeAnnSignature msg)
      , encodeLenPrefixed features
      , Prim.encodeU32 (nodeAnnTimestamp msg)
      , getNodeId (nodeAnnNodeId msg)
      , getRgbColor (nodeAnnRgbColor msg)
      , getAlias (nodeAnnAlias msg)
      , encodeLenPrefixed addrData
      ]

-- | Encode address list.
encodeAddresses :: [Address] -> Either EncodeError ByteString
encodeAddresses addrs = Right $ mconcat (map encodeAddress addrs)
  where
    encodeAddress :: Address -> ByteString
    encodeAddress (AddrIPv4 a port) = mconcat
      [ BS.singleton 1
      , getIPv4Addr a
      , Prim.encodeU16 port
      ]
    encodeAddress (AddrIPv6 a port) = mconcat
      [ BS.singleton 2
      , getIPv6Addr a
      , Prim.encodeU16 port
      ]
    encodeAddress (AddrTorV3 a port) = mconcat
      [ BS.singleton 4
      , getTorV3Addr a
      , Prim.encodeU16 port
      ]
    encodeAddress (AddrDNS h port) =
      let !host = getHostname h
      in  mconcat
        [ BS.singleton 5
        , BS.singleton (fromIntegral $ BS.length host)
        , host
        , Prim.encodeU16 port
        ]

-- | Decode node_announcement message.
decodeNodeAnnouncement :: ByteString
                       -> Either DecodeError (NodeAnnouncement, ByteString)
decodeNodeAnnouncement bs = do
  (sig, bs1)       <- decodeSignature bs
  (features, bs2)  <- decodeFeatureBits bs1
  (timestamp, bs3) <- decodeU32 bs2
  (nid, bs4)       <- decodeNodeId bs3
  (color, bs5)     <- decodeRgbColor bs4
  (al, bs6)        <- decodeAlias bs5
  (addrs, rest)    <- decodeAddresses bs6
  let msg = NodeAnnouncement
        { nodeAnnSignature = sig
        , nodeAnnFeatures  = features
        , nodeAnnTimestamp = timestamp
        , nodeAnnNodeId    = nid
        , nodeAnnRgbColor  = color
        , nodeAnnAlias     = al
        , nodeAnnAddresses = addrs
        }
  Right (msg, rest)

-- Channel update --------------------------------------------------------------

-- | Encode channel_update message.
--
-- The message_flags byte is always 0x01: bit 0 (@must_be_one@) is
-- set, as BOLT #7 requires, and no other bit is.
encodeChannelUpdate :: ChannelUpdate -> ByteString
encodeChannelUpdate msg = mconcat
  [ unSignature (chanUpdateSignature msg)
  , unChainHash (chanUpdateChainHash msg)
  , scidToBytes (chanUpdateShortChanId msg)
  , Prim.encodeU32 (chanUpdateTimestamp msg)
  , BS.singleton 0x01
  , BS.singleton (encodeChannelFlags (chanUpdateChanFlags msg))
  , Prim.encodeU16
      (getCltvExpiryDelta (chanUpdateCltvExpDelta msg))
  , Prim.encodeU64
      (getHtlcMinimumMsat (chanUpdateHtlcMinMsat msg))
  , Prim.encodeU32
      (getFeeBaseMsat (chanUpdateFeeBaseMsat msg))
  , Prim.encodeU32
      (getFeeProportionalMillionths
        (chanUpdateFeeProportional msg))
  , Prim.encodeU64
      (getHtlcMaximumMsat (chanUpdateHtlcMaxMsat msg))
  ]

-- | Decode channel_update message.
--
-- The message_flags byte is read from the wire but not stored.
-- Per BOLT #7, 'htlc_maximum_msat' is always present, and
-- receivers ignore the @must_be_one@ bit.
decodeChannelUpdate :: ByteString
                    -> Either DecodeError
                         (ChannelUpdate, ByteString)
decodeChannelUpdate bs = do
  (sig, bs1)          <- decodeSignature bs
  (chainH, bs2)       <- decodeChainHash bs1
  (scid, bs3)         <- decodeShortChannelId bs2
  (timestamp, bs4)    <- decodeU32 bs3
  (_msgFlags, bs5)    <- decodeU8 bs4
  (chanFlagsRaw, bs6) <- decodeU8 bs5
  (cltvDelta, bs7)    <- decodeU16 bs6
  (htlcMin, bs8)      <- decodeU64 bs7
  (feeBase, bs9)      <- decodeU32 bs8
  (feeProp, bs10)     <- decodeU32 bs9
  (htlcMax, rest)     <- decodeU64 bs10
  let msg = ChannelUpdate
        { chanUpdateSignature       = sig
        , chanUpdateChainHash       = chainH
        , chanUpdateShortChanId     = scid
        , chanUpdateTimestamp       = timestamp
        , chanUpdateChanFlags       = decodeChannelFlags chanFlagsRaw
        , chanUpdateCltvExpDelta    = CltvExpiryDelta cltvDelta
        , chanUpdateHtlcMinMsat     = HtlcMinimumMsat htlcMin
        , chanUpdateFeeBaseMsat     = FeeBaseMsat feeBase
        , chanUpdateFeeProportional =
            FeeProportionalMillionths feeProp
        , chanUpdateHtlcMaxMsat     = HtlcMaximumMsat htlcMax
        }
  Right (msg, rest)

-- Announcement signatures -----------------------------------------------------

-- | Encode announcement_signatures message.
encodeAnnouncementSignatures :: AnnouncementSignatures -> ByteString
encodeAnnouncementSignatures msg = mconcat
  [ unChannelId (annSigChannelId msg)
  , scidToBytes (annSigShortChanId msg)
  , unSignature (annSigNodeSig msg)
  , unSignature (annSigBitcoinSig msg)
  ]

-- | Decode announcement_signatures message.
decodeAnnouncementSignatures :: ByteString
                             -> Either DecodeError
                                  (AnnouncementSignatures, ByteString)
decodeAnnouncementSignatures bs = do
  (cid, bs1)     <- decodeChannelId bs
  (scid, bs2)    <- decodeShortChannelId bs1
  (nodeSig, bs3) <- decodeSignature bs2
  (btcSig, rest) <- decodeSignature bs3
  let msg = AnnouncementSignatures
        { annSigChannelId   = cid
        , annSigShortChanId = scid
        , annSigNodeSig     = nodeSig
        , annSigBitcoinSig  = btcSig
        }
  Right (msg, rest)

-- Query messages --------------------------------------------------------------

-- | Encode query_short_channel_ids message.
encodeQueryShortChannelIds :: QueryShortChannelIds
                           -> Either EncodeError ByteString
encodeQueryShortChannelIds msg = do
  let scidData = queryScidsData msg
  if BS.length scidData > 65535
    then Left EncodeLengthOverflow
    else Right $ mconcat
      [ unChainHash (queryScidsChainHash msg)
      , encodeLenPrefixed scidData
      , TLV.encodeTlvStream (queryScidsTlvs msg)
      ]

-- | Decode query_short_channel_ids message.
--
-- The only known TLV type is 1 (query_flags). A malformed TLV
-- stream or an unknown even type is 'DecodeTlvError'.
decodeQueryShortChannelIds :: ByteString
                           -> Either DecodeError
                                (QueryShortChannelIds, ByteString)
decodeQueryShortChannelIds bs = do
  (chainH, bs1)   <- decodeChainHash bs
  (scidData, bs2) <- decodeLenPrefixed bs1
  tlvs            <- decodeTlvs [1] bs2
  let msg = QueryShortChannelIds
        { queryScidsChainHash = chainH
        , queryScidsData      = scidData
        , queryScidsTlvs      = tlvs
        }
  Right (msg, BS.empty)

-- | Encode reply_short_channel_ids_end message.
encodeReplyShortChannelIdsEnd :: ReplyShortChannelIdsEnd -> ByteString
encodeReplyShortChannelIdsEnd msg = mconcat
  [ unChainHash (replyScidsChainHash msg)
  , BS.singleton (replyScidsFullInfo msg)
  ]

-- | Decode reply_short_channel_ids_end message.
decodeReplyShortChannelIdsEnd :: ByteString
                              -> Either DecodeError
                                   (ReplyShortChannelIdsEnd, ByteString)
decodeReplyShortChannelIdsEnd bs = do
  (chainH, bs1)    <- decodeChainHash bs
  (fullInfo, rest) <- decodeU8 bs1
  let msg = ReplyShortChannelIdsEnd
        { replyScidsChainHash = chainH
        , replyScidsFullInfo  = fullInfo
        }
  Right (msg, rest)

-- | Encode query_channel_range message.
encodeQueryChannelRange :: QueryChannelRange -> ByteString
encodeQueryChannelRange msg = mconcat
  [ unChainHash (queryRangeChainHash msg)
  , Prim.encodeU32
      (getBlockHeight (queryRangeFirstBlock msg))
  , Prim.encodeU32
      (getBlockCount (queryRangeNumBlocks msg))
  , TLV.encodeTlvStream (queryRangeTlvs msg)
  ]

-- | Decode query_channel_range message.
--
-- The only known TLV type is 1 (query_option). A malformed TLV
-- stream or an unknown even type is 'DecodeTlvError'.
decodeQueryChannelRange :: ByteString
                        -> Either DecodeError
                             (QueryChannelRange, ByteString)
decodeQueryChannelRange bs = do
  (chainH, bs1)     <- decodeChainHash bs
  (firstBlock, bs2) <- decodeU32 bs1
  (numBlocks, bs3)  <- decodeU32 bs2
  tlvs              <- decodeTlvs [1] bs3
  let msg = QueryChannelRange
        { queryRangeChainHash  = chainH
        , queryRangeFirstBlock = BlockHeight firstBlock
        , queryRangeNumBlocks  = BlockCount numBlocks
        , queryRangeTlvs       = tlvs
        }
  Right (msg, BS.empty)

-- | Encode reply_channel_range message.
encodeReplyChannelRange :: ReplyChannelRange
                        -> Either EncodeError ByteString
encodeReplyChannelRange msg = do
  let rangeData = replyRangeData msg
  if BS.length rangeData > 65535
    then Left EncodeLengthOverflow
    else Right $ mconcat
      [ unChainHash (replyRangeChainHash msg)
      , Prim.encodeU32
          (getBlockHeight (replyRangeFirstBlock msg))
      , Prim.encodeU32
          (getBlockCount (replyRangeNumBlocks msg))
      , BS.singleton (replyRangeSyncComplete msg)
      , encodeLenPrefixed rangeData
      , TLV.encodeTlvStream (replyRangeTlvs msg)
      ]

-- | Decode reply_channel_range message.
--
-- The known TLV types are 1 (timestamps_tlv) and 3
-- (checksums_tlv). A malformed TLV stream or an unknown even type
-- is 'DecodeTlvError'.
decodeReplyChannelRange :: ByteString
                        -> Either DecodeError
                             (ReplyChannelRange, ByteString)
decodeReplyChannelRange bs = do
  (chainH, bs1)       <- decodeChainHash bs
  (firstBlock, bs2)   <- decodeU32 bs1
  (numBlocks, bs3)    <- decodeU32 bs2
  (syncComplete, bs4) <- decodeU8 bs3
  (rangeData, bs5)    <- decodeLenPrefixed bs4
  tlvs                <- decodeTlvs [1, 3] bs5
  let msg = ReplyChannelRange
        { replyRangeChainHash    = chainH
        , replyRangeFirstBlock   = BlockHeight firstBlock
        , replyRangeNumBlocks    = BlockCount numBlocks
        , replyRangeSyncComplete = syncComplete
        , replyRangeData         = rangeData
        , replyRangeTlvs         = tlvs
        }
  Right (msg, BS.empty)

-- | Encode gossip_timestamp_filter message.
encodeGossipTimestampFilter :: GossipTimestampFilter -> ByteString
encodeGossipTimestampFilter msg = mconcat
  [ unChainHash (gossipFilterChainHash msg)
  , Prim.encodeU32 (gossipFilterFirstTimestamp msg)
  , Prim.encodeU32 (gossipFilterTimestampRange msg)
  ]

-- | Decode gossip_timestamp_filter message.
decodeGossipTimestampFilter :: ByteString
                            -> Either DecodeError
                                 (GossipTimestampFilter, ByteString)
decodeGossipTimestampFilter bs = do
  (chainH, bs1)   <- decodeChainHash bs
  (firstTs, bs2)  <- decodeU32 bs1
  (tsRange, rest) <- decodeU32 bs2
  let msg = GossipTimestampFilter
        { gossipFilterChainHash      = chainH
        , gossipFilterFirstTimestamp = firstTs
        , gossipFilterTimestampRange = tsRange
        }
  Right (msg, rest)

-- Short channel ID list encoding -----------------------------------------------

-- | Encode a list of short channel IDs as concatenated 8-byte values.
--
-- This produces encoded_short_ids data with encoding type 0 (uncompressed).
-- The first byte is the encoding type (0), followed by the concatenated SCIDs.
--
-- Note: This does NOT sort the SCIDs. The caller should ensure they are in
-- ascending order if that's required by the protocol context.
encodeShortChannelIdList :: [ShortChannelId] -> ByteString
encodeShortChannelIdList scids = BS.cons 0 $
  mconcat (map scidToBytes scids)
{-# INLINE encodeShortChannelIdList #-}

-- | Decode a list of short channel IDs from encoded_short_ids data.
--
-- Supports encoding type 0 (uncompressed). Other encoding types will fail.
decodeShortChannelIdList :: ByteString
                         -> Either DecodeError [ShortChannelId]
decodeShortChannelIdList bs
  | BS.null bs = Left DecodeInsufficientBytes
  | otherwise = do
      let encType = BS.index bs 0
          payload = BS.drop 1 bs
      case encType of
        0 -> decodeUncompressedScids payload
        _ -> Left DecodeInvalidShortChannelId  -- Unsupported encoding type
  where
    decodeUncompressedScids :: ByteString -> Either DecodeError [ShortChannelId]
    decodeUncompressedScids !d
      | BS.null d = Right []
      | BS.length d < shortChannelIdLen = Left DecodeInsufficientBytes
      | otherwise = do
          let (scidBytes, rest) = BS.splitAt shortChannelIdLen d
          case scidFromBytes scidBytes of
            Nothing -> Left DecodeInvalidShortChannelId
            Just scid -> do
              scids <- decodeUncompressedScids rest
              Right (scid : scids)
{-# INLINE decodeShortChannelIdList #-}
