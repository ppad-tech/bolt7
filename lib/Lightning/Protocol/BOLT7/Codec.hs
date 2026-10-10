{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT7.Codec
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Encoding and decoding of BOLT #7 messages.

module Lightning.Protocol.BOLT7.Codec (
    EncodeError(..)
  , DecodeError(..)
  , encode_message
  , decode_message
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
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1
  ( ChainHash, MilliSatoshi, Point, ShortChannelId, Signature
  , TlvError, TlvRecord(..), TlvStream )
import qualified Lightning.Protocol.BOLT1 as BOLT1
import qualified Lightning.Protocol.BOLT9 as BOLT9
import Lightning.Protocol.BOLT7.Messages
import Lightning.Protocol.BOLT7.Types

-- errors ---------------------------------------------------------------------

-- | Why a message failed to encode.
data EncodeError
  = EncodeLengthOverflow
    -- ^ a length-prefixed field exceeds 65535 bytes
  | EncodeInvalidTlvs
    -- ^ a @_tlvs@ field holds a record of a type the message defines
  | EncodeCountMismatch
    -- ^ query flags, timestamps or checksums don't number one per
    --   short channel id
  | EncodeInvalidAddresses
    -- ^ 'na_unknown_addresses' begins with a descriptor of a known type
  deriving (Eq, Show, Generic)

instance NFData EncodeError

-- | Why a message failed to decode.
data DecodeError
  = DecodeInsufficientBytes
    -- ^ the input ended before a field did
  | DecodeInvalidPoint
    -- ^ a point lacks a compressed-encoding prefix (0x02 or 0x03)
  | DecodeInvalidAmount
    -- ^ an amount exceeds 21 million BTC
  | DecodeInvalidBool
    -- ^ a boolean byte is neither 0 nor 1
  | DecodeInvalidShortChannelIds
    -- ^ @encoded_short_ids@ lacks an encoding type, or isn't a whole
    --   number of short channel ids
  | DecodeUnknownEncoding !Word8
    -- ^ an encoded array uses an unknown encoding type (type 1, zlib,
    --   is no longer allowed)
  | DecodeCountMismatch
    -- ^ query flags, timestamps or checksums don't number one per
    --   short channel id
  | DecodeTlvError !TlvError
    -- ^ the message's TLV stream is malformed
  | DecodeInvalidTlvValue !Word64
    -- ^ a known TLV record (of the given type) has a malformed value
  | DecodeUnknownType !Word16
    -- ^ the message type is not one of BOLT #7's
  deriving (Eq, Show, Generic)

instance NFData DecodeError

-- messages -------------------------------------------------------------------

-- | Encode a 'Message' payload (excluding the 2-byte message type; see
--   'message_type'). Frame the result with
--   'Lightning.Protocol.BOLT1.encode_envelope', which enforces the
--   65535-byte message limit.
encode_message :: Message -> Either EncodeError BS.ByteString
encode_message m = case m of
  MsgChannelAnnouncement a     -> encode_channel_announcement a
  MsgNodeAnnouncement a        -> encode_node_announcement a
  MsgChannelUpdate a           -> Right (encode_channel_update a)
  MsgAnnouncementSignatures a  -> Right (encode_announcement_signatures a)
  MsgQueryShortChannelIds a    -> encode_query_short_channel_ids a
  MsgReplyShortChannelIdsEnd a -> Right (encode_reply_short_channel_ids_end a)
  MsgQueryChannelRange a       -> encode_query_channel_range a
  MsgReplyChannelRange a       -> encode_reply_channel_range a
  MsgGossipTimestampFilter a   -> Right (encode_gossip_timestamp_filter a)

-- | Decode a message payload, given its message type (as split off by
--   'Lightning.Protocol.BOLT1.decode_envelope').
--
--   >>> decode_message 260 "\NUL"
--   Left (DecodeUnknownType 260)
decode_message :: Word16 -> BS.ByteString -> Either DecodeError Message
decode_message t bs = case t of
  256 -> MsgChannelAnnouncement <$> decode_channel_announcement bs
  257 -> MsgNodeAnnouncement <$> decode_node_announcement bs
  258 -> MsgChannelUpdate <$> decode_channel_update bs
  259 -> MsgAnnouncementSignatures <$> decode_announcement_signatures bs
  261 -> MsgQueryShortChannelIds <$> decode_query_short_channel_ids bs
  262 -> MsgReplyShortChannelIdsEnd <$> decode_reply_short_channel_ids_end bs
  263 -> MsgQueryChannelRange <$> decode_query_channel_range bs
  264 -> MsgReplyChannelRange <$> decode_reply_channel_range bs
  265 -> MsgGossipTimestampFilter <$> decode_gossip_timestamp_filter bs
  _   -> Left (DecodeUnknownType t)

-- channel_announcement -------------------------------------------------------

-- | Encode a t'ChannelAnnouncement' payload. Fails if the features exceed
--   65535 bytes.
encode_channel_announcement
  :: ChannelAnnouncement -> Either EncodeError BS.ByteString
encode_channel_announcement m = do
  f <- prefixed (BOLT9.render (ca_features m))
  pure $ mconcat
    [ BOLT1.un_signature (ca_node_signature_1 m)
    , BOLT1.un_signature (ca_node_signature_2 m)
    , BOLT1.un_signature (ca_bitcoin_signature_1 m)
    , BOLT1.un_signature (ca_bitcoin_signature_2 m)
    , f
    , BOLT1.un_chain_hash (ca_chain_hash m)
    , BOLT1.encode_short_channel_id (ca_short_channel_id m)
    , BOLT1.un_point (ca_node_id_1 m)
    , BOLT1.un_point (ca_node_id_2 m)
    , BOLT1.un_point (ca_bitcoin_key_1 m)
    , BOLT1.un_point (ca_bitcoin_key_2 m)
    , BOLT1.encode_tlv_stream (ca_tlvs m)
    ]

-- | Decode a t'ChannelAnnouncement' payload.
decode_channel_announcement
  :: BS.ByteString -> Either DecodeError ChannelAnnouncement
decode_channel_announcement b0 = do
  (ns1, b1)  <- signature b0
  (ns2, b2)  <- signature b1
  (bs1, b3)  <- signature b2
  (bs2, b4)  <- signature b3
  (f, b5)    <- u16_prefixed b4
  (ch, b6)   <- chain_hash b5
  (s, b7)    <- short_channel_id b6
  (n1, b8)   <- point b7
  (n2, b9)   <- point b8
  (bk1, b10) <- point b9
  (bk2, b11) <- point b10
  ext        <- extension b11
  pure ChannelAnnouncement
    { ca_node_signature_1    = ns1
    , ca_node_signature_2    = ns2
    , ca_bitcoin_signature_1 = bs1
    , ca_bitcoin_signature_2 = bs2
    , ca_features            = BOLT9.parse f
    , ca_chain_hash          = ch
    , ca_short_channel_id    = s
    , ca_node_id_1           = n1
    , ca_node_id_2           = n2
    , ca_bitcoin_key_1       = bk1
    , ca_bitcoin_key_2       = bk2
    , ca_tlvs                = ext
    }

-- node_announcement ----------------------------------------------------------

-- | Encode a t'NodeAnnouncement' payload. Fails if the features or the
--   addresses exceed 65535 bytes, or if 'na_unknown_addresses' begins
--   with a descriptor of a known type.
encode_node_announcement
  :: NodeAnnouncement -> Either EncodeError BS.ByteString
encode_node_announcement m = do
  f <- prefixed (BOLT9.render (na_features m))
  let unknown = na_unknown_addresses m
  case BS.uncons unknown of
    Just (t, _) | t >= 1 && t <= 5 -> Left EncodeInvalidAddresses
    _ -> pure ()
  as <- prefixed (foldMap encode_address (na_addresses m) <> unknown)
  let RgbColor r g b = na_rgb_color m
      Alias al = na_alias m
  pure $ mconcat
    [ BOLT1.un_signature (na_signature m)
    , f
    , BOLT1.encode_u32 (na_timestamp m)
    , BOLT1.un_point (na_node_id m)
    , BS.pack [r, g, b]
    , al
    , as
    , BOLT1.encode_tlv_stream (na_tlvs m)
    ]

encode_address :: Address -> BS.ByteString
encode_address a = case a of
  AddrIPv4 (IPv4Addr b) p   -> BS.cons 1 (b <> BOLT1.encode_u16 p)
  AddrIPv6 (IPv6Addr b) p   -> BS.cons 2 (b <> BOLT1.encode_u16 p)
  AddrTorV2 (TorV2Addr b)   -> BS.cons 3 b
  AddrTorV3 (TorV3Addr b) p -> BS.cons 4 (b <> BOLT1.encode_u16 p)
  AddrDNS (Hostname h) p    ->
    BS.cons 5 (BS.cons (fromIntegral (BS.length h)) (h <> BOLT1.encode_u16 p))

-- | Decode a t'NodeAnnouncement' payload.
--
--   Address descriptors are parsed up to the first one of an unknown
--   type; it and the bytes after it are kept in 'na_unknown_addresses'.
--   A truncated descriptor of a known type is an error.
decode_node_announcement
  :: BS.ByteString -> Either DecodeError NodeAnnouncement
decode_node_announcement b0 = do
  (sig, b1) <- signature b0
  (f, b2)   <- u16_prefixed b1
  (ts, b3)  <- u32 b2
  (nid, b4) <- point b3
  (r, b5)   <- u8 b4
  (g, b6)   <- u8 b5
  (b, b7)   <- u8 b6
  (al, b8)  <- bytes 32 b7
  (as, b9)  <- u16_prefixed b8
  (known, unknown) <- addresses as
  ext <- extension b9
  pure NodeAnnouncement
    { na_signature         = sig
    , na_features          = BOLT9.parse f
    , na_timestamp         = ts
    , na_node_id           = nid
    , na_rgb_color         = RgbColor r g b
    , na_alias             = Alias al
    , na_addresses         = known
    , na_unknown_addresses = unknown
    , na_tlvs              = ext
    }

-- parse address descriptors, stopping at the first of an unknown type
addresses
  :: BS.ByteString -> Either DecodeError ([Address], BS.ByteString)
addresses = go []
  where
    go !acc !bs = case BS.uncons bs of
      Nothing -> Right (reverse acc, BS.empty)
      Just (t, r) -> case t of
        1 -> with_port acc 4 (AddrIPv4 . IPv4Addr) r
        2 -> with_port acc 16 (AddrIPv6 . IPv6Addr) r
        3 -> do
          (a, r1) <- bytes 12 r
          go (AddrTorV2 (TorV2Addr a) : acc) r1
        4 -> with_port acc 35 (AddrTorV3 . TorV3Addr) r
        5 -> do
          (n, r1) <- u8 r
          (h, r2) <- bytes (fromIntegral n) r1
          (p, r3) <- u16 r2
          go (AddrDNS (Hostname h) p : acc) r3
        _ -> Right (reverse acc, bs)

    with_port acc n mk r = do
      (a, r1) <- bytes n r
      (p, r2) <- u16 r1
      go (mk a p : acc) r2

-- channel_update -------------------------------------------------------------

-- | Encode a t'ChannelUpdate' payload.
encode_channel_update :: ChannelUpdate -> BS.ByteString
encode_channel_update m =
  let MessageFlags mf = cu_message_flags m
      ChannelFlags cf = cu_channel_flags m
  in  mconcat
        [ BOLT1.un_signature (cu_signature m)
        , BOLT1.un_chain_hash (cu_chain_hash m)
        , BOLT1.encode_short_channel_id (cu_short_channel_id m)
        , BOLT1.encode_u32 (cu_timestamp m)
        , BS.pack [mf, cf]
        , BOLT1.encode_u16 (cu_cltv_expiry_delta m)
        , BOLT1.encode_milli_satoshi (cu_htlc_minimum_msat m)
        , BOLT1.encode_u32 (cu_fee_base_msat m)
        , BOLT1.encode_u32 (cu_fee_proportional_millionths m)
        , BOLT1.encode_milli_satoshi (cu_htlc_maximum_msat m)
        , BOLT1.encode_tlv_stream (cu_tlvs m)
        ]

-- | Decode a t'ChannelUpdate' payload.
--
--   @htlc_maximum_msat@ is always present; the @must_be_one@ bit of
--   @message_flags@ is kept but not interpreted.
decode_channel_update :: BS.ByteString -> Either DecodeError ChannelUpdate
decode_channel_update b0 = do
  (sig, b1)   <- signature b0
  (ch, b2)    <- chain_hash b1
  (s, b3)     <- short_channel_id b2
  (ts, b4)    <- u32 b3
  (mf, b5)    <- u8 b4
  (cf, b6)    <- u8 b5
  (cltv, b7)  <- u16 b6
  (hmin, b8)  <- milli_satoshi b7
  (fb, b9)    <- u32 b8
  (fp, b10)   <- u32 b9
  (hmax, b11) <- milli_satoshi b10
  ext         <- extension b11
  pure ChannelUpdate
    { cu_signature                   = sig
    , cu_chain_hash                  = ch
    , cu_short_channel_id            = s
    , cu_timestamp                   = ts
    , cu_message_flags               = MessageFlags mf
    , cu_channel_flags               = ChannelFlags cf
    , cu_cltv_expiry_delta           = cltv
    , cu_htlc_minimum_msat           = hmin
    , cu_fee_base_msat               = fb
    , cu_fee_proportional_millionths = fp
    , cu_htlc_maximum_msat           = hmax
    , cu_tlvs                        = ext
    }

-- announcement_signatures ----------------------------------------------------

-- | Encode an t'AnnouncementSignatures' payload.
encode_announcement_signatures :: AnnouncementSignatures -> BS.ByteString
encode_announcement_signatures m = mconcat
  [ BOLT1.un_channel_id (as_channel_id m)
  , BOLT1.encode_short_channel_id (as_short_channel_id m)
  , BOLT1.un_signature (as_node_signature m)
  , BOLT1.un_signature (as_bitcoin_signature m)
  , BOLT1.encode_tlv_stream (as_tlvs m)
  ]

-- | Decode an t'AnnouncementSignatures' payload.
decode_announcement_signatures
  :: BS.ByteString -> Either DecodeError AnnouncementSignatures
decode_announcement_signatures b0 = do
  (cid, b1) <- need (BOLT1.decode_channel_id b0)
  (s, b2)   <- short_channel_id b1
  (ns, b3)  <- signature b2
  (bs, b4)  <- signature b3
  ext       <- extension b4
  pure (AnnouncementSignatures cid s ns bs ext)

-- query_short_channel_ids ----------------------------------------------------

-- | Encode a t'QueryShortChannelIds' payload. Fails if the short channel
--   ids exceed the u16 length limit (8191 of them), if the query flags
--   don't number one per short channel id, or if 'qsci_tlvs' holds a
--   record of type 1.
encode_query_short_channel_ids
  :: QueryShortChannelIds -> Either EncodeError BS.ByteString
encode_query_short_channel_ids m = do
  let scids = qsci_short_channel_ids m
  ids <- prefixed (encode_short_ids scids)
  flags <- case qsci_query_flags m of
    Nothing -> pure []
    Just fs
      | length fs /= length scids -> Left EncodeCountMismatch
      | otherwise -> pure
          [TlvRecord 1 (BS.cons 0 (foldMap (\(QueryFlags w) ->
                                       BOLT1.encode_bigsize w) fs))]
  t <- merge [1] flags (qsci_tlvs m)
  pure (BOLT1.un_chain_hash (qsci_chain_hash m) <> ids <> t)

-- | Decode a t'QueryShortChannelIds' payload.
--
--   Fails on an unknown encoding type (including the deprecated zlib
--   encoding), on @encoded_short_ids@ that isn't a whole number of short
--   channel ids, and on query flags that don't number one per short
--   channel id.
--
--   >>> let zlib = BS.replicate 32 0 <> "\NUL\STX\SOHx"
--   >>> decode_query_short_channel_ids zlib
--   Left (DecodeUnknownEncoding 1)
decode_query_short_channel_ids
  :: BS.ByteString -> Either DecodeError QueryShortChannelIds
decode_query_short_channel_ids b0 = do
  (ch, b1)  <- chain_hash b0
  (ids, b2) <- u16_prefixed b1
  scids     <- short_ids ids
  s         <- tlvs [1] b2
  flags     <- traverse (query_flags_value (length scids))
                 (BOLT1.lookup_tlv 1 s)
  pure QueryShortChannelIds
    { qsci_chain_hash        = ch
    , qsci_short_channel_ids = scids
    , qsci_query_flags       = flags
    , qsci_tlvs              = BOLT1.filter_tlv_stream (/= 1) s
    }

query_flags_value :: Int -> BS.ByteString -> Either DecodeError [QueryFlags]
query_flags_value n v = case BS.uncons v of
  Nothing -> Left (DecodeInvalidTlvValue 1)
  Just (0, r) -> do
    fs <- bigsizes r
    if length fs /= n
      then Left DecodeCountMismatch
      else Right (map QueryFlags fs)
  Just (e, _) -> Left (DecodeUnknownEncoding e)
  where
    bigsizes !r
      | BS.null r = Right []
      | otherwise = case BOLT1.decode_bigsize r of
          Nothing      -> Left (DecodeInvalidTlvValue 1)
          Just (w, r1) -> (w :) <$> bigsizes r1

-- reply_short_channel_ids_end ------------------------------------------------

-- | Encode a t'ReplyShortChannelIdsEnd' payload.
encode_reply_short_channel_ids_end
  :: ReplyShortChannelIdsEnd -> BS.ByteString
encode_reply_short_channel_ids_end m = mconcat
  [ BOLT1.un_chain_hash (rsce_chain_hash m)
  , encode_bool (rsce_full_information m)
  , BOLT1.encode_tlv_stream (rsce_tlvs m)
  ]

-- | Decode a t'ReplyShortChannelIdsEnd' payload.
--
--   >>> decode_reply_short_channel_ids_end (BS.replicate 32 0 <> "\STX")
--   Left DecodeInvalidBool
decode_reply_short_channel_ids_end
  :: BS.ByteString -> Either DecodeError ReplyShortChannelIdsEnd
decode_reply_short_channel_ids_end b0 = do
  (ch, b1)   <- chain_hash b0
  (full, b2) <- bool b1
  ext        <- extension b2
  pure (ReplyShortChannelIdsEnd ch full ext)

-- query_channel_range --------------------------------------------------------

-- | Encode a t'QueryChannelRange' payload. Fails if 'qcr_tlvs' holds a
--   record of type 1.
encode_query_channel_range
  :: QueryChannelRange -> Either EncodeError BS.ByteString
encode_query_channel_range m = do
  let opt = case qcr_query_option m of
        Nothing              -> []
        Just (QueryOption w) -> [TlvRecord 1 (BOLT1.encode_bigsize w)]
  t <- merge [1] opt (qcr_tlvs m)
  pure $ mconcat
    [ BOLT1.un_chain_hash (qcr_chain_hash m)
    , BOLT1.encode_u32 (qcr_first_blocknum m)
    , BOLT1.encode_u32 (qcr_number_of_blocks m)
    , t
    ]

-- | Decode a t'QueryChannelRange' payload.
decode_query_channel_range
  :: BS.ByteString -> Either DecodeError QueryChannelRange
decode_query_channel_range b0 = do
  (ch, b1)    <- chain_hash b0
  (first, b2) <- u32 b1
  (num, b3)   <- u32 b2
  s           <- tlvs [1] b3
  opt         <- traverse option (BOLT1.lookup_tlv 1 s)
  pure QueryChannelRange
    { qcr_chain_hash       = ch
    , qcr_first_blocknum   = first
    , qcr_number_of_blocks = num
    , qcr_query_option     = opt
    , qcr_tlvs             = BOLT1.filter_tlv_stream (/= 1) s
    }
  where
    option v = case BOLT1.decode_bigsize v of
      Just (w, r) | BS.null r -> Right (QueryOption w)
      _ -> Left (DecodeInvalidTlvValue 1)

-- reply_channel_range --------------------------------------------------------

-- | Encode a t'ReplyChannelRange' payload. Fails if the short channel ids
--   exceed the u16 length limit (8191 of them), if the timestamps or
--   checksums don't number one per short channel id, or if 'rcr_tlvs'
--   holds a record of type 1 or 3.
encode_reply_channel_range
  :: ReplyChannelRange -> Either EncodeError BS.ByteString
encode_reply_channel_range m = do
  let scids = rcr_short_channel_ids m
      n = length scids
      count xs
        | length xs /= n = Left EncodeCountMismatch
        | otherwise      = Right ()
  ids <- prefixed (encode_short_ids scids)
  ts <- case rcr_timestamps m of
    Nothing -> pure []
    Just xs -> do
      count xs
      pure [TlvRecord 1 (BS.cons 0 (foldMap
        (\(ChannelUpdateTimestamps a b) -> pair a b) xs))]
  cs <- case rcr_checksums m of
    Nothing -> pure []
    Just xs -> do
      count xs
      pure [TlvRecord 3 (foldMap
        (\(ChannelUpdateChecksums a b) -> pair a b) xs)]
  t <- merge [1, 3] (ts <> cs) (rcr_tlvs m)
  pure $ mconcat
    [ BOLT1.un_chain_hash (rcr_chain_hash m)
    , BOLT1.encode_u32 (rcr_first_blocknum m)
    , BOLT1.encode_u32 (rcr_number_of_blocks m)
    , encode_bool (rcr_sync_complete m)
    , ids
    , t
    ]
  where
    pair a b = BOLT1.encode_u32 a <> BOLT1.encode_u32 b

-- | Decode a t'ReplyChannelRange' payload.
--
--   Fails on an unknown encoding type (including the deprecated zlib
--   encoding), on @encoded_short_ids@ that isn't a whole number of short
--   channel ids, and on timestamps or checksums that don't number one
--   per short channel id.
decode_reply_channel_range
  :: BS.ByteString -> Either DecodeError ReplyChannelRange
decode_reply_channel_range b0 = do
  (ch, b1)    <- chain_hash b0
  (first, b2) <- u32 b1
  (num, b3)   <- u32 b2
  (sync, b4)  <- bool b3
  (ids, b5)   <- u16_prefixed b4
  scids       <- short_ids ids
  s           <- tlvs [1, 3] b5
  let n = length scids
  ts <- traverse (timestamps n) (BOLT1.lookup_tlv 1 s)
  cs <- traverse (pairs 3 ChannelUpdateChecksums n) (BOLT1.lookup_tlv 3 s)
  pure ReplyChannelRange
    { rcr_chain_hash        = ch
    , rcr_first_blocknum    = first
    , rcr_number_of_blocks  = num
    , rcr_sync_complete     = sync
    , rcr_short_channel_ids = scids
    , rcr_timestamps        = ts
    , rcr_checksums         = cs
    , rcr_tlvs              = BOLT1.filter_tlv_stream (\t -> t /= 1 && t /= 3) s
    }
  where
    timestamps n v = case BS.uncons v of
      Nothing     -> Left (DecodeInvalidTlvValue 1)
      Just (0, r) -> pairs 1 ChannelUpdateTimestamps n r
      Just (e, _) -> Left (DecodeUnknownEncoding e)

-- decode an array of u32 pairs holding n elements
pairs
  :: Word64 -> (Word32 -> Word32 -> a) -> Int -> BS.ByteString
  -> Either DecodeError [a]
pairs t mk n r
  | BS.length r `rem` 8 /= 0  = Left (DecodeInvalidTlvValue t)
  | BS.length r `quot` 8 /= n = Left DecodeCountMismatch
  | otherwise                 = Right (go r)
  where
    go !bs = case BOLT1.decode_u32 bs of
      Nothing -> []
      Just (a, r1) -> case BOLT1.decode_u32 r1 of
        Nothing      -> []
        Just (b, r2) -> mk a b : go r2

-- gossip_timestamp_filter ----------------------------------------------------

-- | Encode a t'GossipTimestampFilter' payload.
encode_gossip_timestamp_filter :: GossipTimestampFilter -> BS.ByteString
encode_gossip_timestamp_filter m = mconcat
  [ BOLT1.un_chain_hash (gtf_chain_hash m)
  , BOLT1.encode_u32 (gtf_first_timestamp m)
  , BOLT1.encode_u32 (gtf_timestamp_range m)
  , BOLT1.encode_tlv_stream (gtf_tlvs m)
  ]

-- | Decode a t'GossipTimestampFilter' payload.
decode_gossip_timestamp_filter
  :: BS.ByteString -> Either DecodeError GossipTimestampFilter
decode_gossip_timestamp_filter b0 = do
  (ch, b1)    <- chain_hash b0
  (first, b2) <- u32 b1
  (range, b3) <- u32 b2
  ext         <- extension b3
  pure (GossipTimestampFilter ch first range ext)

-- encoded short channel ids --------------------------------------------------

-- encoding type 0 (uncompressed), the only one allowed
encode_short_ids :: [ShortChannelId] -> BS.ByteString
encode_short_ids = BS.cons 0 . foldMap BOLT1.encode_short_channel_id

short_ids :: BS.ByteString -> Either DecodeError [ShortChannelId]
short_ids bs = case BS.uncons bs of
  Nothing -> Left DecodeInvalidShortChannelIds
  Just (0, r)
    | BS.length r `rem` 8 == 0 -> Right (go r)
    | otherwise                -> Left DecodeInvalidShortChannelIds
  Just (e, _) -> Left (DecodeUnknownEncoding e)
  where
    go !r = case BOLT1.decode_short_channel_id r of
      Nothing      -> []
      Just (s, r1) -> s : go r1

-- helpers --------------------------------------------------------------------

need :: Maybe a -> Either DecodeError a
need = maybe (Left DecodeInsufficientBytes) Right
{-# INLINE need #-}

u8 :: BS.ByteString -> Either DecodeError (Word8, BS.ByteString)
u8 = need . BS.uncons
{-# INLINE u8 #-}

u16 :: BS.ByteString -> Either DecodeError (Word16, BS.ByteString)
u16 = need . BOLT1.decode_u16
{-# INLINE u16 #-}

u32 :: BS.ByteString -> Either DecodeError (Word32, BS.ByteString)
u32 = need . BOLT1.decode_u32
{-# INLINE u32 #-}

bytes
  :: Int -> BS.ByteString
  -> Either DecodeError (BS.ByteString, BS.ByteString)
bytes n bs
  | BS.length bs < n = Left DecodeInsufficientBytes
  | otherwise        = Right (BS.splitAt n bs)
{-# INLINE bytes #-}

u16_prefixed
  :: BS.ByteString -> Either DecodeError (BS.ByteString, BS.ByteString)
u16_prefixed = need . BOLT1.decode_u16_prefixed
{-# INLINE u16_prefixed #-}

bool :: BS.ByteString -> Either DecodeError (Bool, BS.ByteString)
bool bs = do
  (b, r) <- u8 bs
  case b of
    0 -> Right (False, r)
    1 -> Right (True, r)
    _ -> Left DecodeInvalidBool

signature :: BS.ByteString -> Either DecodeError (Signature, BS.ByteString)
signature = need . BOLT1.decode_signature
{-# INLINE signature #-}

chain_hash :: BS.ByteString -> Either DecodeError (ChainHash, BS.ByteString)
chain_hash = need . BOLT1.decode_chain_hash
{-# INLINE chain_hash #-}

short_channel_id
  :: BS.ByteString -> Either DecodeError (ShortChannelId, BS.ByteString)
short_channel_id = need . BOLT1.decode_short_channel_id
{-# INLINE short_channel_id #-}

point :: BS.ByteString -> Either DecodeError (Point, BS.ByteString)
point bs = do
  (b, r) <- bytes 33 bs
  case BOLT1.point b of
    Nothing -> Left DecodeInvalidPoint
    Just p  -> Right (p, r)

milli_satoshi
  :: BS.ByteString -> Either DecodeError (MilliSatoshi, BS.ByteString)
milli_satoshi bs = do
  (w, r) <- need (BOLT1.decode_u64 bs)
  case BOLT1.milli_satoshi w of
    Nothing -> Left DecodeInvalidAmount
    Just a  -> Right (a, r)

-- a TLV stream with the given known types
tlvs :: [Word64] -> BS.ByteString -> Either DecodeError TlvStream
tlvs known bs = either (Left . DecodeTlvError) Right
  (BOLT1.decode_tlv_stream (`elem` known) bs)

-- an extension stream, in which no types are known
extension :: BS.ByteString -> Either DecodeError TlvStream
extension = tlvs []
{-# INLINE extension #-}

prefixed :: BS.ByteString -> Either EncodeError BS.ByteString
prefixed bs =
  maybe (Left EncodeLengthOverflow) Right (BOLT1.encode_u16_prefixed bs)
{-# INLINE prefixed #-}

encode_bool :: Bool -> BS.ByteString
encode_bool b = BS.singleton (if b then 1 else 0)
{-# INLINE encode_bool #-}

-- encode typed records together with the extra records of a @_tlvs@
-- field, which mustn't use any of the message's known types
merge
  :: [Word64] -> [TlvRecord] -> TlvStream -> Either EncodeError BS.ByteString
merge known typed extra
  | any ((`elem` known) . tlv_type) rs = Left EncodeInvalidTlvs
  | otherwise = maybe (Left EncodeInvalidTlvs) (Right . BOLT1.encode_tlv_stream)
      (BOLT1.tlv_stream (typed <> rs))
  where
    rs = BOLT1.un_tlv_stream extra
