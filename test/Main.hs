{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bits ((.&.), (.|.), shiftR, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Word (Word8, Word16, Word32, Word64)
import Lightning.Protocol.BOLT1
  ( ChainHash, Point, ShortChannelId(..), Signature, TlvError(..)
  , TlvRecord(..), TlvStream )
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT7
import qualified Lightning.Protocol.BOLT9 as BOLT9
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))
import Vectors

main :: IO ()
main = defaultMain $ testGroup "ppad-bolt7" [
    field_tests
  , spec_vector_tests
  , mainnet_tests
  , node_announcement_tests
  , channel_update_tests
  , query_tests
  , other_message_tests
  , encode_error_tests
  , dispatch_tests
  , validation_tests
  , checksum_tests
  , property_tests
  ]

-- helpers --------------------------------------------------------------------

must :: String -> Maybe a -> IO a
must what = maybe (assertFailure ("unexpected Nothing: " ++ what)) pure

expectRight :: Show e => String -> Either e a -> IO a
expectRight what =
  either (\e -> assertFailure (what ++ " failed: " ++ show e)) pure

hex :: BS.ByteString -> IO BS.ByteString
hex h = must ("hex " ++ show h) (B16.decode h)

-- total helpers for pure contexts (properties)
chain_hash_of :: Word8 -> Maybe ChainHash
chain_hash_of w = BOLT1.chain_hash (BS.replicate 32 w)

big_endian :: Int -> Word64 -> BS.ByteString
big_endian n w =
  BS.pack [ fromIntegral (w `shiftR` (8 * i)) | i <- [n - 1, n - 2 .. 0] ]

u16be :: Int -> BS.ByteString
u16be = big_endian 2 . fromIntegral

-- replace the byte at an offset (no-op past the end)
set_byte :: Int -> Word8 -> BS.ByteString -> BS.ByteString
set_byte i w bs
  | i < 0 || i >= BS.length bs = bs
  | otherwise = BS.take i bs <> BS.singleton w <> BS.drop (i + 1) bs

-- | Fixture values, built with the smart constructors.
data Fx = Fx
  { fx_chain  :: ChainHash
  , fx_sig    :: Signature
  , fx_node_1 :: Point
  , fx_node_2 :: Point
  , fx_cid    :: BOLT1.ChannelId
  , fx_scid   :: ShortChannelId
  , fx_alias  :: Alias
  , fx_scids  :: [ShortChannelId]
  }

fixtures :: IO Fx
fixtures = do
  ch <- must "chain_hash" (chain_hash_of 0x01)
  sg <- must "signature" (BOLT1.signature (BS.replicate 64 0xee))
  n1 <- must "point" (BOLT1.point (BS.cons 0x02 (BS.replicate 32 0xbb)))
  n2 <- must "point" (BOLT1.point (BS.cons 0x03 (BS.replicate 32 0xaa)))
  ci <- must "channel_id" (BOLT1.channel_id (BS.replicate 32 0xcd))
  sc <- must "short_channel_id" (BOLT1.short_channel_id 539268 845 1)
  al <- must "alias" (alias (BS.replicate 32 0))
  ss <- must "short_channel_ids" $ traverse
          (\(b, t, o) -> BOLT1.short_channel_id b t o)
          [(100000, 1, 0), (200000, 2, 1), (300000, 3, 2)]
  pure Fx
    { fx_chain  = ch
    , fx_sig    = sg
    , fx_node_1 = n1
    , fx_node_2 = n2
    , fx_cid    = ci
    , fx_scid   = sc
    , fx_alias  = al
    , fx_scids  = ss
    }

msat :: Word64 -> IO BOLT1.MilliSatoshi
msat = must "milli_satoshi" . BOLT1.milli_satoshi

update_fixture :: Fx -> IO ChannelUpdate
update_fixture fx = do
  hmin <- msat 1000
  hmax <- msat 1000000000
  pure ChannelUpdate
    { cu_signature                   = fx_sig fx
    , cu_chain_hash                  = fx_chain fx
    , cu_short_channel_id            = fx_scid fx
    , cu_timestamp                   = 1234567890
    , cu_message_flags               = message_flags Forward
    , cu_channel_flags               = channel_flags NodeOne Enabled
    , cu_cltv_expiry_delta           = 144
    , cu_htlc_minimum_msat           = hmin
    , cu_fee_base_msat               = 1000
    , cu_fee_proportional_millionths = 100
    , cu_htlc_maximum_msat           = hmax
    , cu_tlvs                        = BOLT1.empty_tlv_stream
    }

announcement_fixture :: Fx -> ChannelAnnouncement
announcement_fixture fx = ChannelAnnouncement
  { ca_node_signature_1    = fx_sig fx
  , ca_node_signature_2    = fx_sig fx
  , ca_bitcoin_signature_1 = fx_sig fx
  , ca_bitcoin_signature_2 = fx_sig fx
  , ca_features            = BOLT9.empty
  , ca_chain_hash          = fx_chain fx
  , ca_short_channel_id    = fx_scid fx
  , ca_node_id_1           = fx_node_1 fx
  , ca_node_id_2           = fx_node_2 fx
  , ca_bitcoin_key_1       = fx_node_1 fx
  , ca_bitcoin_key_2       = fx_node_2 fx
  , ca_tlvs                = BOLT1.empty_tlv_stream
  }

node_fixture :: Fx -> [Address] -> NodeAnnouncement
node_fixture fx addrs = NodeAnnouncement
  { na_signature         = fx_sig fx
  , na_features          = BOLT9.empty
  , na_timestamp         = 1234567890
  , na_node_id           = fx_node_2 fx
  , na_rgb_color         = RgbColor 0xff 0x00 0x00
  , na_alias             = fx_alias fx
  , na_addresses         = addrs
  , na_unknown_addresses = BS.empty
  , na_tlvs              = BOLT1.empty_tlv_stream
  }

tlvs_of :: [TlvRecord] -> IO TlvStream
tlvs_of = must "tlv_stream" . BOLT1.tlv_stream

-- field types ----------------------------------------------------------------

field_tests :: TestTree
field_tests = testGroup "field types" [
    testCase "format_short_channel_id" $ do
      let fmt b t o =
            fmap format_short_channel_id (BOLT1.short_channel_id b t o)
      fmt 539268 845 1 @?= Just "539268x845x1"
      fmt 0 0 0 @?= Just "0x0x0"
      fmt 0xffffff 0xffffff 0xffff @?= Just "16777215x16777215x65535"
  , testCase "message_flags" $ do
      message_flags Forward @?= MessageFlags 0x01
      message_flags DontForward @?= MessageFlags 0x03
      map (message_flags_forwarding . MessageFlags) [0, 1, 2, 3, 0xfd]
        @?= [Forward, Forward, DontForward, DontForward, Forward]
      map (has_must_be_one . MessageFlags) [0, 1, 2, 0xfe]
        @?= [False, True, False, False]
  , testCase "channel_flags" $ do
      [ channel_flags d s | d <- [NodeOne, NodeTwo], s <- [Enabled, Disabled] ]
        @?= map ChannelFlags [0, 2, 1, 3]
      map (channel_flags_direction . ChannelFlags) [0x04, 0x05, 0xfe]
        @?= [NodeOne, NodeTwo, NodeOne]
      map (channel_flags_status . ChannelFlags) [0x04, 0x06, 0xfd]
        @?= [Enabled, Disabled, Enabled]
  , testCase "query_flags" $ do
      query_flags [] @?= QueryFlags 0
      query_flags [minBound .. maxBound] @?= QueryFlags 31
      query_flags [WantNodeAnnouncement2, WantChannelUpdate1] @?= QueryFlags 18
      map (`has_query_flag` QueryFlags 0x25) [minBound .. maxBound]
        @?= [True, False, True, False, False]
  , testCase "query_option" $ do
      query_option [WantChecksums] @?= QueryOption 2
      map (`has_query_option` QueryOption 0x05) [minBound .. maxBound]
        @?= [True, False]
  , testCase "fixed-size constructors check lengths" $ do
      let lens mk = [ mk (BS.replicate n 0) /= Nothing | n <- [0 .. 40] ]
          only n = [ k == n | k <- [0 .. 40 :: Int] ]
      lens alias @?= only 32
      lens ipv4_addr @?= only 4
      lens ipv6_addr @?= only 16
      lens tor_v2_addr @?= only 12
      lens tor_v3_addr @?= only 35
  , testCase "hostname" $ do
      fmap un_hostname (hostname "ln.example.com") @?= Just "ln.example.com"
      fmap un_hostname (hostname "") @?= Just ""
      fmap (BS.length . un_hostname) (hostname (BS.replicate 255 0x61))
        @?= Just 255
      hostname (BS.replicate 256 0x61) @?= Nothing
      let ascii h = fmap is_ascii_hostname (hostname h)
      ascii "xn--caf-dma" @?= Just True
      ascii "caf\233" @?= Just False
      ascii "\0\255" @?= Just False
      ascii "" @?= Just False
  , testCase "usable_addresses" $ do
      v4 <- must "ipv4" (ipv4_addr "\127\0\0\1")
      v6 <- must "ipv6" (ipv6_addr (BS.replicate 16 1))
      t2 <- must "tor v2" (tor_v2_addr (BS.replicate 12 0xaa))
      t3 <- must "tor v3" (tor_v3_addr (BS.replicate 35 0xbb))
      h1 <- must "hostname" (hostname "a.example.com")
      h2 <- must "hostname" (hostname "b.example.com")
      bad <- must "hostname" (hostname "caf\233")
      usable_addresses
        [ AddrIPv4 v4 9735, AddrTorV2 t2, AddrIPv6 v6 0, AddrTorV3 t3 9735
        , AddrDNS h1 9735, AddrDNS h2 9735, AddrIPv6 v6 9735 ]
        @?= [ AddrIPv4 v4 9735, AddrTorV3 t3 9735, AddrDNS h1 9735
            , AddrIPv6 v6 9735 ]
      -- a hostname that is ignored still counts as the first
      usable_addresses [AddrDNS bad 9735, AddrDNS h1 9735] @?= []
      usable_addresses [AddrDNS h1 0, AddrDNS h2 9735] @?= []
  ]

-- extended-queries.json ------------------------------------------------------

-- the vectors' chain hash (regtest)
regtest :: IO ChainHash
regtest = hex
  "0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206"
  >>= must "chain_hash" . BOLT1.chain_hash

scids :: [(Word32, Word32, Word16)] -> IO [ShortChannelId]
scids = must "short_channel_id"
  . traverse (\(b, t, o) -> BOLT1.short_channel_id b t o)

-- decode a vector, compare with the expected message, and re-encode the
-- expected message
known_answer :: BS.ByteString -> Message -> Assertion
known_answer h expected = do
  wire <- hex h
  (t, payload) <- expectRight "decode_envelope" (BOLT1.decode_envelope wire)
  decode_message t payload @?= Right expected
  message_type expected @?= t
  encode_message expected @?= Right payload

rejected :: BS.ByteString -> DecodeError -> Assertion
rejected h err = do
  wire <- hex h
  (t, payload) <- expectRight "decode_envelope" (BOLT1.decode_envelope wire)
  decode_message t payload @?= Left err

spec_vector_tests :: TestTree
spec_vector_tests = testGroup "bolt07/extended-queries.json" $
  case query_vectors of
    [v0, v1, v2, v3, v4, v5, v6, v7, v8, v9] -> [
        testCase "0: query_channel_range" $ do
          ch <- regtest
          known_answer v0 $ MsgQueryChannelRange
            (QueryChannelRange ch 100000 1500 Nothing BOLT1.empty_tlv_stream)
      , testCase "1: query_channel_range with query_option" $ do
          ch <- regtest
          known_answer v1 $ MsgQueryChannelRange $ QueryChannelRange ch
            35000 100 (Just (query_option [WantTimestamps, WantChecksums]))
            BOLT1.empty_tlv_stream
      , testCase "2: reply_channel_range" $ do
          ch <- regtest
          ss <- scids [(0, 0, 142), (0, 0, 15465), (0, 69, 42692)]
          known_answer v2 $ MsgReplyChannelRange
            (ReplyChannelRange ch 756230 1500 True ss Nothing Nothing
              BOLT1.empty_tlv_stream)
      , testCase "3: reply_channel_range, zlib (rejected)" $
          rejected v3 (DecodeUnknownEncoding 1)
      , testCase "4: reply_channel_range with timestamps and checksums" $ do
          ch <- regtest
          ss <- scids [(0, 0, 12355), (0, 7, 30934), (0, 70, 57793)]
          known_answer v4 $ MsgReplyChannelRange $
            ReplyChannelRange ch 122334 1500 True ss
              (Just [ ChannelUpdateTimestamps 164545 948165
                    , ChannelUpdateTimestamps 489645 4786864
                    , ChannelUpdateTimestamps 46456 9788415 ])
              (Just [ ChannelUpdateChecksums 1111 2222
                    , ChannelUpdateChecksums 3333 4444
                    , ChannelUpdateChecksums 5555 6666 ])
              BOLT1.empty_tlv_stream
      , testCase "5: reply_channel_range, zlib (rejected)" $
          rejected v5 (DecodeUnknownEncoding 1)
      , testCase "6: query_short_channel_ids" $ do
          ch <- regtest
          ss <- scids [(0, 0, 142), (0, 0, 15465), (0, 69, 42692)]
          known_answer v6 $ MsgQueryShortChannelIds
            (QueryShortChannelIds ch ss Nothing BOLT1.empty_tlv_stream)
      , testCase "7: query_short_channel_ids, zlib (rejected)" $
          rejected v7 (DecodeUnknownEncoding 1)
      , testCase "8: query_short_channel_ids, zlib query_flags (rejected)" $
          rejected v8 (DecodeUnknownEncoding 1)
      , testCase "9: query_short_channel_ids, zlib (rejected)" $
          rejected v9 (DecodeUnknownEncoding 1)
      ]
    _ -> [testCase "vector count" (assertFailure "expected 10 vectors")]

-- mainnet gossip -------------------------------------------------------------

decoded_announcements :: IO [(BS.ByteString, ChannelAnnouncement)]
decoded_announcements = traverse go mainnet_announcements
  where
    go v = do
      p <- hex (av_payload v)
      a <- expectRight "decode_channel_announcement"
             (decode_channel_announcement p)
      pure (p, a)

-- the key of the node that signed an update
signer :: ChannelAnnouncement -> ChannelUpdate -> Point
signer a u = case channel_flags_direction (cu_channel_flags u) of
  NodeOne -> ca_node_id_1 a
  NodeTwo -> ca_node_id_2 a

other :: ChannelAnnouncement -> ChannelUpdate -> Point
other a u = case channel_flags_direction (cu_channel_flags u) of
  NodeOne -> ca_node_id_2 a
  NodeTwo -> ca_node_id_1 a

mainnet_tests :: TestTree
mainnet_tests = testGroup "mainnet gossip" [
    testCase "channel_announcements: decode, re-encode, hash, verify" $ do
      anns <- decoded_announcements
      length anns @?= 6
      mapM_ (\(v, (p, a)) -> do
          encode_channel_announcement a @?= Right p
          d <- hex (av_digest v)
          channel_announcement_hash a @?= Right d
          assertBool "signatures verify" (verify_channel_announcement a)
          validate_channel_announcement a @?= Right ())
        (zip mainnet_announcements anns)
  , testCase "channel_announcement fields" $ do
      anns <- decoded_announcements
      (_, a) <- must "first announcement" (safe_head anns)
      mainnet <- hex
        "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
      n1 <- hex
        "02bb5e47ee26aca1654ce092d9d06eac15d953210c737f2220e2d7e54c4f108677"
      n2 <- hex
        "030b828fd1e42549fbc2f1d0960ec3663362c41777d3d87fe1b8e1d4bf063abcaa"
      format_short_channel_id (ca_short_channel_id a) @?= "850052x2992x1"
      BOLT1.un_chain_hash (ca_chain_hash a) @?= mainnet
      BOLT9.render (ca_features a) @?= BS.empty
      BOLT1.un_point (ca_node_id_1 a) @?= n1
      BOLT1.un_point (ca_node_id_2 a) @?= n2
      BOLT1.un_tlv_stream (ca_tlvs a) @?= []
  , testCase "channel_announcement: tampering breaks verification" $ do
      anns <- decoded_announcements
      (p, a) <- must "first announcement" (safe_head anns)
      -- change the short channel id (signed)
      let p' = set_byte 295 0xff p
      a' <- expectRight "decode" (decode_channel_announcement p')
      assertBool "tampered" (not (verify_channel_announcement a'))
      let swapped = a { ca_node_signature_1 = ca_node_signature_2 a
                      , ca_node_signature_2 = ca_node_signature_1 a }
      assertBool "swapped" (not (verify_channel_announcement swapped))
  , testCase "channel_updates: decode, re-encode, hash, checksum, verify" $ do
      anns <- decoded_announcements
      length mainnet_updates @?= 13
      mapM_ (\v -> do
          p <- hex (uv_payload v)
          u <- expectRight "decode_channel_update" (decode_channel_update p)
          (_, a) <- must "announcement" (safe_index anns (uv_ann v))
          cu_short_channel_id u @?= ca_short_channel_id a
          encode_channel_update u @?= p
          d <- hex (uv_digest v)
          channel_update_hash u @?= d
          channel_update_checksum u @?= uv_checksum v
          assertBool "verifies under its signer"
            (verify_channel_update (signer a u) u)
          assertBool "fails under the other node"
            (not (verify_channel_update (other a u) u)))
        mainnet_updates
  , testCase "channel_update fields (136 bytes)" $ do
      u <- update_vector 1
      BS.length (encode_channel_update u) @?= 136
      format_short_channel_id (cu_short_channel_id u) @?= "850052x2992x1"
      cu_timestamp u @?= 1776493010
      cu_message_flags u @?= MessageFlags 0x01
      cu_channel_flags u @?= channel_flags NodeTwo Disabled
      cu_cltv_expiry_delta u @?= 80
      BOLT1.un_milli_satoshi (cu_htlc_minimum_msat u) @?= 1000
      cu_fee_base_msat u @?= 1000
      cu_fee_proportional_millionths u @?= 800
      BOLT1.un_milli_satoshi (cu_htlc_maximum_msat u) @?= 2970000000
      BOLT1.un_tlv_stream (cu_tlvs u) @?= []
  , testCase "channel_update fields (148 bytes, inbound-fee extension)" $ do
      u <- update_vector 0
      ext <- hex "00000000ffffffed"
      cu_timestamp u @?= 1779047542
      cu_channel_flags u @?= channel_flags NodeOne Enabled
      cu_cltv_expiry_delta u @?= 100
      cu_fee_base_msat u @?= 0
      cu_fee_proportional_millionths u @?= 5000
      BOLT1.un_milli_satoshi (cu_htlc_maximum_msat u) @?= 2970000000
      BOLT1.un_tlv_stream (cu_tlvs u) @?= [TlvRecord 55555 ext]
  , testCase "a real update with htlc_minimum_msat > htlc_maximum_msat" $ do
      u <- update_vector 8
      validate_channel_update u @?= Left ValidateHtlcAmounts
  , testCase "dropping the extension breaks verification" $ do
      anns <- decoded_announcements
      (_, a) <- must "announcement" (safe_head anns)
      u <- update_vector 0
      let u' = u { cu_tlvs = BOLT1.empty_tlv_stream }
      assertBool "with" (verify_channel_update (signer a u) u)
      assertBool "without" (not (verify_channel_update (signer a u) u'))
  ]
  where
    update_vector i = do
      v <- must "update vector" (safe_index mainnet_updates i)
      p <- hex (uv_payload v)
      expectRight "decode_channel_update" (decode_channel_update p)

safe_head :: [a] -> Maybe a
safe_head (x : _) = Just x
safe_head []      = Nothing

safe_index :: [a] -> Int -> Maybe a
safe_index xs i
  | i < 0     = Nothing
  | otherwise = safe_head (drop i xs)

-- node_announcement ----------------------------------------------------------

-- a node_announcement payload carrying the given address bytes
node_wire :: Fx -> BS.ByteString -> BS.ByteString
node_wire fx addrs = mconcat
  [ BS.replicate 64 0xee              -- signature
  , u16be 0                           -- flen
  , BS.pack [0x49, 0x96, 0x02, 0xd2]  -- timestamp
  , BOLT1.un_point (fx_node_2 fx)
  , BS.pack [0xff, 0x00, 0x00]
  , BS.replicate 32 0
  , u16be (BS.length addrs)
  , addrs
  ]

-- address descriptors (port 9735)
desc_v4, desc_v6, desc_tor_v2, desc_tor_v3 :: BS.ByteString
desc_v4 = BS.pack [1, 127, 0, 0, 1, 0x26, 0x07]
desc_v6 = BS.pack (2 : replicate 15 0 ++ [1, 0x26, 0x07])
desc_tor_v2 = BS.cons 3 (BS.replicate 12 0xaa)
desc_tor_v3 = BS.cons 4 (BS.replicate 35 0xbb) <> u16be 9735

desc_dns :: BS.ByteString -> BS.ByteString
desc_dns h = BS.pack [5, fromIntegral (BS.length h)] <> h <> u16be 9735

-- decode the addresses of a node_announcement built by node_wire, and
-- check that it re-encodes exactly
addrs_of
  :: Fx -> BS.ByteString -> IO (Either DecodeError ([Address], BS.ByteString))
addrs_of fx bytes = do
  let wire = node_wire fx bytes
  case decode_node_announcement wire of
    Left e -> pure (Left e)
    Right n -> do
      encode_node_announcement n @?= Right wire
      pure (Right (na_addresses n, na_unknown_addresses n))

node_announcement_tests :: TestTree
node_announcement_tests = testGroup "node_announcement" [
    testCase "signed vector: fields, re-encoding, hash, verification" $ do
      let (h, dh) = node_announcement_vector
      p <- hex h
      d <- hex dh
      n <- expectRight "decode" (decode_node_announcement p)
      fs <- hex "08000000000002aaa2"
      nid <- hex
        "0324653eac434488002cc06bbfb7f10fe18991e35f9fe4302dbea6d2353dc0ab1c"
      v4 <- must "ipv4" (ipv4_addr "\127\0\0\1")
      v6 <- must "ipv6" (ipv6_addr (BS.replicate 15 0 <> "\1"))
      t2 <- must "tor v2" (tor_v2_addr (BS.replicate 12 0xaa))
      t3 <- must "tor v3" (tor_v3_addr (BS.replicate 35 0xbb))
      dns <- must "hostname" (hostname "ln.example.com")
      BOLT9.render (na_features n) @?= fs
      na_timestamp n @?= 1700000000
      BOLT1.un_point (na_node_id n) @?= nid
      na_rgb_color n @?= RgbColor 0x31 0x41 0x59
      BS.takeWhile (/= 0) (un_alias (na_alias n)) @?= "ppad-bolt7"
      na_addresses n @?=
        [ AddrIPv4 v4 9735, AddrIPv6 v6 9735, AddrTorV2 t2
        , AddrTorV3 t3 9735, AddrDNS dns 9735 ]
      na_unknown_addresses n @?= "\x06\x01\x02\x03"
      BOLT1.un_tlv_stream (na_tlvs n) @?= [TlvRecord 101 "ext"]
      encode_node_announcement n @?= Right p
      node_announcement_hash n @?= Right d
      assertBool "verifies" (verify_node_announcement n)
      assertBool "tampered"
        (not (verify_node_announcement n { na_timestamp = 1700000001 }))
      usable_addresses (na_addresses n) @?=
        [ AddrIPv4 v4 9735, AddrIPv6 v6 9735, AddrTorV3 t3 9735
        , AddrDNS dns 9735 ]
      validate_node_announcement n @?= Right ()
  , testCase "keeps deprecated Tor v2 descriptors" $ do
      fx <- fixtures
      r <- addrs_of fx (desc_v4 <> desc_tor_v2 <> desc_v6 <> desc_tor_v3)
      fmap (map kind . fst) r @?= Right [1, 3, 2, 4]
  , testCase "stops at the first unknown type, keeping the rest" $ do
      fx <- fixtures
      let rest = BS.pack [6, 1, 2, 3] <> desc_v6
      r <- addrs_of fx (desc_v4 <> rest)
      fmap (\(as, u) -> (map kind as, u)) r @?= Right ([1], rest)
  , testCase "stops at a type-0 descriptor" $ do
      fx <- fixtures
      let bytes = BS.singleton 0 <> desc_v4
      r <- addrs_of fx bytes
      fmap (\(as, u) -> (map kind as, u)) r @?= Right ([], bytes)
  , testCase "keeps descriptors with port 0" $ do
      fx <- fixtures
      v4 <- must "ipv4" (ipv4_addr "\127\0\0\1")
      r <- addrs_of fx (BS.pack [1, 127, 0, 0, 1, 0, 0])
      r @?= Right ([AddrIPv4 v4 0], BS.empty)
  , testCase "keeps empty and non-ASCII hostnames" $ do
      fx <- fixtures
      r <- addrs_of fx (desc_dns "" <> desc_dns "\xff\&ab")
      fmap (map (\a -> case a of
                    AddrDNS h _ -> un_hostname h
                    _           -> "?") . fst) r
        @?= Right ["", "\xff\&ab"]
  , testCase "rejects a truncated descriptor of a known type" $ do
      fx <- fixtures
      let truncated bytes = decode_node_announcement (node_wire fx bytes)
            @?= Left DecodeInsufficientBytes
      truncated (BS.pack [1, 127, 0, 0])
      truncated (BS.pack [2, 0, 0])
      truncated (BS.pack [3, 1, 2, 3])
      truncated (BS.cons 4 (BS.replicate 36 0))
      truncated (BS.pack [5, 4, 0x61])
  , testCase "rejects a node_id without a compressed prefix" $ do
      fx <- fixtures
      let wire = set_byte 70 0x04 (node_wire fx BS.empty)
      decode_node_announcement wire @?= Left DecodeInvalidPoint
  , testCase "na_unknown_addresses must not begin with a known type" $ do
      fx <- fixtures
      let with u = encode_node_announcement
            (node_fixture fx []) { na_unknown_addresses = u }
      mapM_ (\t -> with (BS.pack [t, 0]) @?= Left EncodeInvalidAddresses)
        [1 .. 5]
      assertBool "type 0" (either (const False) (const True)
        (with (BS.pack [0, 1])))
      assertBool "type 6" (either (const False) (const True)
        (with (BS.pack [6, 1])))
  , testCase "extension with an unknown even type is rejected" $ do
      fx <- fixtures
      decode_node_announcement (node_wire fx BS.empty <> "\x02\x00")
        @?= Left (DecodeTlvError (TlvUnknownEvenType 2))
  ]
  where
    kind :: Address -> Int
    kind a = case a of
      AddrIPv4 _ _  -> 1
      AddrIPv6 _ _  -> 2
      AddrTorV2 _   -> 3
      AddrTorV3 _ _ -> 4
      AddrDNS _ _   -> 5

-- channel_update -------------------------------------------------------------

channel_update_tests :: TestTree
channel_update_tests = testGroup "channel_update" [
    testCase "encoder writes the flags and htlc_maximum_msat" $ do
      u <- fixtures >>= update_fixture
      let wire = encode_channel_update u
      BS.length wire @?= 136
      BS.indexMaybe wire 108 @?= Just 0x01
      BS.indexMaybe wire 109 @?= Just 0x00
      BS.drop 128 wire @?= BS.pack [0, 0, 0, 0, 0x3b, 0x9a, 0xca, 0]
  , testCase "dont_forward" $ do
      u <- fixtures >>= update_fixture
      let wire = encode_channel_update
            u { cu_message_flags = message_flags DontForward }
      BS.indexMaybe wire 108 @?= Just 0x03
      d <- expectRight "decode" (decode_channel_update wire)
      message_flags_forwarding (cu_message_flags d) @?= DontForward
  , testCase "keeps a clear must_be_one bit" $ do
      u <- fixtures >>= update_fixture
      let wire = set_byte 108 0x00 (encode_channel_update u)
      d <- expectRight "decode" (decode_channel_update wire)
      d @?= u { cu_message_flags = MessageFlags 0 }
      encode_channel_update d @?= wire
  , testCase "keeps unassigned flag bits" $ do
      u <- fixtures >>= update_fixture
      let wire = set_byte 109 0xfd
                   (set_byte 108 0xfc (encode_channel_update u))
      d <- expectRight "decode" (decode_channel_update wire)
      cu_message_flags d @?= MessageFlags 0xfc
      cu_channel_flags d @?= ChannelFlags 0xfd
      channel_flags_direction (cu_channel_flags d) @?= NodeTwo
      channel_flags_status (cu_channel_flags d) @?= Enabled
      encode_channel_update d @?= wire
  , testCase "keeps extension records" $ do
      u <- fixtures >>= update_fixture
      ext <- tlvs_of [TlvRecord 55555 "\0\0\0\0\xff\xff\xff\xed"]
      let wire = encode_channel_update u { cu_tlvs = ext }
      BS.length wire @?= 148
      decode_channel_update wire @?= Right u { cu_tlvs = ext }
  , testCase "rejects a missing htlc_maximum_msat" $ do
      u <- fixtures >>= update_fixture
      decode_channel_update (BS.take 128 (encode_channel_update u))
        @?= Left DecodeInsufficientBytes
  , testCase "rejects amounts above 21M BTC" $ do
      u <- fixtures >>= update_fixture
      let wire = encode_channel_update u
          over = big_endian 8 0x1d24b2dfac520001
          at off = BS.take off wire <> over <> BS.drop (off + 8) wire
      decode_channel_update (at 112) @?= Left DecodeInvalidAmount
      decode_channel_update (at 128) @?= Left DecodeInvalidAmount
      max_msat <- msat 0x1d24b2dfac520000
      fmap cu_htlc_maximum_msat (decode_channel_update
        (BS.take 128 wire <> big_endian 8 0x1d24b2dfac520000))
        @?= Right max_msat
  , testCase "rejects a malformed extension" $ do
      u <- fixtures >>= update_fixture
      let wire = encode_channel_update u
      decode_channel_update (wire <> "\x02\x00")
        @?= Left (DecodeTlvError (TlvUnknownEvenType 2))
      decode_channel_update (wire <> "\x01\x05\xaa")
        @?= Left (DecodeTlvError TlvTruncated)
      decode_channel_update (wire <> "\xfd\x00\x01\x00")
        @?= Left (DecodeTlvError TlvNonMinimalBigSize)
  , testCase "rejects truncated input" $
      decode_channel_update (BS.replicate 50 0)
        @?= Left DecodeInsufficientBytes
  ]

-- queries --------------------------------------------------------------------

-- payload builders
qsci_wire :: Fx -> BS.ByteString -> BS.ByteString -> BS.ByteString
qsci_wire fx ids tlvs =
  BOLT1.un_chain_hash (fx_chain fx) <> u16be (BS.length ids) <> ids <> tlvs

qcr_wire :: Fx -> BS.ByteString -> BS.ByteString
qcr_wire fx tlvs = mconcat
  [ BOLT1.un_chain_hash (fx_chain fx)
  , big_endian 4 600000
  , big_endian 4 10000
  , tlvs
  ]

rcr_wire :: Fx -> Word8 -> BS.ByteString -> BS.ByteString -> BS.ByteString
rcr_wire fx sync ids tlvs = mconcat
  [ BOLT1.un_chain_hash (fx_chain fx)
  , big_endian 4 600000
  , big_endian 4 10000
  , BS.singleton sync
  , u16be (BS.length ids)
  , ids
  , tlvs
  ]

-- encoded_short_ids for some short channel ids
encoded_ids :: [ShortChannelId] -> BS.ByteString
encoded_ids ss = BS.cons 0 (foldMap BOLT1.encode_short_channel_id ss)

-- TLV stream rules for a query decoder, given its known types and the
-- unknown records it keeps
tlv_stream_tests
  :: String
  -> BS.ByteString
  -> (Fx -> BS.ByteString -> Either DecodeError [TlvRecord])
  -> TestTree
tlv_stream_tests name known dec = testGroup name [
    testCase "empty stream" $ do
      fx <- fixtures
      dec fx BS.empty @?= Right []
  , testCase "keeps unknown odd types" $ do
      fx <- fixtures
      dec fx (BS.pack [5, 1, 0x2a, 7, 0]) @?=
        Right [TlvRecord 5 "\x2a", TlvRecord 7 ""]
  , testCase "rejects unknown even types" $ do
      fx <- fixtures
      dec fx (BS.pack [2, 0]) @?= Left (DecodeTlvError (TlvUnknownEvenType 2))
  , testCase "rejects a length past the end" $ do
      fx <- fixtures
      dec fx (BS.pack [5, 5, 0xaa]) @?= Left (DecodeTlvError TlvTruncated)
  , testCase "rejects repeated types" $ do
      fx <- fixtures
      dec fx (BS.pack [5, 0, 5, 0])
        @?= Left (DecodeTlvError TlvNotStrictlyIncreasing)
  , testCase "rejects a non-minimal BigSize" $ do
      fx <- fixtures
      dec fx (BS.pack [0xfd, 0x00, 0x05, 0x00])
        @?= Left (DecodeTlvError TlvNonMinimalBigSize)
  , testCase "known records are typed, not kept as unknown" $ do
      fx <- fixtures
      dec fx known @?= Right []
      dec fx (known <> BS.pack [5, 0]) @?= Right [TlvRecord 5 ""]
  ]

query_tests :: TestTree
query_tests = testGroup "queries" [
    tlv_stream_tests "query_short_channel_ids TLVs" (BS.pack [1, 2, 0, 0])
      $ \fx tlvs ->
      fmap (BOLT1.un_tlv_stream . qsci_tlvs)
        (decode_query_short_channel_ids
          (qsci_wire fx (encoded_ids [fx_scid fx]) tlvs))
  , tlv_stream_tests "query_channel_range TLVs" (BS.pack [1, 1, 3])
      $ \fx tlvs ->
      fmap (BOLT1.un_tlv_stream . qcr_tlvs)
        (decode_query_channel_range (qcr_wire fx tlvs))
  , tlv_stream_tests "reply_channel_range TLVs"
      (BS.pack ([1, 9] ++ replicate 9 0 ++ [3, 8] ++ replicate 8 0))
      $ \fx tlvs ->
      fmap (BOLT1.un_tlv_stream . rcr_tlvs)
        (decode_reply_channel_range
          (rcr_wire fx 1 (encoded_ids [fx_scid fx]) tlvs))
  , testGroup "query_short_channel_ids" [
      testCase "query_flags" $ do
        fx <- fixtures
        let ss = take 2 (fx_scids fx)
            wire = qsci_wire fx (encoded_ids ss)
              (BS.pack [1, 3, 0, 0x01, 0x1f])
            m = QueryShortChannelIds (fx_chain fx) ss
                  (Just [QueryFlags 1, QueryFlags 31]) BOLT1.empty_tlv_stream
        decode_query_short_channel_ids wire @?= Right m
        encode_query_short_channel_ids m @?= Right wire
    , testCase "query_flags beyond one byte" $ do
        fx <- fixtures
        let ss = take 1 (fx_scids fx)
            wire = qsci_wire fx (encoded_ids ss)
                     (BS.pack [1, 4, 0, 0xfd, 0x01, 0x00])
        fmap qsci_query_flags (decode_query_short_channel_ids wire)
          @?= Right (Just [QueryFlags 256])
    , testCase "rejects a query flag count mismatch" $ do
        fx <- fixtures
        let ss = take 2 (fx_scids fx)
            wire = qsci_wire fx (encoded_ids ss) (BS.pack [1, 2, 0, 0x01])
        decode_query_short_channel_ids wire @?= Left DecodeCountMismatch
    , testCase "rejects malformed query_flags" $ do
        fx <- fixtures
        let with tlv = decode_query_short_channel_ids
              (qsci_wire fx (encoded_ids [fx_scid fx]) tlv)
        with (BS.pack [1, 0]) @?= Left (DecodeInvalidTlvValue 1)
        with (BS.pack [1, 4, 0, 0xfd, 0x00, 0x01])
          @?= Left (DecodeInvalidTlvValue 1)
        with (BS.pack [1, 2, 0, 0xfd]) @?= Left (DecodeInvalidTlvValue 1)
        with (BS.pack [1, 2, 1, 0x01]) @?= Left (DecodeUnknownEncoding 1)
    , testCase "rejects malformed encoded_short_ids" $ do
        fx <- fixtures
        let with ids = decode_query_short_channel_ids (qsci_wire fx ids "")
        with BS.empty @?= Left DecodeInvalidShortChannelIds
        with (BS.cons 0 (BS.replicate 7 0))
          @?= Left DecodeInvalidShortChannelIds
        with (BS.cons 0 (BS.replicate 9 0))
          @?= Left DecodeInvalidShortChannelIds
        with (BS.cons 1 (BS.replicate 8 0)) @?= Left (DecodeUnknownEncoding 1)
        with (BS.cons 2 (BS.replicate 8 0)) @?= Left (DecodeUnknownEncoding 2)
    , testCase "an empty list still carries its encoding type" $ do
        fx <- fixtures
        let m = QueryShortChannelIds (fx_chain fx) [] Nothing
                  BOLT1.empty_tlv_stream
            wire = qsci_wire fx (BS.singleton 0) ""
        encode_query_short_channel_ids m @?= Right wire
        decode_query_short_channel_ids wire @?= Right m
    ]
  , testGroup "query_channel_range" [
      testCase "query_option" $ do
        fx <- fixtures
        let wire = qcr_wire fx (BS.pack [1, 3, 0xfd, 0x01, 0x00])
        fmap qcr_query_option (decode_query_channel_range wire)
          @?= Right (Just (QueryOption 256))
    , testCase "rejects malformed query_option" $ do
        fx <- fixtures
        let with tlv = decode_query_channel_range (qcr_wire fx tlv)
        with (BS.pack [1, 0]) @?= Left (DecodeInvalidTlvValue 1)
        with (BS.pack [1, 2, 3, 0]) @?= Left (DecodeInvalidTlvValue 1)
        with (BS.pack [1, 3, 0xfd, 0x00, 0x03])
          @?= Left (DecodeInvalidTlvValue 1)
    , testCase "rejects truncated input" $ do
        fx <- fixtures
        decode_query_channel_range (BS.take 39 (qcr_wire fx ""))
          @?= Left DecodeInsufficientBytes
    ]
  , testGroup "reply_channel_range" [
      testCase "rejects malformed timestamps" $ do
        fx <- fixtures
        let ss = take 2 (fx_scids fx)
            with tlv = decode_reply_channel_range
              (rcr_wire fx 1 (encoded_ids ss) tlv)
        with (BS.pack [1, 0]) @?= Left (DecodeInvalidTlvValue 1)
        with (BS.pack [1, 9, 1] <> BS.replicate 8 0)
          @?= Left (DecodeUnknownEncoding 1)
        with (BS.pack [1, 9, 0] <> BS.replicate 8 0)
          @?= Left DecodeCountMismatch
        with (BS.pack [1, 13, 0] <> BS.replicate 12 0)
          @?= Left (DecodeInvalidTlvValue 1)
    , testCase "rejects malformed checksums" $ do
        fx <- fixtures
        let ss = take 2 (fx_scids fx)
            with tlv = decode_reply_channel_range
              (rcr_wire fx 1 (encoded_ids ss) tlv)
        with (BS.pack [3, 8] <> BS.replicate 8 0) @?= Left DecodeCountMismatch
        with (BS.pack [3, 12] <> BS.replicate 12 0)
          @?= Left (DecodeInvalidTlvValue 3)
        fmap rcr_checksums (with (BS.pack [3, 16] <> BS.replicate 16 0))
          @?= Right (Just (replicate 2 (ChannelUpdateChecksums 0 0)))
    , testCase "rejects a sync_complete other than 0 or 1" $ do
        fx <- fixtures
        let with s = fmap rcr_sync_complete (decode_reply_channel_range
              (rcr_wire fx s (encoded_ids []) ""))
        with 0 @?= Right False
        with 1 @?= Right True
        with 2 @?= Left DecodeInvalidBool
    , testCase "rejects malformed encoded_short_ids" $ do
        fx <- fixtures
        let with ids = decode_reply_channel_range (rcr_wire fx 1 ids "")
        with BS.empty @?= Left DecodeInvalidShortChannelIds
        with (BS.cons 0 (BS.replicate 12 0))
          @?= Left DecodeInvalidShortChannelIds
        with (BS.cons 1 (BS.replicate 8 0)) @?= Left (DecodeUnknownEncoding 1)
    ]
  ]

-- other messages -------------------------------------------------------------

other_message_tests :: TestTree
other_message_tests = testGroup "other messages" [
    testCase "reply_short_channel_ids_end" $ do
      fx <- fixtures
      let ch = BOLT1.un_chain_hash (fx_chain fx)
      ext <- tlvs_of [TlvRecord 9 "x"]
      decode_reply_short_channel_ids_end (ch <> "\x01")
        @?= Right (ReplyShortChannelIdsEnd (fx_chain fx) True
                     BOLT1.empty_tlv_stream)
      decode_reply_short_channel_ids_end (ch <> "\x00\x09\x01x")
        @?= Right (ReplyShortChannelIdsEnd (fx_chain fx) False ext)
      decode_reply_short_channel_ids_end (ch <> "\x02")
        @?= Left DecodeInvalidBool
      decode_reply_short_channel_ids_end ch @?= Left DecodeInsufficientBytes
  , testCase "announcement_signatures" $ do
      fx <- fixtures
      ext <- tlvs_of [TlvRecord 1 "a"]
      let m = AnnouncementSignatures (fx_cid fx) (fx_scid fx) (fx_sig fx)
                (fx_sig fx) ext
          wire = encode_announcement_signatures m
      BS.length wire @?= 32 + 8 + 64 + 64 + 3
      decode_announcement_signatures wire @?= Right m
      decode_announcement_signatures (BS.take 167 wire)
        @?= Left DecodeInsufficientBytes
      -- type 1 is odd, so it may appear in an extension; 2 may not
      decode_announcement_signatures (BS.take 168 wire <> "\x02\x00")
        @?= Left (DecodeTlvError (TlvUnknownEvenType 2))
  , testCase "gossip_timestamp_filter" $ do
      fx <- fixtures
      let m = GossipTimestampFilter (fx_chain fx) 1609459200 86400
                BOLT1.empty_tlv_stream
          wire = encode_gossip_timestamp_filter m
      BS.drop 32 wire @?= BS.pack [0x5f, 0xee, 0x66, 0x00, 0, 1, 0x51, 0x80]
      decode_gossip_timestamp_filter wire @?= Right m
      decode_gossip_timestamp_filter (BS.take 39 wire)
        @?= Left DecodeInsufficientBytes
  , testCase "channel_announcement rejects bad points" $ do
      fx <- fixtures
      wire <- expectRight "encode"
        (encode_channel_announcement (announcement_fixture fx))
      -- node_id_1 starts after the signatures, features, chain hash
      -- and short channel id
      decode_channel_announcement (set_byte 298 0x04 wire)
        @?= Left DecodeInvalidPoint
      decode_channel_announcement (BS.take 429 wire)
        @?= Left DecodeInsufficientBytes
  ]

-- encoding errors ------------------------------------------------------------

encode_error_tests :: TestTree
encode_error_tests = testGroup "encoding errors" [
    testCase "channel_announcement features" $ do
      fx <- fixtures
      let feats n = (announcement_fixture fx)
            { ca_features = BOLT9.parse (BS.replicate n 1) }
      encode_channel_announcement (feats 65536) @?= Left EncodeLengthOverflow
      wire <- expectRight "encode" (encode_channel_announcement (feats 65535))
      decode_channel_announcement wire @?= Right (feats 65535)
  , testCase "node_announcement features and addresses" $ do
      fx <- fixtures
      v4 <- must "ipv4" (ipv4_addr "\127\0\0\1")
      let base = node_fixture fx []
      encode_node_announcement base
        { na_features = BOLT9.parse (BS.replicate 65536 1) }
        @?= Left EncodeLengthOverflow
      -- 10923 IPv4 descriptors take 65538 bytes
      let v4s n = node_fixture fx (replicate n (AddrIPv4 v4 1))
      encode_node_announcement (v4s 10923) @?= Left EncodeLengthOverflow
      wire <- expectRight "encode" (encode_node_announcement (v4s 9362))
      fmap (length . na_addresses) (decode_node_announcement wire)
        @?= Right 9362
  , testCase "short channel id lists" $ do
      fx <- fixtures
      let ids n = map ShortChannelId [1 .. n]
          q n = QueryShortChannelIds (fx_chain fx) (ids n) Nothing
                  BOLT1.empty_tlv_stream
          r n = ReplyChannelRange (fx_chain fx) 0 1 True (ids n) Nothing
                  Nothing BOLT1.empty_tlv_stream
      encode_query_short_channel_ids (q 8192) @?= Left EncodeLengthOverflow
      encode_reply_channel_range (r 8192) @?= Left EncodeLengthOverflow
      wire <- expectRight "encode" (encode_query_short_channel_ids (q 8191))
      decode_query_short_channel_ids wire @?= Right (q 8191)
  , testCase "count mismatches" $ do
      fx <- fixtures
      let ss = fx_scids fx
          q = QueryShortChannelIds (fx_chain fx) ss (Just [QueryFlags 1])
                BOLT1.empty_tlv_stream
          r = ReplyChannelRange (fx_chain fx) 0 1 True ss Nothing Nothing
                BOLT1.empty_tlv_stream
      encode_query_short_channel_ids q @?= Left EncodeCountMismatch
      encode_reply_channel_range r
        { rcr_timestamps = Just [ChannelUpdateTimestamps 1 2] }
        @?= Left EncodeCountMismatch
      encode_reply_channel_range r
        { rcr_checksums = Just (replicate 4 (ChannelUpdateChecksums 1 2)) }
        @?= Left EncodeCountMismatch
  , testCase "extra records must not use known types" $ do
      fx <- fixtures
      t1 <- tlvs_of [TlvRecord 1 "\0"]
      t3 <- tlvs_of [TlvRecord 3 ""]
      t5 <- tlvs_of [TlvRecord 5 ""]
      let q = QueryShortChannelIds (fx_chain fx) [] Nothing
          c = QueryChannelRange (fx_chain fx) 0 1 Nothing
          r = ReplyChannelRange (fx_chain fx) 0 1 True [] Nothing Nothing
      encode_query_short_channel_ids (q t1) @?= Left EncodeInvalidTlvs
      encode_query_channel_range (c t1) @?= Left EncodeInvalidTlvs
      encode_reply_channel_range (r t1) @?= Left EncodeInvalidTlvs
      encode_reply_channel_range (r t3) @?= Left EncodeInvalidTlvs
      wire <- expectRight "encode" (encode_reply_channel_range (r t5))
      decode_reply_channel_range wire @?= Right (r t5)
  ]

-- dispatch -------------------------------------------------------------------

dispatch_tests :: TestTree
dispatch_tests = testGroup "dispatch" [
    testCase "unknown message types" $
      mapM_ (\t -> decode_message t "" @?= Left (DecodeUnknownType t))
        [0, 16, 255, 260, 266, 32768]
  , testCase "message types" $ do
      fx <- fixtures
      u <- update_fixture fx
      let e = BOLT1.empty_tlv_stream
          ms = [ MsgChannelAnnouncement (announcement_fixture fx)
               , MsgNodeAnnouncement (node_fixture fx [])
               , MsgChannelUpdate u
               , MsgAnnouncementSignatures (AnnouncementSignatures
                   (fx_cid fx) (fx_scid fx) (fx_sig fx) (fx_sig fx) e)
               , MsgQueryShortChannelIds
                   (QueryShortChannelIds (fx_chain fx) [] Nothing e)
               , MsgReplyShortChannelIdsEnd
                   (ReplyShortChannelIdsEnd (fx_chain fx) True e)
               , MsgQueryChannelRange
                   (QueryChannelRange (fx_chain fx) 0 1 Nothing e)
               , MsgReplyChannelRange (ReplyChannelRange
                   (fx_chain fx) 0 1 True [] Nothing Nothing e)
               , MsgGossipTimestampFilter
                   (GossipTimestampFilter (fx_chain fx) 0 1 e)
               ]
      map message_type ms @?= [256, 257, 258, 259, 261, 262, 263, 264, 265]
      mapM_ (\m -> do
          p <- expectRight "encode" (encode_message m)
          decode_message (message_type m) p @?= Right m)
        ms
  ]

-- validation -----------------------------------------------------------------

validation_tests :: TestTree
validation_tests = testGroup "validation" [
    testCase "channel_announcement node id order" $ do
      fx <- fixtures
      let a = announcement_fixture fx
      validate_channel_announcement a @?= Right ()
      validate_channel_announcement
        a { ca_node_id_1 = fx_node_2 fx, ca_node_id_2 = fx_node_1 fx }
        @?= Left ValidateNodeIdOrdering
      validate_channel_announcement a { ca_node_id_2 = fx_node_1 fx }
        @?= Left ValidateNodeIdOrdering
  , testCase "node_announcement DNS hostnames" $ do
      fx <- fixtures
      v4 <- must "ipv4" (ipv4_addr "\127\0\0\1")
      h1 <- must "hostname" (hostname "a.example.com")
      h2 <- must "hostname" (hostname "b.example.com")
      validate_node_announcement
        (node_fixture fx [AddrIPv4 v4 1, AddrDNS h1 1])
        @?= Right ()
      validate_node_announcement (node_fixture fx [AddrDNS h1 1, AddrDNS h2 1])
        @?= Left ValidateMultipleDns
  , testCase "channel_update htlc amounts" $ do
      fx <- fixtures
      u <- update_fixture fx
      big <- msat 2000000000
      validate_channel_update u @?= Right ()
      validate_channel_update
        u { cu_htlc_minimum_msat = cu_htlc_maximum_msat u } @?= Right ()
      validate_channel_update u { cu_htlc_minimum_msat = big }
        @?= Left ValidateHtlcAmounts
  , testCase "query_channel_range block range" $ do
      fx <- fixtures
      let q f n = validate_query_channel_range
            (QueryChannelRange (fx_chain fx) f n Nothing
              BOLT1.empty_tlv_stream)
      q 600000 10000 @?= Right ()
      q 600000 0 @?= Left ValidateZeroBlocks
      q maxBound 1 @?= Right ()
      q maxBound 2 @?= Left ValidateBlockOverflow
      q 0 maxBound @?= Right ()
      q 1 maxBound @?= Right ()
      q 2 maxBound @?= Left ValidateBlockOverflow
  , testCase "short channel ids ascend" $ do
      fx <- fixtures
      let ss = fx_scids fx
          e = BOLT1.empty_tlv_stream
          r xs = validate_reply_channel_range
            (ReplyChannelRange (fx_chain fx) 0 1 True xs Nothing Nothing e)
          q xs = validate_query_short_channel_ids
            (QueryShortChannelIds (fx_chain fx) xs Nothing e)
      r ss @?= Right ()
      r [] @?= Right ()
      r (reverse ss) @?= Left ValidateScidNotAscending
      r (take 1 ss <> take 1 ss) @?= Left ValidateScidNotAscending
      q ss @?= Right ()
      q (reverse ss) @?= Left ValidateScidNotAscending
  ]

-- checksums ------------------------------------------------------------------

-- an independent, table-driven CRC-32C
crc32c_ref :: BS.ByteString -> Word32
crc32c_ref = xor 0xffffffff . BS.foldl' step 0xffffffff
  where
    step c b = (c `shiftR` 8)
      `xor` table (fromIntegral ((c `xor` fromIntegral b) .&. 0xff))
    table :: Word32 -> Word32
    table n = go (8 :: Int) n
      where
        go 0 x = x
        go k x
          | x .&. 1 == 1 = go (k - 1) ((x `shiftR` 1) `xor` 0x82f63b78)
          | otherwise    = go (k - 1) (x `shiftR` 1)

-- the checksum per BOLT #7: the update without signature and timestamp
checksum_ref :: BS.ByteString -> Word32
checksum_ref wire =
  crc32c_ref (BS.take 40 (BS.drop 64 wire) <> BS.drop 108 wire)

checksum_tests :: TestTree
checksum_tests = testGroup "checksums" [
    testCase "reference CRC-32C matches RFC 3720 vectors" $ do
      crc32c_ref "123456789" @?= 0xe3069283
      crc32c_ref (BS.replicate 32 0x00) @?= 0x8a9136aa
      crc32c_ref (BS.replicate 32 0xff) @?= 0x62a8ab43
      crc32c_ref (BS.pack [0 .. 31]) @?= 0x46dd794e
      crc32c_ref (BS.pack [31, 30 .. 0]) @?= 0x113fdb5c
  , testCase "checksum ignores signature and timestamp" $ do
      fx <- fixtures
      u <- update_fixture fx
      sig <- must "signature" (BOLT1.signature (BS.replicate 64 0x11))
      channel_update_checksum u { cu_signature = sig, cu_timestamp = 1 }
        @?= channel_update_checksum u
      assertBool "covers the fees"
        (channel_update_checksum u { cu_fee_base_msat = 1001 }
          /= channel_update_checksum u)
  , testProperty "checksum matches the reference" $
      forAll gen_channel_update $ \u ->
        channel_update_checksum u === checksum_ref (encode_channel_update u)
  ]

-- properties -----------------------------------------------------------------

property_tests :: TestTree
property_tests = testGroup "properties" [
    testProperty "real gossip re-encodes exactly" $
      forAll (elements samples) $ \(t, h) ->
        case B16.decode h of
          Nothing -> counterexample "bad hex" False
          Just p  -> fmap encode_message (decode_message t p)
                       === Right (Right p)
  , testProperty "mutated real gossip re-encodes exactly, if it decodes" $
      forAll (elements samples) $ \(t, h) ->
      forAll arbitrary $ \(NonNegative i, w) ->
        case B16.decode h of
          Nothing -> counterexample "bad hex" False
          Just p  ->
            let p' = set_byte (i `mod` BS.length p) w p
            in  case decode_message t p' of
                  Left _  -> property True
                  Right m -> encode_message m === Right p'
  , testProperty "messages round-trip" $
      forAll gen_message $ \m -> case encode_message m of
        Left e  -> counterexample (show e) False
        Right p -> decode_message (message_type m) p === Right m
  , testProperty "channel_flags accessors" $ \w ->
      let f = ChannelFlags w
      in  channel_flags (channel_flags_direction f) (channel_flags_status f)
            === ChannelFlags (w .&. 0x03)
  , testProperty "message_flags accessors" $ \w ->
      let f = MessageFlags w
      in  message_flags (message_flags_forwarding f)
            === MessageFlags (w .&. 0x02 .|. 0x01)
  , testProperty "query_flags sets exactly its bits" $
      forAll (sublistOf [minBound .. maxBound]) $ \fs ->
        map (`has_query_flag` query_flags fs) [minBound .. maxBound]
          === map (`elem` fs) [minBound .. maxBound]
  , testProperty "query_option sets exactly its bits" $
      forAll (sublistOf [minBound .. maxBound]) $ \fs ->
        map (`has_query_option` query_option fs) [minBound .. maxBound]
          === map (`elem` fs) [minBound .. maxBound]
  ]

-- (type, hex payload) of every embedded real or signed message
samples :: [(Word16, BS.ByteString)]
samples =
     [ (256, av_payload v) | v <- mainnet_announcements ]
  <> [ (258, uv_payload v) | v <- mainnet_updates ]
  <> [ (257, fst node_announcement_vector) ]

-- generators -----------------------------------------------------------------

gen_bytes :: Int -> Gen BS.ByteString
gen_bytes n = BS.pack <$> vectorOf n arbitrary

gen_var_bytes :: Int -> Gen BS.ByteString
gen_var_bytes m = choose (0, m) >>= gen_bytes

gen_chain_hash :: Gen ChainHash
gen_chain_hash = gen_bytes 32 `suchThatMap` BOLT1.chain_hash

gen_signature :: Gen Signature
gen_signature = gen_bytes 64 `suchThatMap` BOLT1.signature

gen_point :: Gen Point
gen_point = do
  h <- elements [0x02, 0x03]
  b <- gen_bytes 32
  pure (BS.cons h b) `suchThatMap` BOLT1.point

gen_scid :: Gen ShortChannelId
gen_scid = ShortChannelId <$> arbitrary

gen_msat :: Gen BOLT1.MilliSatoshi
gen_msat = choose (0, 0x1d24b2dfac520000) `suchThatMap` BOLT1.milli_satoshi

gen_features :: Gen BOLT9.FeatureVector
gen_features = BOLT9.parse <$> gen_var_bytes 8

-- TLV records of distinct odd types at or above the given minimum
gen_tlvs :: Word64 -> Gen TlvStream
gen_tlvs lo = do
  n <- choose (0, 3)
  ts <- vectorOf n (choose (lo `div` 2, 40000))
  rs <- traverse (\t -> TlvRecord (2 * t + 1) <$> gen_var_bytes 10) ts
  pure rs `suchThatMap` BOLT1.tlv_stream

gen_address :: Gen Address
gen_address = oneof
  [ AddrIPv4 <$> (gen_bytes 4 `suchThatMap` ipv4_addr) <*> arbitrary
  , AddrIPv6 <$> (gen_bytes 16 `suchThatMap` ipv6_addr) <*> arbitrary
  , AddrTorV2 <$> (gen_bytes 12 `suchThatMap` tor_v2_addr)
  , AddrTorV3 <$> (gen_bytes 35 `suchThatMap` tor_v3_addr) <*> arbitrary
  , AddrDNS <$> (gen_var_bytes 40 `suchThatMap` hostname) <*> arbitrary
  ]

gen_unknown_addresses :: Gen BS.ByteString
gen_unknown_addresses = oneof
  [ pure BS.empty
  , BS.cons <$> elements (0 : [6 .. 255]) <*> gen_var_bytes 10
  ]

gen_channel_announcement :: Gen ChannelAnnouncement
gen_channel_announcement = ChannelAnnouncement
  <$> gen_signature <*> gen_signature <*> gen_signature <*> gen_signature
  <*> gen_features <*> gen_chain_hash <*> gen_scid
  <*> gen_point <*> gen_point <*> gen_point <*> gen_point
  <*> gen_tlvs 0

gen_node_announcement :: Gen NodeAnnouncement
gen_node_announcement = NodeAnnouncement
  <$> gen_signature <*> gen_features <*> arbitrary <*> gen_point
  <*> (RgbColor <$> arbitrary <*> arbitrary <*> arbitrary)
  <*> (gen_bytes 32 `suchThatMap` alias)
  <*> listOf gen_address <*> gen_unknown_addresses <*> gen_tlvs 0

gen_channel_update :: Gen ChannelUpdate
gen_channel_update = ChannelUpdate
  <$> gen_signature <*> gen_chain_hash <*> gen_scid <*> arbitrary
  <*> (MessageFlags <$> arbitrary) <*> (ChannelFlags <$> arbitrary)
  <*> arbitrary <*> gen_msat <*> arbitrary <*> arbitrary <*> gen_msat
  <*> gen_tlvs 0

-- a list of short channel ids and, maybe, one entry for each
gen_scids_with :: Gen a -> Gen ([ShortChannelId], Maybe [a])
gen_scids_with g = do
  ss <- listOf gen_scid
  xs <- oneof [pure Nothing, Just <$> vectorOf (length ss) g]
  pure (ss, xs)

gen_message :: Gen Message
gen_message = oneof
  [ MsgChannelAnnouncement <$> gen_channel_announcement
  , MsgNodeAnnouncement <$> gen_node_announcement
  , MsgChannelUpdate <$> gen_channel_update
  , MsgAnnouncementSignatures <$> (AnnouncementSignatures
      <$> (gen_bytes 32 `suchThatMap` BOLT1.channel_id) <*> gen_scid
      <*> gen_signature <*> gen_signature <*> gen_tlvs 0)
  , MsgQueryShortChannelIds <$> do
      (ss, fs) <- gen_scids_with (QueryFlags <$> arbitrary)
      QueryShortChannelIds <$> gen_chain_hash <*> pure ss <*> pure fs
        <*> gen_tlvs 3
  , MsgReplyShortChannelIdsEnd <$> (ReplyShortChannelIdsEnd
      <$> gen_chain_hash <*> arbitrary <*> gen_tlvs 0)
  , MsgQueryChannelRange <$> (QueryChannelRange
      <$> gen_chain_hash <*> arbitrary <*> arbitrary
      <*> oneof [pure Nothing, Just . QueryOption <$> arbitrary]
      <*> gen_tlvs 3)
  , MsgReplyChannelRange <$> do
      (ss, ts) <- gen_scids_with
        (ChannelUpdateTimestamps <$> arbitrary <*> arbitrary)
      cs <- oneof [pure Nothing, Just <$> vectorOf (length ss)
        (ChannelUpdateChecksums <$> arbitrary <*> arbitrary)]
      ReplyChannelRange <$> gen_chain_hash <*> arbitrary <*> arbitrary
        <*> arbitrary <*> pure ss <*> pure ts <*> pure cs <*> gen_tlvs 5
  , MsgGossipTimestampFilter <$> (GossipTimestampFilter
      <$> gen_chain_hash <*> arbitrary <*> arbitrary <*> gen_tlvs 0)
  ]
