{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Fixtures
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Benchmark inputs: real mainnet gossip, a signed node_announcement, and
-- large gossip query replies.

module Fixtures (
    Fixtures(..)
  , fixtures
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT7

data Fixtures = Fixtures
  { f_ann_bytes  :: !BS.ByteString
  , f_ann        :: !ChannelAnnouncement
  , f_upd_bytes  :: !BS.ByteString
  , f_upd        :: !ChannelUpdate
  , f_upd_key    :: !BOLT1.Point
  , f_node_bytes :: !BS.ByteString
  , f_node       :: !NodeAnnouncement
  , f_qsci_bytes :: !BS.ByteString
  , f_qsci       :: !QueryShortChannelIds
  , f_rcr_bytes  :: !BS.ByteString
  , f_rcr        :: !ReplyChannelRange
  }

-- | The fixtures, or 'Nothing' if one fails to build.
fixtures :: Maybe Fixtures
fixtures = do
  ann_bytes <- B16.decode ann_hex
  ann <- right (decode_channel_announcement ann_bytes)
  upd_bytes <- B16.decode upd_hex
  upd <- right (decode_channel_update upd_bytes)
  node_bytes <- B16.decode node_hex
  node <- right (decode_node_announcement node_bytes)
  ch <- BOLT1.chain_hash (BS.replicate 32 0x6f)
  let ids = [ BOLT1.ShortChannelId (0x0cf88400000000 + 0x10000 * i)
            | i <- [1 .. 1000] ]
      qsci = QueryShortChannelIds ch ids
        (Just (map (\i -> QueryFlags (i `mod` 32)) [1 .. 1000]))
        BOLT1.empty_tlv_stream
      rcr = ReplyChannelRange ch 850000 10000 True ids
        (Just [ ChannelUpdateTimestamps (1776493010 + i) (1776493020 + i)
              | i <- [1 .. 1000] ])
        (Just [ ChannelUpdateChecksums (0x1da953bd + i) (0xf0a89fda + i)
              | i <- [1 .. 1000] ])
        BOLT1.empty_tlv_stream
  qsci_bytes <- right (encode_query_short_channel_ids qsci)
  rcr_bytes <- right (encode_reply_channel_range rcr)
  pure Fixtures
    { f_ann_bytes  = ann_bytes
    , f_ann        = ann
    , f_upd_bytes  = upd_bytes
    , f_upd        = upd
    , f_upd_key    = ca_node_id_1 ann
    , f_node_bytes = node_bytes
    , f_node       = node
    , f_qsci_bytes = qsci_bytes
    , f_qsci       = qsci
    , f_rcr_bytes  = rcr_bytes
    , f_rcr        = rcr
    }
  where
    right :: Either e a -> Maybe a
    right = either (const Nothing) Just

-- channel_announcement for 850052x2992x1
ann_hex :: BS.ByteString
ann_hex = mconcat
  [ "3c256c270126d2c52ab5464fa959d627e9baec8c76bd2b81d34b47c4efb03290"
  , "40126ea7a0f8d31420976dc28ff684dc63f62620fa10dbac4fe3d417ca5690c1"
  , "b7fe1de61f5dd2b8368886d6c98f625c9110198e57c14fac93d4eff34e19ce5f"
  , "5dc6fef4501e8105c8f9b82d6d0b348f5840864445b071ae7a741b5eb92d2ec0"
  , "ab85681e4e1e85144bbbd8757c14cee92468d2c5ac84345cb28931b6fed5e339"
  , "00ed9dd363db630f31248cfa933a3667cbf87bb78a48a442e81c652686355365"
  , "ff33164c7588f0a753d50c4d52dd4a81c611fa47b4e950dfc9e3379a12ef7834"
  , "781c0900b90f56a5a49893446d03c8d283c3c093e3c8c682ff6cb40d70d9eebc"
  , "00006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d619000000"
  , "00000cf884000bb0000102bb5e47ee26aca1654ce092d9d06eac15d953210c73"
  , "7f2220e2d7e54c4f108677030b828fd1e42549fbc2f1d0960ec3663362c41777"
  , "d3d87fe1b8e1d4bf063abcaa02bc3f260955062a4f427704edef1864c76d6777"
  , "fe81bed43a5f6297dda1c7ca9203523e164f4f9fbdea978c6570d56b13ca8338"
  , "430e6794ff74cf6aa0dbda71dd2d"
  ]

-- a channel_update for it from node_id_1, with an inbound-fee extension
upd_hex :: BS.ByteString
upd_hex = mconcat
  [ "1c3f0859cb3f4494919c3dcc80d22bbf1e25e378bd95f071530833c2371c33d7"
  , "0766012ebef1cd79b299a54d0d29bea2ef6feca1d6cfa188864c95d7eea7e9a4"
  , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
  , "0cf884000bb000016a0a1c760100006400000000000003e80000000000001388"
  , "00000000b1069a80fdd9030800000000ffffffed"
  ]

-- a node_announcement with one address descriptor of each type
node_hex :: BS.ByteString
node_hex = mconcat
  [ "edbd2df19aee2f072d9665e2d9a2807fbcd4be6876f6863955ea11870e411e66"
  , "063a31dcef14f1983524f87e108ee556b3a6646c873cd4bbcc07512c8f5e8f6a"
  , "000908000000000002aaa26553f1000324653eac434488002cc06bbfb7f10fe1"
  , "8991e35f9fe4302dbea6d2353dc0ab1c314159707061642d626f6c7437000000"
  , "000000000000000000000000000000000000000063017f000001260702000000"
  , "00000000000000000000000001260703aaaaaaaaaaaaaaaaaaaaaaaa04bbbbbb"
  , "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  , "2607050e6c6e2e6578616d706c652e636f6d2607060102036503657874"
  ]
