-- |
-- Module: Main
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Criterion timing benchmarks for ppad-bolt7.

module Main where

import Criterion.Main
import Fixtures
import Lightning.Protocol.BOLT7
import System.Exit (exitFailure)

main :: IO ()
main = case fixtures of
  Nothing -> putStrLn "fixtures failed to build" >> exitFailure
  Just f  -> defaultMain [
      bgroup "decode" [
        bench "channel_announcement" $
          nf decode_channel_announcement (f_ann_bytes f)
      , bench "node_announcement" $
          nf decode_node_announcement (f_node_bytes f)
      , bench "channel_update" $ nf decode_channel_update (f_upd_bytes f)
      , bench "channel_update (decode_message)" $
          nf (decode_message 258) (f_upd_bytes f)
      , bench "query_short_channel_ids (1000 ids, flags)" $
          nf decode_query_short_channel_ids (f_qsci_bytes f)
      , bench "reply_channel_range (1000 ids, timestamps, checksums)" $
          nf decode_reply_channel_range (f_rcr_bytes f)
      ]
    , bgroup "encode" [
        bench "channel_announcement" $
          nf encode_channel_announcement (f_ann f)
      , bench "node_announcement" $ nf encode_node_announcement (f_node f)
      , bench "channel_update" $ nf encode_channel_update (f_upd f)
      , bench "query_short_channel_ids (1000 ids, flags)" $
          nf encode_query_short_channel_ids (f_qsci f)
      , bench "reply_channel_range (1000 ids, timestamps, checksums)" $
          nf encode_reply_channel_range (f_rcr f)
      ]
    , bgroup "hash" [
        bench "channel_announcement_hash" $
          nf channel_announcement_hash (f_ann f)
      , bench "channel_update_hash" $ nf channel_update_hash (f_upd f)
      , bench "channel_update_checksum" $
          nf channel_update_checksum (f_upd f)
      ]
    , bgroup "verify" [
        bench "verify_channel_announcement" $
          nf verify_channel_announcement (f_ann f)
      , bench "verify_node_announcement" $
          nf verify_node_announcement (f_node f)
      , bench "verify_channel_update" $
          nf (verify_channel_update (f_upd_key f)) (f_upd f)
      ]
    , bgroup "validate" [
        bench "validate_reply_channel_range (1000 ids)" $
          nf validate_reply_channel_range (f_rcr f)
      ]
    ]
