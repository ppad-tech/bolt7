-- |
-- Module: Main
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Weigh allocation benchmarks for ppad-bolt7.

module Main where

import Fixtures
import Lightning.Protocol.BOLT7
import System.Exit (exitFailure)
import Weigh

main :: IO ()
main = case fixtures of
  Nothing -> putStrLn "fixtures failed to build" >> exitFailure
  Just f  -> mainWith $ do
    wgroup "decode" $ do
      func "channel_announcement" decode_channel_announcement (f_ann_bytes f)
      func "node_announcement" decode_node_announcement (f_node_bytes f)
      func "channel_update" decode_channel_update (f_upd_bytes f)
      func "query_short_channel_ids (1000 ids, flags)"
        decode_query_short_channel_ids (f_qsci_bytes f)
      func "reply_channel_range (1000 ids, timestamps, checksums)"
        decode_reply_channel_range (f_rcr_bytes f)
    wgroup "encode" $ do
      func "channel_announcement" encode_channel_announcement (f_ann f)
      func "node_announcement" encode_node_announcement (f_node f)
      func "channel_update" encode_channel_update (f_upd f)
      func "query_short_channel_ids (1000 ids, flags)"
        encode_query_short_channel_ids (f_qsci f)
      func "reply_channel_range (1000 ids, timestamps, checksums)"
        encode_reply_channel_range (f_rcr f)
    wgroup "hash" $ do
      func "channel_announcement_hash" channel_announcement_hash (f_ann f)
      func "channel_update_hash" channel_update_hash (f_upd f)
      func "channel_update_checksum" channel_update_checksum (f_upd f)
    wgroup "verify" $ do
      func "verify_channel_announcement" verify_channel_announcement (f_ann f)
      func "verify_node_announcement" verify_node_announcement (f_node f)
      func "verify_channel_update"
        (verify_channel_update (f_upd_key f)) (f_upd f)
