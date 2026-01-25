{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Lightning.Protocol.BOLT7
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Routing gossip protocol for the Lightning Network, per
-- [BOLT #7](https://github.com/lightning/bolts/blob/master/07-routing-gossip.md).

module Lightning.Protocol.BOLT7 (
  -- * Core types
  -- | Re-exported from "Lightning.Protocol.BOLT7.Types".
    module Lightning.Protocol.BOLT7.Types

  -- * Message types
  -- | Re-exported from "Lightning.Protocol.BOLT7.Messages".
  , module Lightning.Protocol.BOLT7.Messages

  -- * Codec functions
  -- | Re-exported from "Lightning.Protocol.BOLT7.Codec".
  , module Lightning.Protocol.BOLT7.Codec

  -- * Hash functions
  -- | Re-exported from "Lightning.Protocol.BOLT7.Hash".
  , module Lightning.Protocol.BOLT7.Hash

  -- * Validation functions
  -- | Re-exported from "Lightning.Protocol.BOLT7.Validate".
  , module Lightning.Protocol.BOLT7.Validate

  -- $messagetypes

  -- ** Channel announcement
  -- $announcement

  -- ** Node announcement
  -- $nodeannouncement

  -- ** Channel updates
  -- $updates

  -- ** Gossip queries
  -- $queries
  ) where

import Lightning.Protocol.BOLT7.Codec
import Lightning.Protocol.BOLT7.Hash
import Lightning.Protocol.BOLT7.Messages
import Lightning.Protocol.BOLT7.Types
import Lightning.Protocol.BOLT7.Validate

-- $messagetypes
--
-- BOLT #7 defines the following message types:
--
-- * 256: channel_announcement
-- * 257: node_announcement
-- * 258: channel_update
-- * 259: announcement_signatures
-- * 261: query_short_channel_ids
-- * 262: reply_short_channel_ids_end
-- * 263: query_channel_range
-- * 264: reply_channel_range
-- * 265: gossip_timestamp_filter

-- $announcement
--
-- Channel announcement messages:
--
-- * channel_announcement (256) - public channel announcement
-- * announcement_signatures (259) - signatures enabling announcement

-- $nodeannouncement
--
-- Node announcement message:
--
-- * node_announcement (257) - advertises node metadata

-- $updates
--
-- Channel update message:
--
-- * channel_update (258) - per-direction routing parameters

-- $queries
--
-- Gossip query messages:
--
-- * query_short_channel_ids (261) - request specific channel info
-- * reply_short_channel_ids_end (262) - concludes query response
-- * query_channel_range (263) - query channels in block range
-- * reply_channel_range (264) - response with channel IDs
-- * gossip_timestamp_filter (265) - constrain relayed gossip
