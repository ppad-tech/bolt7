{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT7.Validate
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Stateless checks of BOLT #7 messages.

module Lightning.Protocol.BOLT7.Validate (
    ValidationError(..)
  , validate_channel_announcement
  , validate_node_announcement
  , validate_channel_update
  , validate_query_short_channel_ids
  , validate_query_channel_range
  , validate_reply_channel_range
  ) where

import Control.DeepSeq (NFData)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1 (ShortChannelId)
import Lightning.Protocol.BOLT7.Messages
import Lightning.Protocol.BOLT7.Types

-- | A rule a message breaks.
data ValidationError
  = ValidateNodeIdOrdering
    -- ^ @node_id_1@ is not lexicographically less than @node_id_2@
  | ValidateMultipleDns
    -- ^ more than one DNS hostname descriptor
  | ValidateHtlcAmounts
    -- ^ @htlc_minimum_msat@ exceeds @htlc_maximum_msat@
  | ValidateZeroBlocks
    -- ^ @number_of_blocks@ is 0
  | ValidateBlockOverflow
    -- ^ the block range extends past block 2^32 - 1
  | ValidateScidNotAscending
    -- ^ short channel ids are not in strictly ascending order
  deriving (Eq, Show, Generic)

instance NFData ValidationError

-- | Check that @node_id_1@ is lexicographically less than @node_id_2@;
--   a receiver must ignore the message otherwise.
--
--   Signatures and feature bits are not checked here; see
--   'Lightning.Protocol.BOLT7.verify_channel_announcement' and
--   ppad-bolt9's @validate_remote@.
validate_channel_announcement
  :: ChannelAnnouncement -> Either ValidationError ()
validate_channel_announcement m
  | ca_node_id_1 m < ca_node_id_2 m = Right ()
  | otherwise                       = Left ValidateNodeIdOrdering

-- | Check that at most one DNS hostname is announced; a receiver must
--   not forward the message otherwise.
--
--   Descriptors a receiver should merely ignore (Tor v2, port 0, a
--   hostname that isn't ASCII) are left to
--   'Lightning.Protocol.BOLT7.usable_addresses'.
validate_node_announcement :: NodeAnnouncement -> Either ValidationError ()
validate_node_announcement m
  | length [ () | AddrDNS _ _ <- na_addresses m ] > 1 =
      Left ValidateMultipleDns
  | otherwise = Right ()

-- | Check that @htlc_minimum_msat@ does not exceed @htlc_maximum_msat@;
--   a receiver should not route through the channel otherwise.
validate_channel_update :: ChannelUpdate -> Either ValidationError ()
validate_channel_update m
  | cu_htlc_minimum_msat m > cu_htlc_maximum_msat m = Left ValidateHtlcAmounts
  | otherwise = Right ()

-- | Check that the short channel ids are in strictly ascending order, as
--   their encoding requires.
validate_query_short_channel_ids
  :: QueryShortChannelIds -> Either ValidationError ()
validate_query_short_channel_ids = ascending . qsci_short_channel_ids

-- | Check that @number_of_blocks@ is at least 1, as the sender must
--   ensure, and that the range ends at or before block 2^32 - 1 (a
--   library rule: such a range can't be represented).
validate_query_channel_range
  :: QueryChannelRange -> Either ValidationError ()
validate_query_channel_range m
  | num == 0                  = Left ValidateZeroBlocks
  | first + num > 0x100000000 = Left ValidateBlockOverflow
  | otherwise                 = Right ()
  where
    first = fromIntegral (qcr_first_blocknum m) :: Word64
    num   = fromIntegral (qcr_number_of_blocks m) :: Word64

-- | Check that the short channel ids are in strictly ascending order, as
--   their encoding requires.
validate_reply_channel_range
  :: ReplyChannelRange -> Either ValidationError ()
validate_reply_channel_range = ascending . rcr_short_channel_ids

ascending :: [ShortChannelId] -> Either ValidationError ()
ascending (a : rest@(b : _))
  | a < b     = ascending rest
  | otherwise = Left ValidateScidNotAscending
ascending _ = Right ()
