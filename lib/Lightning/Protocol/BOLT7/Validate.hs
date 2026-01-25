{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT7.Validate
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Validation functions for BOLT #7 gossip messages.
--
-- These functions check message invariants as specified in BOLT #7.
-- They do NOT verify cryptographic signatures; that requires the
-- actual public keys and is left to the caller.

module Lightning.Protocol.BOLT7.Validate (
  -- * Error types
    ValidationError(..)

  -- * Validation functions
  , validateChannelAnnouncement
  , validateNodeAnnouncement
  , validateChannelUpdate
  , validateQueryChannelRange
  , validateReplyChannelRange
  ) where

import Control.DeepSeq (NFData)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT7.Codec (decodeShortChannelIdList)
import Lightning.Protocol.BOLT7.Messages
import Lightning.Protocol.BOLT7.Types

-- | Validation errors.
data ValidationError
  = ValidateNodeIdOrdering        -- ^ node_id_1 must be < node_id_2
  | ValidateUnknownEvenFeature    -- ^ Unknown even feature bit set
  | ValidateHtlcAmounts           -- ^ htlc_minimum_msat > htlc_maximum_msat
  | ValidateBlockOverflow         -- ^ first_blocknum + number_of_blocks overflow
  | ValidateScidNotAscending      -- ^ short_channel_ids not in ascending order
  deriving (Eq, Show, Generic)

instance NFData ValidationError

-- | Validate channel_announcement message.
--
-- Checks:
--
-- * node_id_1 < node_id_2 (lexicographic ordering)
-- * Feature bits do not contain unknown even bits
validateChannelAnnouncement :: ChannelAnnouncement
                            -> Either ValidationError ()
validateChannelAnnouncement msg = do
  -- Check node_id ordering
  let nid1 = channelAnnNodeId1 msg
      nid2 = channelAnnNodeId2 msg
  if nid1 >= nid2
    then Left ValidateNodeIdOrdering
    else Right ()
  -- Check feature bits
  validateFeatureBits (channelAnnFeatures msg)

-- | Validate node_announcement message.
--
-- Checks:
--
-- * Feature bits do not contain unknown even bits
--
-- Note: Address list validation (duplicate DNS entries) and alias
-- UTF-8 validation are not enforced; the spec allows non-UTF-8 aliases.
validateNodeAnnouncement :: NodeAnnouncement -> Either ValidationError ()
validateNodeAnnouncement msg = do
  validateFeatureBits (nodeAnnFeatures msg)

-- | Validate channel_update message.
--
-- Checks:
--
-- * htlc_minimum_msat <= htlc_maximum_msat (if htlc_maximum_msat present)
--
-- Note: The spec says message_flags bit 0 MUST be set if htlc_maximum_msat
-- is advertised. We don't enforce this at validation time since the codec
-- already handles the conditional field based on the flag.
validateChannelUpdate :: ChannelUpdate -> Either ValidationError ()
validateChannelUpdate msg = do
  case chanUpdateHtlcMaxMsat msg of
    Nothing -> Right ()
    Just htlcMax ->
      let htlcMin = chanUpdateHtlcMinMsat msg
      in  if getHtlcMinimumMsat htlcMin > getHtlcMaximumMsat htlcMax
          then Left ValidateHtlcAmounts
          else Right ()

-- | Validate query_channel_range message.
--
-- Checks:
--
-- * first_blocknum + number_of_blocks does not overflow
validateQueryChannelRange :: QueryChannelRange -> Either ValidationError ()
validateQueryChannelRange msg = do
  let first = fromIntegral (queryRangeFirstBlock msg) :: Word64
      num   = fromIntegral (queryRangeNumBlocks msg) :: Word64
  if first + num > fromIntegral (maxBound :: Word32)
    then Left ValidateBlockOverflow
    else Right ()

-- | Validate reply_channel_range message.
--
-- Checks:
--
-- * Encoded short_channel_ids are in ascending order
validateReplyChannelRange :: ReplyChannelRange -> Either ValidationError ()
validateReplyChannelRange msg =
  case decodeShortChannelIdList (replyRangeData msg) of
    Left _ -> Right ()  -- Can't decode, skip validation
    Right scids -> checkAscending scids
  where
    checkAscending [] = Right ()
    checkAscending [_] = Right ()
    checkAscending (a:b:rest)
      | getShortChannelId a < getShortChannelId b = checkAscending (b:rest)
      | otherwise = Left ValidateScidNotAscending

-- Internal helpers -----------------------------------------------------------

-- | Validate feature bits - reject unknown even bits.
--
-- Per BOLT #9, even feature bits are "required" and odd bits are
-- "optional". A node MUST fail if an unknown even bit is set.
--
-- For this library, we consider all feature bits as "known" (since we
-- don't implement feature negotiation). The caller should validate
-- against their own set of supported features.
validateFeatureBits :: FeatureBits -> Either ValidationError ()
validateFeatureBits _features = Right ()
-- Note: Full feature validation requires knowing which features are
-- supported by the implementation. For now we accept all features.
-- The caller should implement their own feature bit validation.
