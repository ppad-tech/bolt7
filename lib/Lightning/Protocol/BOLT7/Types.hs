{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT7.Types
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Field types for BOLT #7 gossip messages.

module Lightning.Protocol.BOLT7.Types (
  -- * Short channel ids
    format_short_channel_id

  -- * channel_update flags
  , MessageFlags(..)
  , Forwarding(..)
  , message_flags
  , message_flags_forwarding
  , has_must_be_one
  , ChannelFlags(..)
  , Direction(..)
  , ChannelStatus(..)
  , channel_flags
  , channel_flags_direction
  , channel_flags_status

  -- * node_announcement fields
  , RgbColor(..)
  , Alias(..)
  , alias
  , un_alias
  , Address(..)
  , IPv4Addr(..)
  , ipv4_addr
  , un_ipv4_addr
  , IPv6Addr(..)
  , ipv6_addr
  , un_ipv6_addr
  , TorV2Addr(..)
  , tor_v2_addr
  , un_tor_v2_addr
  , TorV3Addr(..)
  , tor_v3_addr
  , un_tor_v3_addr
  , Hostname(..)
  , hostname
  , un_hostname
  , is_ascii_hostname
  , usable_addresses

  -- * Query fields
  , QueryFlags(..)
  , QueryFlag(..)
  , query_flags
  , has_query_flag
  , QueryOption(..)
  , QueryOptionFlag(..)
  , query_option
  , has_query_option
  , ChannelUpdateTimestamps(..)
  , ChannelUpdateChecksums(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Bits ((.|.), bit, testBit)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1
  (ShortChannelId, scid_block_height, scid_tx_index, scid_output_index)

-- short channel ids ----------------------------------------------------------

-- | Render a short channel id in the human-readable format of BOLT #7:
--   block height, transaction index and output index, in decimal,
--   separated by @x@.
--
--   >>> fmap format_short_channel_id (short_channel_id 539268 845 1)
--   Just "539268x845x1"
format_short_channel_id :: ShortChannelId -> String
format_short_channel_id s =
  show (scid_block_height s) ++ "x" ++ show (scid_tx_index s) ++ "x"
    ++ show (scid_output_index s)

-- channel_update flags -------------------------------------------------------

-- | The @message_flags@ byte of a @channel_update@: bit 0 is
--   @must_be_one@, bit 1 is @dont_forward@, and the other bits are
--   unassigned. Every byte is a valid value; unassigned bits are kept so
--   that re-encoding reproduces the signed bytes.
newtype MessageFlags = MessageFlags Word8
  deriving (Eq, Show, Generic)

instance NFData MessageFlags

-- | Whether a @channel_update@ may be relayed to other peers (the
--   @dont_forward@ bit).
data Forwarding
  = Forward      -- ^ @dont_forward@ is 0
  | DontForward  -- ^ @dont_forward@ is 1, as for unannounced channels
  deriving (Eq, Ord, Show, Generic)

instance NFData Forwarding

-- | The 'MessageFlags' an origin node sets: @must_be_one@ set, the given
--   @dont_forward@ bit, and no unassigned bits.
--
--   >>> message_flags DontForward
--   MessageFlags 3
message_flags :: Forwarding -> MessageFlags
message_flags Forward     = MessageFlags 0x01
message_flags DontForward = MessageFlags 0x03
{-# INLINE message_flags #-}

-- | The @dont_forward@ bit of 'MessageFlags'.
--
--   >>> message_flags_forwarding (MessageFlags 0x01)
--   Forward
message_flags_forwarding :: MessageFlags -> Forwarding
message_flags_forwarding (MessageFlags w)
  | testBit w 1 = DontForward
  | otherwise   = Forward
{-# INLINE message_flags_forwarding #-}

-- | Whether the @must_be_one@ bit is set. Senders must set it; receivers
--   ignore it.
--
--   >>> has_must_be_one (MessageFlags 0x00)
--   False
has_must_be_one :: MessageFlags -> Bool
has_must_be_one (MessageFlags w) = testBit w 0
{-# INLINE has_must_be_one #-}

-- | The @channel_flags@ byte of a @channel_update@: bit 0 is
--   @direction@, bit 1 is @disable@, and the other bits are unassigned.
--   Every byte is a valid value; unassigned bits are kept so that
--   re-encoding reproduces the signed bytes.
newtype ChannelFlags = ChannelFlags Word8
  deriving (Eq, Show, Generic)

instance NFData ChannelFlags

-- | The node a @channel_update@ originates from.
data Direction
  = NodeOne  -- ^ @node_id_1@ (@direction@ is 0)
  | NodeTwo  -- ^ @node_id_2@ (@direction@ is 1)
  deriving (Eq, Ord, Show, Generic)

instance NFData Direction

-- | Whether a channel direction is usable.
data ChannelStatus
  = Enabled   -- ^ @disable@ is 0
  | Disabled  -- ^ @disable@ is 1
  deriving (Eq, Ord, Show, Generic)

instance NFData ChannelStatus

-- | The 'ChannelFlags' for the given direction and status, with no
--   unassigned bits set.
--
--   >>> channel_flags NodeTwo Disabled
--   ChannelFlags 3
channel_flags :: Direction -> ChannelStatus -> ChannelFlags
channel_flags d s = ChannelFlags (dir .|. sta)
  where
    dir = case d of
      NodeOne -> 0x00
      NodeTwo -> 0x01
    sta = case s of
      Enabled  -> 0x00
      Disabled -> 0x02
{-# INLINE channel_flags #-}

-- | The @direction@ bit of 'ChannelFlags'.
--
--   >>> channel_flags_direction (ChannelFlags 0x05)
--   NodeTwo
channel_flags_direction :: ChannelFlags -> Direction
channel_flags_direction (ChannelFlags w)
  | testBit w 0 = NodeTwo
  | otherwise   = NodeOne
{-# INLINE channel_flags_direction #-}

-- | The @disable@ bit of 'ChannelFlags'.
--
--   >>> channel_flags_status (ChannelFlags 0x02)
--   Disabled
channel_flags_status :: ChannelFlags -> ChannelStatus
channel_flags_status (ChannelFlags w)
  | testBit w 1 = Disabled
  | otherwise   = Enabled
{-# INLINE channel_flags_status #-}

-- node_announcement fields ---------------------------------------------------

-- | A node's colour: red, green and blue.
data RgbColor = RgbColor
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  deriving (Eq, Show, Generic)

instance NFData RgbColor

-- | A 32-byte node alias. Origin nodes should use UTF-8 padded with zero
--   bytes, but receivers must accept any bytes.
newtype Alias = Alias BS.ByteString
  deriving (Eq, Show, Generic)

instance NFData Alias

-- | Construct an 'Alias' from exactly 32 bytes.
--
--   >>> alias "too short"
--   Nothing
alias :: BS.ByteString -> Maybe Alias
alias bs
  | BS.length bs == 32 = Just (Alias bs)
  | otherwise          = Nothing
{-# INLINE alias #-}

-- | The bytes of an 'Alias'.
un_alias :: Alias -> BS.ByteString
un_alias (Alias bs) = bs
{-# INLINE un_alias #-}

-- | A 4-byte IPv4 address.
newtype IPv4Addr = IPv4Addr BS.ByteString
  deriving (Eq, Show, Generic)

instance NFData IPv4Addr

-- | Construct an 'IPv4Addr' from exactly 4 bytes.
--
--   >>> ipv4_addr "\127\NUL\NUL\SOH"
--   Just (IPv4Addr "\DEL\NUL\NUL\SOH")
ipv4_addr :: BS.ByteString -> Maybe IPv4Addr
ipv4_addr bs
  | BS.length bs == 4 = Just (IPv4Addr bs)
  | otherwise         = Nothing
{-# INLINE ipv4_addr #-}

-- | The bytes of an 'IPv4Addr'.
un_ipv4_addr :: IPv4Addr -> BS.ByteString
un_ipv4_addr (IPv4Addr bs) = bs
{-# INLINE un_ipv4_addr #-}

-- | A 16-byte IPv6 address.
newtype IPv6Addr = IPv6Addr BS.ByteString
  deriving (Eq, Show, Generic)

instance NFData IPv6Addr

-- | Construct an 'IPv6Addr' from exactly 16 bytes.
ipv6_addr :: BS.ByteString -> Maybe IPv6Addr
ipv6_addr bs
  | BS.length bs == 16 = Just (IPv6Addr bs)
  | otherwise          = Nothing
{-# INLINE ipv6_addr #-}

-- | The bytes of an 'IPv6Addr'.
un_ipv6_addr :: IPv6Addr -> BS.ByteString
un_ipv6_addr (IPv6Addr bs) = bs
{-# INLINE un_ipv6_addr #-}

-- | The 12 data bytes of a deprecated Tor v2 address descriptor (type
--   3). They are kept, uninterpreted, so that re-encoding reproduces
--   the signed bytes.
newtype TorV2Addr = TorV2Addr BS.ByteString
  deriving (Eq, Show, Generic)

instance NFData TorV2Addr

-- | Construct a 'TorV2Addr' from exactly 12 bytes.
tor_v2_addr :: BS.ByteString -> Maybe TorV2Addr
tor_v2_addr bs
  | BS.length bs == 12 = Just (TorV2Addr bs)
  | otherwise          = Nothing
{-# INLINE tor_v2_addr #-}

-- | The bytes of a 'TorV2Addr'.
un_tor_v2_addr :: TorV2Addr -> BS.ByteString
un_tor_v2_addr (TorV2Addr bs) = bs
{-# INLINE un_tor_v2_addr #-}

-- | A 35-byte Tor v3 onion service address: a 32-byte ed25519 public
--   key, a 2-byte checksum and a 1-byte version.
newtype TorV3Addr = TorV3Addr BS.ByteString
  deriving (Eq, Show, Generic)

instance NFData TorV3Addr

-- | Construct a 'TorV3Addr' from exactly 35 bytes.
tor_v3_addr :: BS.ByteString -> Maybe TorV3Addr
tor_v3_addr bs
  | BS.length bs == 35 = Just (TorV3Addr bs)
  | otherwise          = Nothing
{-# INLINE tor_v3_addr #-}

-- | The bytes of a 'TorV3Addr'.
un_tor_v3_addr :: TorV3Addr -> BS.ByteString
un_tor_v3_addr (TorV3Addr bs) = bs
{-# INLINE un_tor_v3_addr #-}

-- | A DNS hostname of at most 255 bytes (the limit of its length byte).
--
--   BOLT #7 requires hostnames to be ASCII, with other characters
--   Punycode-encoded, but receivers keep whatever bytes they are given;
--   see 'is_ascii_hostname'.
newtype Hostname = Hostname BS.ByteString
  deriving (Eq, Show, Generic)

instance NFData Hostname

-- | Construct a 'Hostname' from at most 255 bytes.
--
--   >>> hostname "ln.example.com"
--   Just (Hostname "ln.example.com")
--   >>> hostname (BS.replicate 256 0x61)
--   Nothing
hostname :: BS.ByteString -> Maybe Hostname
hostname bs
  | BS.length bs <= 255 = Just (Hostname bs)
  | otherwise           = Nothing
{-# INLINE hostname #-}

-- | The bytes of a 'Hostname'.
un_hostname :: Hostname -> BS.ByteString
un_hostname (Hostname bs) = bs
{-# INLINE un_hostname #-}

-- | Whether a 'Hostname' is non-empty and ASCII, as BOLT #7 requires.
--
--   >>> fmap is_ascii_hostname (hostname "caf\233")
--   Just False
is_ascii_hostname :: Hostname -> Bool
is_ascii_hostname (Hostname bs) = not (BS.null bs) && BS.all (< 0x80) bs
{-# INLINE is_ascii_hostname #-}

-- | An address descriptor of a known type, with its port where it has
--   one.
data Address
  = AddrIPv4  !IPv4Addr  {-# UNPACK #-} !Word16  -- ^ type 1
  | AddrIPv6  !IPv6Addr  {-# UNPACK #-} !Word16  -- ^ type 2
  | AddrTorV2 !TorV2Addr                         -- ^ type 3 (deprecated)
  | AddrTorV3 !TorV3Addr {-# UNPACK #-} !Word16  -- ^ type 4
  | AddrDNS   !Hostname  {-# UNPACK #-} !Word16  -- ^ type 5
  deriving (Eq, Show, Generic)

instance NFData Address

-- | The addresses a receiver should connect to, per the BOLT #7 receiver
--   rules: drops Tor v2 descriptors, descriptors with port 0, DNS
--   hostnames that are empty or not ASCII, and every DNS descriptor
--   after the first.
usable_addresses :: [Address] -> [Address]
usable_addresses = go False
  where
    go _ [] = []
    go !dns (a : as) = case a of
      AddrIPv4 _ p  -> port p
      AddrIPv6 _ p  -> port p
      AddrTorV2 _   -> go dns as
      AddrTorV3 _ p -> port p
      AddrDNS h p
        | dns                            -> go dns as
        | p /= 0 && is_ascii_hostname h  -> a : go True as
        | otherwise                      -> go True as
      where
        port p
          | p == 0    = go dns as
          | otherwise = a : go dns as

-- query fields ---------------------------------------------------------------

-- | A @query_flags@ bitfield, sent for each short channel id in a
--   @query_short_channel_ids@. Every value is valid; unassigned bits are
--   kept.
newtype QueryFlags = QueryFlags Word64
  deriving (Eq, Show, Generic)

instance NFData QueryFlags

-- | The assigned bits of 'QueryFlags'.
data QueryFlag
  = WantChannelAnnouncement  -- ^ bit 0
  | WantChannelUpdate1       -- ^ bit 1: the @channel_update@ of node 1
  | WantChannelUpdate2       -- ^ bit 2: the @channel_update@ of node 2
  | WantNodeAnnouncement1    -- ^ bit 3: the @node_announcement@ of node 1
  | WantNodeAnnouncement2    -- ^ bit 4: the @node_announcement@ of node 2
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData QueryFlag

-- | The 'QueryFlags' with exactly the given bits set.
--
--   >>> query_flags [WantChannelAnnouncement, WantChannelUpdate2]
--   QueryFlags 5
query_flags :: [QueryFlag] -> QueryFlags
query_flags = QueryFlags . foldr (\f acc -> acc .|. bit (fromEnum f)) 0

-- | Whether the given bit is set.
--
--   >>> has_query_flag WantChannelUpdate1 (QueryFlags 2)
--   True
has_query_flag :: QueryFlag -> QueryFlags -> Bool
has_query_flag f (QueryFlags w) = testBit w (fromEnum f)
{-# INLINE has_query_flag #-}

-- | The @query_option_flags@ bitfield of a @query_channel_range@. Every
--   value is valid; unassigned bits are kept.
newtype QueryOption = QueryOption Word64
  deriving (Eq, Show, Generic)

instance NFData QueryOption

-- | The assigned bits of 'QueryOption'.
data QueryOptionFlag
  = WantTimestamps  -- ^ bit 0
  | WantChecksums   -- ^ bit 1
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData QueryOptionFlag

-- | The 'QueryOption' with exactly the given bits set.
--
--   >>> query_option [WantTimestamps, WantChecksums]
--   QueryOption 3
query_option :: [QueryOptionFlag] -> QueryOption
query_option = QueryOption . foldr (\f acc -> acc .|. bit (fromEnum f)) 0

-- | Whether the given bit is set.
--
--   >>> has_query_option WantChecksums (QueryOption 1)
--   False
has_query_option :: QueryOptionFlag -> QueryOption -> Bool
has_query_option f (QueryOption w) = testBit w (fromEnum f)
{-# INLINE has_query_option #-}

-- | The timestamps of a channel's latest @channel_update@s, from
--   @node_id_1@ and @node_id_2@ (0 where there is none).
data ChannelUpdateTimestamps = ChannelUpdateTimestamps
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  deriving (Eq, Show, Generic)

instance NFData ChannelUpdateTimestamps

-- | The checksums of a channel's latest @channel_update@s, from
--   @node_id_1@ and @node_id_2@ (0 where there is none).
data ChannelUpdateChecksums = ChannelUpdateChecksums
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  deriving (Eq, Show, Generic)

instance NFData ChannelUpdateChecksums
