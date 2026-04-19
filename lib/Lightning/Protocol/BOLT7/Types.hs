{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT7.Types
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Core types for BOLT #7 routing gossip.

module Lightning.Protocol.BOLT7.Types (
  -- * Identifiers (re-exported from BOLT1)
    ChainHash(..)
  , chainHash
  , unChainHash
  , mainnetChainHash
  , ShortChannelId(..)
  , shortChannelId
  , scidBlockHeight
  , scidTxIndex
  , scidOutputIndex
  , scidWord64
  , scidFromBytes
  , scidToBytes
  , formatScid
  , ChannelId(..)
  , channelId
  , unChannelId

  -- * Cryptographic types (re-exported from BOLT1)
  , Signature(..)
  , signature
  , unSignature
  , Point(..)
  , point
  , unPoint
  , NodeId
  , nodeId
  , getNodeId

  -- * Node metadata
  , RgbColor
  , rgbColor
  , getRgbColor
  , Alias
  , alias
  , getAlias
  , Timestamp
  , FeatureBits
  , featureBits
  , getFeatureBits

  -- * Address types
  , Address(..)
  , IPv4Addr
  , ipv4Addr
  , getIPv4Addr
  , IPv6Addr
  , ipv6Addr
  , getIPv6Addr
  , TorV3Addr
  , torV3Addr
  , getTorV3Addr

  -- * Channel update flags
  , MessageFlags(..)
  , encodeMessageFlags
  , decodeMessageFlags
  , ChannelFlags(..)
  , encodeChannelFlags
  , decodeChannelFlags

  -- * Routing parameters
  , CltvExpiryDelta(..)
  , FeeBaseMsat(..)
  , FeeProportionalMillionths(..)
  , HtlcMinimumMsat(..)
  , HtlcMaximumMsat(..)

  -- * Constants
  , chainHashLen
  , shortChannelIdLen
  , channelIdLen
  , signatureLen
  , pointLen
  , nodeIdLen
  , rgbColorLen
  , aliasLen
  , ipv4AddrLen
  , ipv6AddrLen
  , torV3AddrLen
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1.Prim
  ( ChainHash(..), unChainHash, chainHash
  , ShortChannelId(..), shortChannelId
  , scidBlockHeight, scidTxIndex, scidOutputIndex, scidWord64
  , ChannelId(..), unChannelId, channelId
  , Signature(..), unSignature, signature
  , Point(..), unPoint, point
  )

-- Constants -------------------------------------------------------------------

-- | Length of a chain hash (32 bytes).
chainHashLen :: Int
chainHashLen = 32
{-# INLINE chainHashLen #-}

-- | Length of a short channel ID (8 bytes).
shortChannelIdLen :: Int
shortChannelIdLen = 8
{-# INLINE shortChannelIdLen #-}

-- | Length of a channel ID (32 bytes).
channelIdLen :: Int
channelIdLen = 32
{-# INLINE channelIdLen #-}

-- | Length of a signature (64 bytes).
signatureLen :: Int
signatureLen = 64
{-# INLINE signatureLen #-}

-- | Length of a compressed public key (33 bytes).
pointLen :: Int
pointLen = 33
{-# INLINE pointLen #-}

-- | Length of a node ID (33 bytes, same as compressed public key).
nodeIdLen :: Int
nodeIdLen = 33
{-# INLINE nodeIdLen #-}

-- | Length of RGB color (3 bytes).
rgbColorLen :: Int
rgbColorLen = 3
{-# INLINE rgbColorLen #-}

-- | Length of node alias (32 bytes).
aliasLen :: Int
aliasLen = 32
{-# INLINE aliasLen #-}

-- | Length of IPv4 address (4 bytes).
ipv4AddrLen :: Int
ipv4AddrLen = 4
{-# INLINE ipv4AddrLen #-}

-- | Length of IPv6 address (16 bytes).
ipv6AddrLen :: Int
ipv6AddrLen = 16
{-# INLINE ipv6AddrLen #-}

-- | Length of Tor v3 address (35 bytes).
torV3AddrLen :: Int
torV3AddrLen = 35
{-# INLINE torV3AddrLen #-}

-- Identifiers -----------------------------------------------------------------

-- | Bitcoin mainnet chain hash (genesis block hash, little-endian).
mainnetChainHash :: ChainHash
mainnetChainHash = ChainHash $ BS.pack
  [ 0x6f, 0xe2, 0x8c, 0x0a, 0xb6, 0xf1, 0xb3, 0x72
  , 0xc1, 0xa6, 0xa2, 0x46, 0xae, 0x63, 0xf7, 0x4f
  , 0x93, 0x1e, 0x83, 0x65, 0xe1, 0x5a, 0x08, 0x9c
  , 0x68, 0xd6, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00
  ]

-- | Parse ShortChannelId from 8 big-endian bytes.
scidFromBytes :: ByteString -> Maybe ShortChannelId
scidFromBytes !bs
  | BS.length bs /= shortChannelIdLen = Nothing
  | otherwise =
      let !w = (fromIntegral (BS.index bs 0) `shiftL` 56)
           .|. (fromIntegral (BS.index bs 1) `shiftL` 48)
           .|. (fromIntegral (BS.index bs 2) `shiftL` 40)
           .|. (fromIntegral (BS.index bs 3) `shiftL` 32)
           .|. (fromIntegral (BS.index bs 4) `shiftL` 24)
           .|. (fromIntegral (BS.index bs 5) `shiftL` 16)
           .|. (fromIntegral (BS.index bs 6) `shiftL` 8)
           .|.  fromIntegral (BS.index bs 7) :: Word64
      in  Just (ShortChannelId w)
{-# INLINE scidFromBytes #-}

-- | Encode ShortChannelId as 8 big-endian bytes.
scidToBytes :: ShortChannelId -> ByteString
scidToBytes !sci =
  let !w = scidWord64 sci
  in  BS.pack
        [ fromIntegral (w `shiftR` 56)
        , fromIntegral (w `shiftR` 48)
        , fromIntegral (w `shiftR` 40)
        , fromIntegral (w `shiftR` 32)
        , fromIntegral (w `shiftR` 24)
        , fromIntegral (w `shiftR` 16)
        , fromIntegral (w `shiftR` 8)
        , fromIntegral w
        ]
{-# INLINE scidToBytes #-}

-- | Format short channel ID as human-readable string.
formatScid :: ShortChannelId -> String
formatScid sci =
  show (scidBlockHeight sci) ++ "x" ++
  show (scidTxIndex sci) ++ "x" ++
  show (scidOutputIndex sci)
{-# INLINE formatScid #-}

-- Cryptographic types ---------------------------------------------------------

-- | Node ID (33 bytes, same as compressed public key).
--
-- Has Ord instance for lexicographic comparison (required by spec for
-- channel announcements where node_id_1 < node_id_2).
newtype NodeId = NodeId { getNodeId :: ByteString }
  deriving (Eq, Ord, Show, Generic)

instance NFData NodeId

-- | Smart constructor for NodeId. Returns Nothing if not 33 bytes.
nodeId :: ByteString -> Maybe NodeId
nodeId !bs
  | BS.length bs == nodeIdLen = Just (NodeId bs)
  | otherwise = Nothing
{-# INLINE nodeId #-}

-- Node metadata ---------------------------------------------------------------

-- | RGB color (3 bytes).
newtype RgbColor = RgbColor { getRgbColor :: ByteString }
  deriving (Eq, Show, Generic)

instance NFData RgbColor

-- | Smart constructor for RgbColor. Returns Nothing if not 3 bytes.
rgbColor :: ByteString -> Maybe RgbColor
rgbColor !bs
  | BS.length bs == rgbColorLen = Just (RgbColor bs)
  | otherwise = Nothing
{-# INLINE rgbColor #-}

-- | Node alias (32 bytes, UTF-8 padded with zero bytes).
newtype Alias = Alias { getAlias :: ByteString }
  deriving (Eq, Show, Generic)

instance NFData Alias

-- | Smart constructor for Alias. Returns Nothing if not 32 bytes.
alias :: ByteString -> Maybe Alias
alias !bs
  | BS.length bs == aliasLen = Just (Alias bs)
  | otherwise = Nothing
{-# INLINE alias #-}

-- | Timestamp (Unix epoch seconds).
type Timestamp = Word32

-- | Feature bits (variable length).
newtype FeatureBits = FeatureBits { getFeatureBits :: ByteString }
  deriving (Eq, Show, Generic)

instance NFData FeatureBits

-- | Smart constructor for FeatureBits (any length).
featureBits :: ByteString -> FeatureBits
featureBits = FeatureBits
{-# INLINE featureBits #-}

-- Address types ---------------------------------------------------------------

-- | IPv4 address (4 bytes).
newtype IPv4Addr = IPv4Addr { getIPv4Addr :: ByteString }
  deriving (Eq, Show, Generic)

instance NFData IPv4Addr

-- | Smart constructor for IPv4Addr. Returns Nothing if not 4 bytes.
ipv4Addr :: ByteString -> Maybe IPv4Addr
ipv4Addr !bs
  | BS.length bs == ipv4AddrLen = Just (IPv4Addr bs)
  | otherwise = Nothing
{-# INLINE ipv4Addr #-}

-- | IPv6 address (16 bytes).
newtype IPv6Addr = IPv6Addr { getIPv6Addr :: ByteString }
  deriving (Eq, Show, Generic)

instance NFData IPv6Addr

-- | Smart constructor for IPv6Addr. Returns Nothing if not 16 bytes.
ipv6Addr :: ByteString -> Maybe IPv6Addr
ipv6Addr !bs
  | BS.length bs == ipv6AddrLen = Just (IPv6Addr bs)
  | otherwise = Nothing
{-# INLINE ipv6Addr #-}

-- | Tor v3 onion address (35 bytes: 32 pubkey + 2 checksum + 1 version).
newtype TorV3Addr = TorV3Addr { getTorV3Addr :: ByteString }
  deriving (Eq, Show, Generic)

instance NFData TorV3Addr

-- | Smart constructor for TorV3Addr. Returns Nothing if not 35 bytes.
torV3Addr :: ByteString -> Maybe TorV3Addr
torV3Addr !bs
  | BS.length bs == torV3AddrLen = Just (TorV3Addr bs)
  | otherwise = Nothing
{-# INLINE torV3Addr #-}

-- | Network address with port.
data Address
  = AddrIPv4 !IPv4Addr !Word16    -- ^ IPv4 address + port
  | AddrIPv6 !IPv6Addr !Word16    -- ^ IPv6 address + port
  | AddrTorV3 !TorV3Addr !Word16  -- ^ Tor v3 address + port
  | AddrDNS !ByteString !Word16   -- ^ DNS hostname + port
  deriving (Eq, Show, Generic)

instance NFData Address

-- Channel update flags --------------------------------------------------------

-- | Message flags for channel_update.
--
-- Bit 0: htlc_maximum_msat field is present.
data MessageFlags = MessageFlags
  { mfHtlcMaxPresent :: !Bool  -- ^ htlc_maximum_msat is present
  }
  deriving (Eq, Show, Generic)

instance NFData MessageFlags

-- | Encode MessageFlags to Word8.
encodeMessageFlags :: MessageFlags -> Word8
encodeMessageFlags mf = if mfHtlcMaxPresent mf then 0x01 else 0x00
{-# INLINE encodeMessageFlags #-}

-- | Decode Word8 to MessageFlags.
decodeMessageFlags :: Word8 -> MessageFlags
decodeMessageFlags w = MessageFlags { mfHtlcMaxPresent = w .&. 0x01 /= 0 }
{-# INLINE decodeMessageFlags #-}

-- | Channel flags for channel_update.
--
-- Bit 0: direction (0 = node_id_1 is origin, 1 = node_id_2 is origin).
-- Bit 1: disabled (1 = channel disabled).
data ChannelFlags = ChannelFlags
  { cfDirection :: !Bool  -- ^ True = node_id_2 is origin
  , cfDisabled  :: !Bool  -- ^ True = channel is disabled
  }
  deriving (Eq, Show, Generic)

instance NFData ChannelFlags

-- | Encode ChannelFlags to Word8.
encodeChannelFlags :: ChannelFlags -> Word8
encodeChannelFlags cf =
  (if cfDirection cf then 0x01 else 0x00) .|.
  (if cfDisabled cf then 0x02 else 0x00)
{-# INLINE encodeChannelFlags #-}

-- | Decode Word8 to ChannelFlags.
decodeChannelFlags :: Word8 -> ChannelFlags
decodeChannelFlags w = ChannelFlags
  { cfDirection = w .&. 0x01 /= 0
  , cfDisabled  = w .&. 0x02 /= 0
  }
{-# INLINE decodeChannelFlags #-}

-- Routing parameters ----------------------------------------------------------

-- | CLTV expiry delta.
newtype CltvExpiryDelta = CltvExpiryDelta { getCltvExpiryDelta :: Word16 }
  deriving (Eq, Ord, Show, Generic)

instance NFData CltvExpiryDelta

-- | Base fee in millisatoshis.
newtype FeeBaseMsat = FeeBaseMsat { getFeeBaseMsat :: Word32 }
  deriving (Eq, Ord, Show, Generic)

instance NFData FeeBaseMsat

-- | Proportional fee in millionths.
newtype FeeProportionalMillionths = FeeProportionalMillionths
  { getFeeProportionalMillionths :: Word32 }
  deriving (Eq, Ord, Show, Generic)

instance NFData FeeProportionalMillionths

-- | Minimum HTLC value in millisatoshis.
newtype HtlcMinimumMsat = HtlcMinimumMsat { getHtlcMinimumMsat :: Word64 }
  deriving (Eq, Ord, Show, Generic)

instance NFData HtlcMinimumMsat

-- | Maximum HTLC value in millisatoshis.
newtype HtlcMaximumMsat = HtlcMaximumMsat { getHtlcMaximumMsat :: Word64 }
  deriving (Eq, Ord, Show, Generic)

instance NFData HtlcMaximumMsat
