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
  -- * Identifiers
    ChainHash
  , chainHash
  , getChainHash
  , mainnetChainHash
  , ShortChannelId
  , shortChannelId
  , mkShortChannelId
  , getShortChannelId
  , scidBlockHeight
  , scidTxIndex
  , scidOutputIndex
  , formatScid
  , ChannelId
  , channelId
  , getChannelId

  -- * Cryptographic types
  , Signature
  , signature
  , getSignature
  , Point
  , point
  , getPoint
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

  -- * Routing parameters
  , CltvExpiryDelta
  , FeeBaseMsat
  , FeeProportionalMillionths
  , HtlcMinimumMsat
  , HtlcMaximumMsat

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

-- | Chain hash identifying the blockchain (32 bytes).
newtype ChainHash = ChainHash { getChainHash :: ByteString }
  deriving (Eq, Show, Generic)

instance NFData ChainHash

-- | Smart constructor for ChainHash. Returns Nothing if not 32 bytes.
chainHash :: ByteString -> Maybe ChainHash
chainHash !bs
  | BS.length bs == chainHashLen = Just (ChainHash bs)
  | otherwise = Nothing
{-# INLINE chainHash #-}

-- | Bitcoin mainnet chain hash (genesis block hash, little-endian).
--
-- This is the double-SHA256 of the mainnet genesis block header, reversed
-- to little-endian byte order as used in the protocol.
mainnetChainHash :: ChainHash
mainnetChainHash = ChainHash $ BS.pack
  [ 0x6f, 0xe2, 0x8c, 0x0a, 0xb6, 0xf1, 0xb3, 0x72
  , 0xc1, 0xa6, 0xa2, 0x46, 0xae, 0x63, 0xf7, 0x4f
  , 0x93, 0x1e, 0x83, 0x65, 0xe1, 0x5a, 0x08, 0x9c
  , 0x68, 0xd6, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00
  ]

-- | Short channel ID (8 bytes): block height (3) + tx index (3) + output (2).
newtype ShortChannelId = ShortChannelId { getShortChannelId :: ByteString }
  deriving (Eq, Show, Generic)

instance NFData ShortChannelId

-- | Smart constructor for ShortChannelId. Returns Nothing if not 8 bytes.
shortChannelId :: ByteString -> Maybe ShortChannelId
shortChannelId !bs
  | BS.length bs == shortChannelIdLen = Just (ShortChannelId bs)
  | otherwise = Nothing
{-# INLINE shortChannelId #-}

-- | Construct ShortChannelId from components.
--
-- Block height and tx index are truncated to 24 bits.
--
-- >>> mkShortChannelId 539268 845 1
-- ShortChannelId {getShortChannelId = "\NUL\131\132\NUL\ETX-\NUL\SOH"}
mkShortChannelId
  :: Word32  -- ^ Block height (24 bits)
  -> Word32  -- ^ Transaction index (24 bits)
  -> Word16  -- ^ Output index
  -> ShortChannelId
mkShortChannelId !block !txIdx !outIdx = ShortChannelId $ BS.pack
  [ fromIntegral ((block `shiftR` 16) .&. 0xff) :: Word8
  , fromIntegral ((block `shiftR` 8) .&. 0xff)
  , fromIntegral (block .&. 0xff)
  , fromIntegral ((txIdx `shiftR` 16) .&. 0xff)
  , fromIntegral ((txIdx `shiftR` 8) .&. 0xff)
  , fromIntegral (txIdx .&. 0xff)
  , fromIntegral ((outIdx `shiftR` 8) .&. 0xff)
  , fromIntegral (outIdx .&. 0xff)
  ]
{-# INLINE mkShortChannelId #-}

-- | Extract block height from short channel ID (first 3 bytes, big-endian).
scidBlockHeight :: ShortChannelId -> Word32
scidBlockHeight (ShortChannelId bs) =
  let b0 = fromIntegral (BS.index bs 0)
      b1 = fromIntegral (BS.index bs 1)
      b2 = fromIntegral (BS.index bs 2)
  in  (b0 `shiftL` 16) .|. (b1 `shiftL` 8) .|. b2
{-# INLINE scidBlockHeight #-}

-- | Extract transaction index from short channel ID (bytes 3-5, big-endian).
scidTxIndex :: ShortChannelId -> Word32
scidTxIndex (ShortChannelId bs) =
  let b3 = fromIntegral (BS.index bs 3)
      b4 = fromIntegral (BS.index bs 4)
      b5 = fromIntegral (BS.index bs 5)
  in  (b3 `shiftL` 16) .|. (b4 `shiftL` 8) .|. b5
{-# INLINE scidTxIndex #-}

-- | Extract output index from short channel ID (last 2 bytes, big-endian).
scidOutputIndex :: ShortChannelId -> Word16
scidOutputIndex (ShortChannelId bs) =
  let b6 = fromIntegral (BS.index bs 6)
      b7 = fromIntegral (BS.index bs 7)
  in  (b6 `shiftL` 8) .|. b7
{-# INLINE scidOutputIndex #-}

-- | Format short channel ID as human-readable string.
--
-- Uses the standard "block x tx x output" notation.
--
-- >>> formatScid (mkShortChannelId 539268 845 1)
-- "539268x845x1"
formatScid :: ShortChannelId -> String
formatScid scid =
  show (scidBlockHeight scid) ++ "x" ++
  show (scidTxIndex scid) ++ "x" ++
  show (scidOutputIndex scid)
{-# INLINE formatScid #-}

-- | Channel ID (32 bytes).
newtype ChannelId = ChannelId { getChannelId :: ByteString }
  deriving (Eq, Show, Generic)

instance NFData ChannelId

-- | Smart constructor for ChannelId. Returns Nothing if not 32 bytes.
channelId :: ByteString -> Maybe ChannelId
channelId !bs
  | BS.length bs == channelIdLen = Just (ChannelId bs)
  | otherwise = Nothing
{-# INLINE channelId #-}

-- Cryptographic types ---------------------------------------------------------

-- | Signature (64 bytes).
newtype Signature = Signature { getSignature :: ByteString }
  deriving (Eq, Show, Generic)

instance NFData Signature

-- | Smart constructor for Signature. Returns Nothing if not 64 bytes.
signature :: ByteString -> Maybe Signature
signature !bs
  | BS.length bs == signatureLen = Just (Signature bs)
  | otherwise = Nothing
{-# INLINE signature #-}

-- | Compressed public key (33 bytes).
newtype Point = Point { getPoint :: ByteString }
  deriving (Eq, Show, Generic)

instance NFData Point

-- | Smart constructor for Point. Returns Nothing if not 33 bytes.
point :: ByteString -> Maybe Point
point !bs
  | BS.length bs == pointLen = Just (Point bs)
  | otherwise = Nothing
{-# INLINE point #-}

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

-- Routing parameters ----------------------------------------------------------

-- | CLTV expiry delta.
type CltvExpiryDelta = Word16

-- | Base fee in millisatoshis.
type FeeBaseMsat = Word32

-- | Proportional fee in millionths.
type FeeProportionalMillionths = Word32

-- | Minimum HTLC value in millisatoshis.
type HtlcMinimumMsat = Word64

-- | Maximum HTLC value in millisatoshis.
type HtlcMaximumMsat = Word64
