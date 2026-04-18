# ppad-bolt7

A pure Haskell implementation of
[BOLT #7](https://github.com/lightning/bolts/blob/master/07-routing-gossip.md)
(Lightning Network routing gossip protocol).

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  >
  > -- import qualified
  > import qualified Lightning.Protocol.BOLT7 as B7
  >
  > -- decode a gossip_timestamp_filter from wire bytes (after msg type)
  > -- wire contains: 32-byte chain_hash, 4-byte first_timestamp,
  > --                4-byte timestamp_range
  > case B7.decodeGossipTimestampFilter wire of
  >   Left err -> print err
  >   Right (msg, rest) -> print (B7.gossipFilterFirstTimestamp msg)
  1609459200
  >
  > -- construct and encode a gossip_timestamp_filter
  > let msg = B7.GossipTimestampFilter B7.mainnetChainHash 1609459200 86400
  > let encoded = B7.encodeGossipTimestampFilter msg
  >
  > -- validate a channel_announcement before processing
  > case B7.validateChannelAnnouncement announcement of
  >   Left B7.ValidateNodeIdOrdering -> putStrLn "invalid node ordering"
  >   Right () -> putStrLn "valid"
  >
  > -- compute the hash for signature verification
  > let sigHash = B7.channelAnnouncementHash encodedAnnouncement
  > -- verify signatures with ppad-secp256k1 (separate library)
```

## Overview

ppad-bolt7 provides types and codecs for BOLT #7 gossip messages:

* `channel_announcement` (256)
* `node_announcement` (257)
* `channel_update` (258)
* `announcement_signatures` (259)
* `query_short_channel_ids` (261)
* `reply_short_channel_ids_end` (262)
* `query_channel_range` (263)
* `reply_channel_range` (264)
* `gossip_timestamp_filter` (265)

## Documentation

Haddock documentation is available at
[docs.ppad.tech/bolt7](https://docs.ppad.tech/bolt7).

## Security

This is a pre-release version of the library and makes no claims about
security whatsoever.

## Development

A Nix development shell is provided via flake. Enter it with:

```
$ nix develop
```

Then use `cabal` as usual:

```
$ cabal build
$ cabal test
$ cabal bench
```

