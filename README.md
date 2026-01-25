# ppad-bolt7

A pure Haskell implementation of
[BOLT #7](https://github.com/lightning/bolts/blob/master/07-routing-gossip.md)
(Lightning Network routing gossip protocol).

## Synopsis

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

## Security

This library has received no security audit. Use at your own risk.
