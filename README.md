# ppad-bolt7

[![](https://img.shields.io/hackage/v/ppad-bolt7?color=blue)](https://hackage.haskell.org/package/ppad-bolt7)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-bolt7-lightblue)](https://docs.ppad.tech/bolt7)

A pure Haskell implementation of Lightning Network routing gossip,
[BOLT #7][bolt7]: the `channel_announcement`, `node_announcement`,
`channel_update` and `announcement_signatures` messages, the gossip
queries, signature digests and verification, and `channel_update`
checksums.

Decoding keeps every byte of a message (unassigned flag bits, unknown
address descriptors, extension TLV records), so a decoded message
re-encodes exactly and its signatures still verify.

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  > import qualified Data.ByteString as BS
  > import qualified Data.ByteString.Base16 as B16
  > import qualified Lightning.Protocol.BOLT1 as BOLT1
  > import Lightning.Protocol.BOLT7
  >
  > -- a channel_update payload from mainnet gossip
  > :{
  | let Just wire = B16.decode $ mconcat
  |       [ "1c3f0859cb3f4494919c3dcc80d22bbf1e25e378bd95f071530833c2371c33d7"
  |       , "0766012ebef1cd79b299a54d0d29bea2ef6feca1d6cfa188864c95d7eea7e9a4"
  |       , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
  |       , "0cf884000bb000016a0a1c760100006400000000000003e80000000000001388"
  |       , "00000000b1069a80fdd9030800000000ffffffed" ]
  | :}
  > let Right upd = decode_channel_update wire
  > format_short_channel_id (cu_short_channel_id upd)
  "850052x2992x1"
  > channel_flags_direction (cu_channel_flags upd)
  NodeOne
  > cu_fee_proportional_millionths upd
  5000
  >
  > -- extension records (here LND's inbound fees) are kept, so the
  > -- update re-encodes exactly and its signature verifies
  > BOLT1.un_tlv_stream (cu_tlvs upd)
  [TlvRecord {tlv_type = 55555, tlv_value = "\NUL\NUL\NUL\NUL\255\255\255\237"}]
  > encode_channel_update upd == wire
  True
  > :{
  | let Just node_1 = BOLT1.point =<< B16.decode
  |       "02bb5e47ee26aca1654ce092d9d06eac15d953210c737f2220e2d7e54c4f108677"
  | :}
  > verify_channel_update node_1 upd
  True
  > verify_channel_update node_1 upd { cu_fee_base_msat = 1 }
  False
  >
  > -- dispatch on a message type, as split off by BOLT1.decode_envelope
  > fmap message_type (decode_message 258 wire)
  Right 258
  >
  > -- gossip queries have typed fields
  > let Just chain = BOLT1.chain_hash (BS.replicate 32 0)
  > let opt = Just (query_option [WantTimestamps])
  > let q = QueryChannelRange chain 800000 1000 opt BOLT1.empty_tlv_stream
  > let Right payload = encode_query_channel_range q
  > decode_query_channel_range payload == Right q
  True
  >
  > -- the deprecated zlib encoding is rejected
  > decode_reply_channel_range (BS.replicate 41 0 <> "\NUL\STX\SOHx")
  Left (DecodeUnknownEncoding 1)
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/bolt7](https://docs.ppad.tech/bolt7).

## Specification

This library implements BOLT #7 as of lightning/bolts commit
[1aadb719][spec] (2026-09-21); a copy lives in `etc/`. Fundamental
types come from [ppad-bolt1][bolt1] and feature vectors from
[ppad-bolt9][bolt9].

Not implemented:

* The zlib encoding (type 1) of `encoded_short_ids` and its kin, which
  the specification forbids; messages using it fail to decode.
* Signing. The digest functions give what a node signs, with e.g.
  [ppad-secp256k1][secp], to create announcements and updates.
* Rules that need state or chain access: timestamp ordering,
  blacklisting, funding output checks, chain hash filtering, rebroadcast
  and pruning policy. Feature-bit rules are left to ppad-bolt9.

Checksums cover a `channel_update`'s extension records, per the
specification's wording.

## Performance

The aim is best-in-class performance for pure Haskell code. Run the
benchmarks with:

```
$ cabal bench
```

## Security

This library has received no security audit. Decoding validates every
field the specification constrains, and rejects unknown even TLV types.
Signature verification uses ppad-secp256k1 and, like libsecp256k1,
accepts only low-s signatures.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-bolt7
```

to get a REPL for the main library.

[bolt7]: https://github.com/lightning/bolts/blob/master/07-routing-gossip.md
[bolt1]: https://git.ppad.tech/bolt1
[bolt9]: https://git.ppad.tech/bolt9
[secp]: https://git.ppad.tech/secp256k1
[spec]: https://github.com/lightning/bolts/tree/1aadb719b4007c4cea0ba6e36b08c4fb53788dee
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
