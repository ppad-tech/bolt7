# IMPL1: Core BOLT #7 Implementation

## Overview

Complete implementation of BOLT #7 routing gossip protocol with full
encode/decode support, validation, and signature verification.

## Phase 1: Fix Skeleton and Verify Build

**Goal:** Ensure current skeleton compiles and tests pass.

**Tasks:**
1. Enter nix develop shell
2. Run `cabal build` and fix any compilation errors
3. Run `cabal test` and verify skeleton tests pass
4. Ensure benchmarks compile

**Dependencies:** None (can start immediately)

## Phase 2: Complete Types Module

**Goal:** Ensure all types are correct and complete.

**Tasks:**
1. Verify ShortChannelId parsing functions (block height, tx index, output)
2. Add `mkShortChannelId` constructor from components
3. Add human-readable SCID formatting (`539268x845x1` style)
4. Add Bitcoin mainnet chain hash constant
5. Verify Address type covers all descriptor types:
   - Type 1: IPv4 (4 bytes + 2 port)
   - Type 2: IPv6 (16 bytes + 2 port)
   - Type 4: Tor v3 (35 bytes + 2 port)
   - Type 5: DNS (1 len + hostname + 2 port)
6. Add Ord instances where needed for lexicographic comparison

**Dependencies:** Phase 1

## Phase 3: Complete Codec Module

**Goal:** Fully correct encode/decode for all 9 message types.

**Tasks:**
1. Review and fix `channel_announcement` codec:
   - Signature order: node_sig_1, node_sig_2, bitcoin_sig_1, bitcoin_sig_2
   - Features are u16-length-prefixed
   - Verify field order matches spec exactly

2. Review and fix `node_announcement` codec:
   - Features are u16-length-prefixed (flen)
   - Addresses are u16-length-prefixed (addrlen)
   - Verify address type encoding (type byte + data + port)
   - Handle unknown address types gracefully (skip by length)

3. Review and fix `channel_update` codec:
   - `message_flags` bit 0 MUST be 1 (htlc_maximum_msat always present)
   - Remove Maybe wrapper from htlc_maximum_msat (always required now)
   - Verify field order and sizes

4. Review and fix `announcement_signatures` codec:
   - Straightforward fixed-size fields

5. Review and fix query message codecs:
   - `query_short_channel_ids`: chain_hash + u16-prefixed encoded_short_ids + TLV
   - `reply_short_channel_ids_end`: chain_hash + full_information byte
   - `query_channel_range`: chain_hash + first_blocknum + number_of_blocks + TLV
   - `reply_channel_range`: chain_hash + first/num blocks + sync_complete +
     u16-prefixed encoded_short_ids + TLV
   - `gossip_timestamp_filter`: chain_hash + first_timestamp + timestamp_range

6. Add TLV type definitions:
   - `query_flags` (type 1) for query_short_channel_ids
   - `query_option` (type 1) for query_channel_range
   - `timestamps_tlv` (type 1) and `checksums_tlv` (type 3) for reply_channel_range

7. Add encoded_short_ids helpers:
   - `encodeShortChannelIds :: [ShortChannelId] -> ByteString`
   - `decodeShortChannelIds :: ByteString -> Either DecodeError [ShortChannelId]`
   - Encoding type 0 only (uncompressed, ascending order)

**Dependencies:** Phase 2

## Phase 4: Signature Hash and Checksum Computation

**Goal:** Support signature verification and checksums for gossip messages.

**Tasks:**
1. Add `channelAnnouncementHash` function:
   - Double-SHA256 of message starting at offset 256 (after 4 signatures)
   - Returns 32-byte hash for signature verification

2. Add `nodeAnnouncementHash` function:
   - Double-SHA256 of message starting after signature field

3. Add `channelUpdateHash` function:
   - Double-SHA256 of message starting after signature field

4. Add internal `Lightning.Protocol.BOLT7.CRC32C` helper module:
   - Implement CRC32C (Castagnoli polynomial 0x1EDC6F41) per RFC3720
   - Pure Haskell implementation, no external dependencies
   - Can be refactored into ppad-crc32c later if needed elsewhere

5. Add `channelUpdateChecksum` function:
   - CRC32C of channel_update excluding signature and timestamp fields
   - Used in reply_channel_range checksums_tlv

**Dependencies:** Phase 3

## Phase 5: Validation Module

**Goal:** Implement message validation rules from spec.

**Tasks:**
1. Create `Lightning.Protocol.BOLT7.Validate` module

2. Add `validateChannelAnnouncement`:
   - node_id_1 < node_id_2 (lexicographic ordering)
   - All signatures present and correct length
   - Feature bits validation (unknown even bits = error)

3. Add `validateNodeAnnouncement`:
   - Timestamp must be valid
   - Feature bits validation
   - Address list validation (no duplicate type 5 DNS entries)
   - Alias is valid UTF-8 (or at least doesn't crash)

4. Add `validateChannelUpdate`:
   - message_flags bit 0 must be 1
   - htlc_minimum_msat <= htlc_maximum_msat
   - Timestamp validation

5. Add `validateQueryChannelRange`:
   - first_blocknum + number_of_blocks doesn't overflow

6. Add `validateReplyChannelRange`:
   - Encoded short_channel_ids are in ascending order
   - Timestamp/checksum counts match SCID count

**Dependencies:** Phase 3

## Phase 6: Tests

**Goal:** Comprehensive test coverage.

**Tasks:**
1. Add encode/decode roundtrip tests for all message types
2. Add validation tests (valid and invalid cases)
3. Add property tests:
   - Roundtrip: decode(encode(x)) == x
   - SCID component extraction consistency
   - Lexicographic ordering preservation
4. Add edge case tests:
   - Empty feature bits
   - Empty address lists
   - Maximum-size messages
   - Boundary values for numeric fields

**Dependencies:** Phases 3, 5

## Phase 7: Benchmarks

**Goal:** Performance baselines for all operations.

**Tasks:**
1. Complete criterion benchmarks for all encode/decode operations
2. Complete weigh benchmarks for allocation tracking
3. Add benchmarks for:
   - Signature hash computation
   - SCID list encoding/decoding
   - Validation functions

**Dependencies:** Phases 3, 4, 5

## Phase 8: Documentation

**Goal:** Complete Haddock documentation.

**Tasks:**
1. Document all exported types with examples
2. Document all exported functions with examples
3. Add module-level documentation explaining BOLT #7 protocol
4. Verify `cabal haddock` produces clean output

**Dependencies:** All previous phases

---

## Dependency Graph

```
Phase 1 (build fix)
    |
    v
Phase 2 (types)
    |
    v
Phase 3 (codec)
    |
    +---> Phase 4 (sig hash) --+
    |                          |
    +---> Phase 5 (validate) --+
                               |
                               v
                        Phase 6 (tests)
                               |
                               v
                        Phase 7 (benchmarks)
                               |
                               v
                        Phase 8 (docs)
```

Phases 4 and 5 can proceed in parallel after Phase 3.

---

## Open Questions

1. **Signature verification:** This library provides hash computation only.
   Actual verification requires ppad-secp256k1 and is left to the caller.
   Should we add optional verification helpers?

2. **Zlib compression:** Encoding type 1 for encoded_short_ids uses zlib.
   Spec says MUST NOT use, but we may need to decode it. Add zlib dependency
   or reject?

## Resolved

1. **CRC32C:** Implement in internal helper module
   `Lightning.Protocol.BOLT7.CRC32C`. Refactor to ppad-crc32c later if
   needed elsewhere.

---

## Estimated Complexity

| Phase | Complexity | Notes |
|-------|------------|-------|
| 1     | Low        | Just build fixes |
| 2     | Low        | Minor type additions |
| 3     | Medium     | Core codec work |
| 4     | Medium     | Requires careful hash construction |
| 5     | Medium     | Many validation rules |
| 6     | Medium     | Comprehensive test writing |
| 7     | Low        | Mechanical benchmark setup |
| 8     | Low        | Documentation pass |
