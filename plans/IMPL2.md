# IMPL2: Refactoring and Type Safety Improvements

## Overview

Internal refactoring to reduce code duplication and improve type safety.
These changes are purely internal - no API changes to exported functions.

## Phase 1: Decoder Combinator (Codec.hs)

**Goal:** Extract common decoder pattern into reusable combinator.

**Files:** `lib/Lightning/Protocol/BOLT7/Codec.hs`

**Current state:** Lines 140-211 contain 8 nearly-identical decoders:

```haskell
decodeSignature bs = do
  (bytes, rest) <- decodeBytes signatureLen bs
  case signature bytes of
    Nothing -> Left DecodeInvalidSignature
    Just s  -> Right (s, rest)
```

**Tasks:**
1. Add `decodeFixed` combinator in "Primitive helpers" section (~line 90):
   ```haskell
   decodeFixed :: Int -> DecodeError -> (ByteString -> Maybe a)
               -> ByteString -> Either DecodeError (a, ByteString)
   decodeFixed len err mkVal bs = do
     (bytes, rest) <- decodeBytes len bs
     case mkVal bytes of
       Nothing -> Left err
       Just v  -> Right (v, rest)
   {-# INLINE decodeFixed #-}
   ```

2. Rewrite type-specific decoders (lines 140-211) using `decodeFixed`:
   ```haskell
   decodeSignature :: ByteString -> Either DecodeError (Signature, ByteString)
   decodeSignature = decodeFixed signatureLen DecodeInvalidSignature signature
   {-# INLINE decodeSignature #-}
   ```

3. Apply to all 8 decoders:
   - decodeSignature
   - decodeChainHash
   - decodeShortChannelId
   - decodeChannelId
   - decodeNodeId
   - decodePoint
   - decodeRgbColor
   - decodeAlias

**Verification:**
- `cabal build` succeeds
- `cabal test` passes (roundtrip tests verify correctness)

**Dependencies:** None (can start immediately)

---

## Phase 2: Double-SHA256 Helper (Hash.hs)

**Goal:** Extract repeated double-hash pattern.

**Files:** `lib/Lightning/Protocol/BOLT7/Hash.hs`

**Current state:** Lines 45, 56, 67 all contain:
```haskell
SHA256.hash (SHA256.hash payload)
```

**Tasks:**
1. Add internal helper after imports (~line 32):
   ```haskell
   -- | Double SHA-256 hash (used for Lightning message signing).
   doubleSha256 :: ByteString -> ByteString
   doubleSha256 = SHA256.hash . SHA256.hash
   {-# INLINE doubleSha256 #-}
   ```

2. Update `channelAnnouncementHash` (line 45):
   ```haskell
   in  doubleSha256 payload
   ```

3. Update `nodeAnnouncementHash` (line 56):
   ```haskell
   in  doubleSha256 payload
   ```

4. Update `channelUpdateHash` (line 67):
   ```haskell
   in  doubleSha256 payload
   ```

**Verification:**
- `cabal build` succeeds
- `cabal test` passes

**Dependencies:** None (can start immediately)

---

## Phase 3: Length-Prefixed Encoding Helper (Codec.hs)

**Goal:** Extract repeated length-prefix encoding pattern.

**Files:** `lib/Lightning/Protocol/BOLT7/Codec.hs`

**Current state:** Pattern appears in 5 encoders:
```haskell
Prim.encodeU16 (fromIntegral $ BS.length features) <> features
```

Locations:
- Line 273-274 (encodeChannelAnnouncement)
- Line 326-327 (encodeNodeAnnouncement, features)
- Line 332-333 (encodeNodeAnnouncement, addresses)
- Line 479 (encodeQueryShortChannelIds)
- Line 559 (encodeReplyChannelRange)

**Tasks:**
1. Add `encodeLenPrefixed` helper in "Primitive helpers" section (~line 90):
   ```haskell
   -- | Encode with u16 length prefix.
   encodeLenPrefixed :: ByteString -> ByteString
   encodeLenPrefixed bs =
     Prim.encodeU16 (fromIntegral $ BS.length bs) <> bs
   {-# INLINE encodeLenPrefixed #-}
   ```

2. Update all 5 locations to use the helper.

**Verification:**
- `cabal build` succeeds
- `cabal test` passes

**Dependencies:** None (can start immediately)

---

## Phase 4: Routing Parameter Newtypes (Types.hs)

**Goal:** Replace type aliases with newtypes to prevent accidental mixing.

**Files:**
- `lib/Lightning/Protocol/BOLT7/Types.hs`
- `lib/Lightning/Protocol/BOLT7/Messages.hs`
- `lib/Lightning/Protocol/BOLT7/Codec.hs`

**Current state (Types.hs:407-419):**
```haskell
type CltvExpiryDelta = Word16
type FeeBaseMsat = Word32
type FeeProportionalMillionths = Word32
type HtlcMinimumMsat = Word64
type HtlcMaximumMsat = Word64
```

**Tasks:**
1. Convert type aliases to newtypes in Types.hs:
   ```haskell
   newtype CltvExpiryDelta = CltvExpiryDelta
     { getCltvExpiryDelta :: Word16 }
     deriving (Eq, Ord, Show, Generic)

   instance NFData CltvExpiryDelta

   newtype FeeBaseMsat = FeeBaseMsat
     { getFeeBaseMsat :: Word32 }
     deriving (Eq, Ord, Show, Generic)

   instance NFData FeeBaseMsat

   -- etc. for all 5 types
   ```

2. Update exports in Types.hs to include constructors and accessors.

3. Update ChannelUpdate record in Messages.hs - no changes needed,
   field types remain the same names.

4. Update Codec.hs encode/decode:
   - `encodeChannelUpdate`: wrap primitives in constructors
   - `decodeChannelUpdate`: use constructors when building record

5. Update Validate.hs if needed (comparison of HtlcMinimumMsat vs
   HtlcMaximumMsat may need unwrapping).

**Verification:**
- `cabal build` succeeds
- `cabal test` passes

**Dependencies:** None (can start immediately)

**Note:** This is the most invasive change. Consider whether the type
safety benefit outweighs the additional unwrapping boilerplate.

---

## Phase 5: Channel Flags ADT (Optional)

**Goal:** Replace raw Word8 flags with structured ADT.

**Files:**
- `lib/Lightning/Protocol/BOLT7/Types.hs`
- `lib/Lightning/Protocol/BOLT7/Messages.hs`
- `lib/Lightning/Protocol/BOLT7/Codec.hs`

**Current state (Messages.hs:129-130):**
```haskell
chanUpdateMsgFlags  :: !Word8
chanUpdateChanFlags :: !Word8
```

**Tasks:**
1. Add flag types to Types.hs:
   ```haskell
   -- | Message flags for channel_update.
   data MessageFlags = MessageFlags
     { mfHtlcMaximumPresent :: !Bool
     }
     deriving (Eq, Show, Generic)

   instance NFData MessageFlags

   -- | Channel flags for channel_update.
   data ChannelFlags = ChannelFlags
     { cfDirection :: !Bool  -- ^ True = node_id_2 is origin
     , cfDisabled  :: !Bool  -- ^ True = channel disabled
     }
     deriving (Eq, Show, Generic)

   instance NFData ChannelFlags
   ```

2. Add encode/decode helpers:
   ```haskell
   encodeMessageFlags :: MessageFlags -> Word8
   decodeMessageFlags :: Word8 -> MessageFlags

   encodeChannelFlags :: ChannelFlags -> Word8
   decodeChannelFlags :: Word8 -> ChannelFlags
   ```

3. Update ChannelUpdate in Messages.hs to use new types.

4. Update Codec.hs to use encode/decode helpers.

**Verification:**
- `cabal build` succeeds
- `cabal test` passes

**Dependencies:** None (can start immediately)

**Note:** This is optional. The benefit is making flag semantics explicit,
but it adds complexity. Defer if unclear.

---

## Phase 6: Complete validateReplyChannelRange (Validate.hs)

**Goal:** Implement the stubbed validation function.

**Files:** `lib/Lightning/Protocol/BOLT7/Validate.hs`

**Current state (lines 118-124):**
```haskell
validateReplyChannelRange :: ReplyChannelRange -> Either ValidationError ()
validateReplyChannelRange _msg = do
  Right ()  -- stubbed
```

**Tasks:**
1. Import `decodeShortChannelIdList` from Codec module.

2. Implement ascending order check:
   ```haskell
   validateReplyChannelRange msg = do
     case decodeShortChannelIdList (replyRangeData msg) of
       Left _ -> Right ()  -- Can't decode, skip validation
       Right scids -> checkAscending scids
     where
       checkAscending [] = Right ()
       checkAscending [_] = Right ()
       checkAscending (a:b:rest)
         | getShortChannelId a < getShortChannelId b = checkAscending (b:rest)
         | otherwise = Left ValidateScidNotAscending
   ```

**Verification:**
- `cabal build` succeeds
- `cabal test` passes
- Add test case for non-ascending SCIDs

**Dependencies:** None (can start immediately)

---

## Dependency Graph

```
Phase 1 (decoder combinator)  \
                               \
Phase 2 (double-sha256)  -------+---> All can proceed in parallel
                               /
Phase 3 (len-prefixed)  ------/
                             /
Phase 4 (routing newtypes) -/

Phase 5 (channel flags) -----> Optional, defer if needed

Phase 6 (validation) --------> Independent
```

All phases are independent and can be executed in parallel.

---

## Complexity and Risk

| Phase | Complexity | Risk   | Lines Changed | Notes                    |
|-------|------------|--------|---------------|--------------------------|
| 1     | Low        | Low    | ~80           | Pure extraction          |
| 2     | Low        | Low    | ~10           | Trivial                  |
| 3     | Low        | Low    | ~15           | Pure extraction          |
| 4     | Medium     | Medium | ~50           | Touches multiple modules |
| 5     | Medium     | Medium | ~40           | Optional, defer if unsure|
| 6     | Low        | Low    | ~15           | Completes stub           |

---

## Execution Order Recommendation

1. **First batch (parallel):** Phases 1, 2, 3, 6
   - Low risk, immediate benefit
   - Can be done by separate agents concurrently

2. **Second batch:** Phase 4
   - Review after first batch merges
   - Evaluate if type safety benefit justifies churn

3. **Defer:** Phase 5
   - Nice-to-have but adds complexity
   - Revisit if flag handling becomes error-prone
