{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ViewPatterns #-}
-- Constructor specialisation can clone recursive workers before measure
-- specialisation, leaving runtime dictionary arguments in the clones.
-- Use the explicit SPECIALIZE pragmas below instead.
{-# OPTIONS_GHC -fno-spec-constr #-}

-- |
-- Module      : Data.Text.NanoRope.Internal
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- B-tree internals, chunk scans and invariant checks, exposed for tests and
-- benchmarks with __no API stability guarantees__. Applications should use
-- "Data.Text.NanoRope" or "Data.Text.NanoRope.Measured".
--
-- = Representation
--
-- Leaves hold exact-size unpinned UTF-8 arrays of at most 'maxChunk' bytes,
-- split at code point boundaries; inner nodes hold up to 'maxChildren'
-- children. Every node caches its subtree's t'Metrics' (packed into 64 bits
-- in leaves), so seeking reads at most 'maxChildren' headers per level and an
-- edit copies one leaf and its path. Nodes are unlifted: reading a child
-- never needs a thunk check.
--
-- Hot paths stay allocation-free only through strict arguments, unpacked
-- result records and careful helper placement: check benchmark allocation
-- after changing them.
--
-- = Scanning
--
-- Slices of at least 32 bytes are scanned in C (SSE2/AVX2 on x86-64, portable
-- C elsewhere), shorter ones in Haskell 8 bytes at a time. @-f -simd@ uses
-- Haskell only. See 'kernels'.
module Data.Text.NanoRope.Internal
  ( -- * Types
    Rope (.., Rope)
  , Node (.., Leaf)
  , Lazy (..)
  , Children
  , sizeofChildren
  , indexChildren
  , Measure (..)
  , Metrics (..)
  , Unit (..)
  , Position (..)
  , count
  , subMetrics
  , PackedMetrics
  , packMetrics
  , unpackMetrics

    -- * Tuning constants
  , maxChunk
  , minChunk
  , maxChildren
  , minChildren
  , maxPending
  , outputBuffer

    -- * Construction
  , empty
  , singleton
  , fromText
  , fromLazyText

    -- * Deconstruction
  , toText
  , toLazyText
  , toString
  , toChunks
  , foldrChunks
  , foldlChunks'
  , chunkAt

    -- * Output
  , hPutUtf8
  , writeFileUtf8

    -- * Queries
  , null
  , length
  , lineCount
  , metrics
  , measure

    -- * Combining and breaking
  , append
  , splitAt
  , take
  , drop
  , slice
  , sliceText

    -- * Editing
  , insert
  , delete
  , replace

    -- * Lines
  , getLine
  , lines

    -- * Converting between units
  , metricsAt
  , convert

    -- * Positions
  , splitAtPosition
  , metricsAtPosition
  , metricsAtLineAndPosition
  , metricsToPosition
  , offsetToPosition
  , positionToOffset

    -- * Custom measures
  , splitWhere
  , metricsWhere
  , remeasure

    -- * Debugging
  , invariants
  , height

    -- * Chunk primitives
  , sliceMetrics
  , offsetInChunk
  , chunkText
  , ChunkLine (..)
  , Kernels (..)
  , kernels
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Control.Monad.ST (RealWorld)
import Data.Bits (complement, unsafeShiftL, unsafeShiftR, xor, (.&.), (.|.))
import Data.Kind (Type)
import qualified Data.List as L
import Data.Primitive.ByteArray
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import qualified Data.Text.Lazy as TL
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import GHC.Exts
  ( Int (..)
  , Int#
  , SmallArray#
  , SmallMutableArray#
  , TYPE
  , UnliftedType
  , cloneSmallArray#
  , copySmallArray#
  , indexSmallArray#
  , indexWord8ArrayAsWord64#
  , newSmallArray#
  , runRW#
  , sizeofSmallArray#
  , thawSmallArray#
  , unsafeFreezeSmallArray#
  , writeSmallArray#
  , (-#)
  )
import GHC.ST (ST (..))
import GHC.Word (Word64 (..))
import System.IO (Handle, IOMode (WriteMode), hPutBuf, withBinaryFile)
#ifdef NANO_ROPE_SIMD
import System.IO.Unsafe (unsafeDupablePerformIO)
#endif
import Prelude hiding (drop, getLine, length, lines, null, splitAt, take)

------------------------------------------------------------------------------
-- Tuning constants

-- | Maximum leaf size in bytes. Must not exceed @65535 / maxChildren@:
-- @sumMetrics@ adds packed 16-bit leaf counts without unpacking them.
maxChunk :: Int

-- | Maximum number of children of an inner node.
maxChildren :: Int
#ifdef NANO_ROPE_SMALL
-- Small nodes exercise deep trees with short test inputs. Six children
-- allow non-root inner nodes with two children to be undersized.
maxChunk = 16
maxChildren = 6
#else
maxChunk = 512
maxChildren = 16
#endif

-- | Minimum number of bytes in a leaf, unless the leaf is the root.
--
-- A quarter (rather than half) of 'maxChunk' gives hysteresis: a leaf that
-- was just split in two does not merge back after deleting a character.
minChunk :: Int
minChunk = maxChunk `quot` 4

-- | Minimum number of children of an inner node, unless it is the root
-- (which has at least two).
minChildren :: Int
minChildren = maxChildren `quot` 2

-- | Maximum buffered input in bytes; see 'Typing'. Each keystroke copies
-- the buffer, so it stays smaller than a chunk.
maxPending :: Int
maxPending = maxChunk `quot` 4

-- | UTF-8 output buffer size in bytes: 32 KiB in the default build.
-- Holds whole chunks and batches writes to reduce per-chunk I/O overhead.
outputBuffer :: Int
#ifdef NANO_ROPE_SMALL
-- A small buffer makes tests exercise repeated flushes.
outputBuffer = 4 * maxChunk
#else
outputBuffer = 64 * maxChunk
#endif

------------------------------------------------------------------------------
-- Metrics

-- | Built-in counts cached at every tree node. The metrics of a rope's
-- prefix also describe its endpoint in all four units; see 'metricsAt'.
data Metrics = Metrics
  { bytes :: {-# UNPACK #-} !Int
  -- ^ UTF-8 bytes.
  , chars :: {-# UNPACK #-} !Int
  -- ^ Unicode code points, not grapheme clusters or display columns.
  , utf16Units :: {-# UNPACK #-} !Int
  -- ^ UTF-16 code units, the default position unit in LSP.
  , newlines :: {-# UNPACK #-} !Int
  -- ^ Line feeds (@\\n@).
  }
  deriving (Eq, Show)

instance Semigroup Metrics where
  Metrics b1 c1 u1 l1 <> Metrics b2 c2 u2 l2 =
    Metrics (b1 + b2) (c1 + c2) (u1 + u2) (l1 + l2)
  {-# INLINE (<>) #-}

instance Monoid Metrics where
  mempty = Metrics 0 0 0 0
  {-# INLINE mempty #-}

instance NFData Metrics where
  rnf !_ = ()

-- | Componentwise subtraction.
subMetrics :: Metrics -> Metrics -> Metrics
subMetrics (Metrics b1 c1 u1 l1) (Metrics b2 c2 u2 l2) =
  Metrics (b1 - b2) (c1 - c2) (u1 - u2) (l1 - l2)
{-# INLINE subMetrics #-}

-- | A unit for offsets and lengths. Public offset operations clamp negative
-- offsets to the start and offsets beyond the document to its end.
data Unit
  = -- | UTF-8 bytes. An offset inside a code point is rounded down to
    -- the start of that code point.
    Bytes
  | -- | Unicode code points, not grapheme clusters or display columns.
    Chars
  | -- | UTF-16 code units. An offset between the two halves of a surrogate
    -- pair is rounded down to the start of that code point.
    Utf16
  | -- | Zero-based line starts. Offset zero is the document start; offset
    -- @n > 0@ is just after the @n@-th @\\n@. Length in this unit counts
    -- line feeds, not lines. A lone @\\r@ does not start a line.
    Lines
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Read the count for one unit from t'Metrics'.
count :: Unit -> Metrics -> Int
count Bytes = bytes
count Chars = chars
count Utf16 = utf16Units
count Lines = newlines
{-# INLINE count #-}

-- | A zero-based line and column. Position functions take the column's
-- unit separately; columns are not necessarily display widths.
data Position = Position
  { posLine :: !Int
  -- ^ Zero-based line index.
  , posColumn :: !Int
  -- ^ Offset from the line start, in the unit supplied to the operation.
  }
  deriving (Eq, Ord, Show)

instance NFData Position where
  rnf !_ = ()

------------------------------------------------------------------------------
-- Custom measures

-- | A monoidal summary of text, cached at every node beside t'Metrics'.
-- Chunk boundaries can fall between any two code points, so 'measureChunk'
-- must be a monoid homomorphism:
--
-- > measureChunk (x <> y) == measureChunk x <> measureChunk y
-- > measureChunk mempty   == mempty
--
-- Context-sensitive measures track boundaries (e.g. counting @\\r\\n@ once
-- means remembering a leading @\\n@ and trailing @\\r@). Annotations are kept
-- in WHNF; use strict fields to avoid thunks.
class Monoid a => Measure a where
  -- | Measure a piece of text. The argument is a zero-copy view of at most
  -- 'maxChunk' bytes.
  measureChunk :: Text -> a

-- | No custom measure.
instance Measure () where
  measureChunk _ = ()
  {-# INLINE measureChunk #-}

instance (Measure a, Measure b) => Measure (a, b) where
  measureChunk t = (measureChunk t, measureChunk t)
  {-# INLINE measureChunk #-}

instance (Measure a, Measure b, Measure c) => Measure (a, b, c) where
  measureChunk t = (measureChunk t, measureChunk t, measureChunk t)
  {-# INLINE measureChunk #-}

------------------------------------------------------------------------------
-- The tree

-- | A rope annotated with a custom measure @a@ (@()@ for none). The v'Rope'
-- pattern exposes the tree after applying pending input; edits and metric
-- queries inspect the buffer directly.
data Rope a
  = -- | Tree, unit and end offset of the last insertion (negative: do not
    -- buffer the next one), and known free space in its leaf.
    Settled
      (Node a)
      !Unit
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | -- | Pending insertion: the lazily applied result, base tree, unit,
    -- offset, buffered text (<= 'maxPending' bytes), its packed metrics and
    -- remaining capacity. The next offset and total metrics are derived
    -- ('typingNext', 'metrics') to keep this small.
    Typing
      (Lazy a)
      (Node a)
      !Unit
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !PackedMetrics
      {-# UNPACK #-} !Int

-- | A lifted wrapper that can defer construction of an unlifted node.
-- Used for the pending tree update in 'Typing'.
data Lazy a = Lazy (Node a)

-- | Match or build a rope's tree. Matching applies pending input and
-- returns the evaluated, unlifted root.
pattern Rope :: Node a -> Rope a
pattern Rope root <- (rootOf -> root)
  where
    Rope root = Settled root Bytes (-1) 0

{-# COMPLETE Rope #-}

rootOf :: Rope a -> Node a
rootOf (Settled root _ _ _) = root
rootOf (Typing (Lazy root) _ _ _ _ _ _) = root
{-# INLINE rootOf #-}

-- | An unlifted B-tree node (never a thunk; annotations in WHNF). Both
-- constructors lead with metrics, all that seeking reads of skipped nodes.
type Node :: Type -> UnliftedType
data Node a
  = -- | Packed metrics (three words smaller), annotation, and a payload
    -- filling its whole array. Match or build with v'Leaf'.
    PackedLeaf
      {-# UNPACK #-} !PackedMetrics
      !a
      {-# UNPACK #-} !ByteArray
  | -- | Metrics, height (at least 1), annotation and children.
    Inner
      {-# UNPACK #-} !Metrics
      {-# UNPACK #-} !Int
      !a
      {-# UNPACK #-} !(Children a)

-- | A leaf: metrics, annotation and UTF-8 payload.
pattern Leaf :: Metrics -> a -> ByteArray -> Node a
pattern Leaf m a arr <- PackedLeaf (unpackMetrics -> !m) a arr
  where
    Leaf m a arr = PackedLeaf (packMetrics m) a arr

{-# COMPLETE Leaf, Inner #-}

-- | Four 16-bit counts in a 64-bit word, from low to high: bytes, code
-- points, UTF-16 code units, and line feeds. Supports up to 65535 bytes.
type PackedMetrics = Word64

-- | Pack metrics whose fields each fit in 16 bits. Does not check bounds.
packMetrics :: Metrics -> PackedMetrics
packMetrics (Metrics b c u l) =
  fromIntegral b
    .|. (fromIntegral c `unsafeShiftL` 16)
    .|. (fromIntegral u `unsafeShiftL` 32)
    .|. (fromIntegral l `unsafeShiftL` 48)
{-# INLINE packMetrics #-}

-- | Decode the four counts in a packed leaf metric.
unpackMetrics :: PackedMetrics -> Metrics
unpackMetrics w = Metrics (field 0) (field 16) (field 32) (field 48)
  where
    field s = fromIntegral ((w `unsafeShiftR` s) .&. 0xFFFF)
{-# INLINE unpackMetrics #-}

nodeMetrics :: Node a -> Metrics
nodeMetrics (Leaf m _ _) = m
nodeMetrics (Inner m _ _ _) = m
{-# INLINE nodeMetrics #-}

nodeAnn :: Node a -> a
nodeAnn (Leaf _ a _) = a
nodeAnn (Inner _ _ a _) = a
{-# INLINE nodeAnn #-}

nodeHeight :: Node a -> Int
nodeHeight Leaf{} = 0
nodeHeight (Inner _ h _ _) = h
{-# INLINE nodeHeight #-}

nodeBytes :: Node a -> Int
nodeBytes node = bytes (nodeMetrics node)
{-# INLINE nodeBytes #-}

nodeIsEmpty :: Node a -> Bool
nodeIsEmpty node = nodeBytes node == 0
{-# INLINE nodeIsEmpty #-}

-- | Construct an empty leaf. Unlifted values cannot be top-level constants,
-- so this allocates a leaf for each empty result.
emptyNode :: Monoid a => Node a
emptyNode = PackedLeaf 0 mempty emptyByteArray
{-# INLINE emptyNode #-}

------------------------------------------------------------------------------
-- Arrays of nodes

-- | A small array of unlifted child nodes.
data Children a = Children (SmallArray# (Node a))

data MutableChildren s a = MutableChildren (SmallMutableArray# s (Node a))

sizeofChildren :: Children a -> Int
sizeofChildren (Children cs) = I# (sizeofSmallArray# cs)
{-# INLINE sizeofChildren #-}

-- | Unchecked read; unlifted elements need no evaluation check.
indexChildren :: Children a -> Int -> Node a
indexChildren (Children cs) (I# i) = case indexSmallArray# cs i of (# node #) -> node
{-# INLINE indexChildren #-}

newChildren :: Int -> Node a -> ST s (MutableChildren s a)
newChildren (I# n) node = ST $ \s -> case newSmallArray# n node s of
  (# s', m #) -> (# s', MutableChildren m #)
{-# INLINE newChildren #-}

writeChildren :: MutableChildren s a -> Int -> Node a -> ST s ()
writeChildren (MutableChildren m) (I# i) node = ST $ \s -> (# writeSmallArray# m i node s, () #)
{-# INLINE writeChildren #-}

-- | Copy @cnt@ children from @src@ at @off@ to @dst@ at @d@.
copyChildren :: MutableChildren s a -> Int -> Children a -> Int -> Int -> ST s ()
copyChildren (MutableChildren dst) (I# d) (Children src) (I# off) (I# cnt) =
  ST $ \s -> (# copySmallArray# src off dst d cnt s, () #)
{-# INLINE copyChildren #-}

thawChildren :: Children a -> Int -> Int -> ST s (MutableChildren s a)
thawChildren (Children cs) (I# off) (I# cnt) = ST $ \s -> case thawSmallArray# cs off cnt s of
  (# s', m #) -> (# s', MutableChildren m #)
{-# INLINE thawChildren #-}

cloneChildren :: Children a -> Int -> Int -> Children a
cloneChildren (Children cs) (I# off) (I# cnt) = Children (cloneSmallArray# cs off cnt)
{-# INLINE cloneChildren #-}

runChildren :: (forall s. ST s (MutableChildren s a)) -> Children a
runChildren (ST build) =
  case runRW# (\s -> case build s of (# s', MutableChildren m #) -> unsafeFreezeSmallArray# m s') of
    (# _, cs #) -> Children cs
{-# INLINE runChildren #-}

-- | @n >= 1@ children, @f i@ at index @i@.
generateChildren :: Int -> (Int -> Node a) -> Children a
generateChildren n f = runChildren $ do
  m <- newChildren n (f 0)
  let go !i
        | i >= n = pure m
        | otherwise = writeChildren m i (f i) >> go (i + 1)
  go 1
{-# INLINE generateChildren #-}

------------------------------------------------------------------------------
-- Seeking

-- NOINLINE workers returning unpacked records keep results in registers; an
-- unboxed tuple of boxed Int/Metrics would allocate at every level.

-- | A child index and the total metrics of preceding children.
data Seek = Seek {-# UNPACK #-} !Int {-# UNPACK #-} !Metrics

-- | A child index and an offset within it.
data Sought = Sought {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | A child index, an offset within it, and the byte count before it.
data SoughtBytes = SoughtBytes {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | The first child whose cumulative count reaches @k@ (or the last), and
-- the metrics before it. Finds the index first (less register pressure),
-- then sums the shorter side.
seekChild :: Unit -> Int -> Metrics -> Children a -> Seek
seekChild !u !k !total !cs = case seekUnit u k (count u total) cs of
  Sought i _
    | 2 * i > n -> Seek i (total `subMetrics` sumMetrics cs i (n - i))
    | otherwise -> Seek i (sumMetrics cs 0 i)
  where
    n = sizeofChildren cs
{-# NOINLINE seekChild #-}

-- | Like 'seekChild', but return only the child index and relative offset.
-- Use the supplied total to search from the nearer end of the node.
seekUnit :: Unit -> Int -> Int -> Children a -> Sought
seekUnit !u !k !total !cs = case u of
  Bytes -> scan bytes
  Chars -> scan chars
  Utf16 -> scan utf16Units
  Lines -> scan newlines
  where
    n = sizeofChildren cs
    scan sel
      | 2 * k > total = backwards (n - 1) 0
      | otherwise = forwards 0 k
      where
        forwards !i !j
          | i >= n - 1 || m >= j = Sought i j
          | otherwise = forwards (i + 1) (j - m)
          where
            m = sel (nodeMetrics (indexChildren cs i))
        -- Same child from the end: the last with fewer than k before it.
        backwards !i !after
          | i <= 0 = Sought 0 k
          | before < k = Sought i (k - before)
          | otherwise = backwards (i - 1) (total - before)
          where
            before = total - after - sel (nodeMetrics (indexChildren cs i))
    {-# INLINE scan #-}
{-# NOINLINE seekUnit #-}

-- | Like 'seekUnit', also returning the byte count before the child.
seekUnitBytes :: Unit -> Int -> Metrics -> Children a -> SoughtBytes
seekUnitBytes !u !k !total !cs = case u of
  Bytes -> case seekUnit Bytes k (bytes total) cs of Sought i j -> SoughtBytes i j (k - j)
  Chars -> scan chars
  Utf16 -> scan utf16Units
  Lines -> scan newlines
  where
    n = sizeofChildren cs
    scan sel = go 0 k 0
      where
        go !i !j !b
          | i >= n - 1 || sel m >= j = SoughtBytes i j b
          | otherwise = go (i + 1) (j - sel m) (b + bytes m)
          where
            m = nodeMetrics (indexChildren cs i)
    {-# INLINE scan #-}
{-# NOINLINE seekUnitBytes #-}

-- | Find the child containing byte @i@ and its relative offset. Unlike
-- 'seekChild', an offset at a boundary selects the following child.
seekByte :: Int -> Children a -> Sought
seekByte !i !cs = go 0 i
  where
    n = sizeofChildren cs
    go !c !j
      | c >= n - 1 || j < m = Sought c j
      | otherwise = go (c + 1) (j - m)
      where
        m = nodeBytes (indexChildren cs c)
{-# NOINLINE seekByte #-}

------------------------------------------------------------------------------
-- Building inner nodes

-- | Combine child annotations with a right fold. The @()@ measure does not
-- evaluate its arguments, so it avoids traversing the children.
foldAnn :: Monoid a => Children a -> a
foldAnn cs = go 0
  where
    n = sizeofChildren cs
    go !i
      | i >= n - 1 = nodeAnn (indexChildren cs i)
      | otherwise = nodeAnn (indexChildren cs i) <> go (i + 1)
{-# INLINE foldAnn #-}

-- | Metrics of children @off .. off + cnt - 1@.
--
-- Children all have the same height. For leaves, add packed counts directly:
-- @maxChildren * maxChunk < 65536@ prevents carries between 16-bit fields.
sumMetrics :: Children a -> Int -> Int -> Metrics
sumMetrics !cs !off !cnt
  | cnt <= 0 = mempty
  | otherwise = case indexChildren cs off of
      PackedLeaf{} -> unpackMetrics (packed off 0)
      Inner{} -> spelled off 0 0 0 0
  where
    end = off + cnt
    packed !i !acc
      | i >= end = acc
      | otherwise = case indexChildren cs i of
          PackedLeaf m _ _ -> packed (i + 1) (acc + m)
          Inner{} -> unreachable "sumMetrics"
    spelled !i !b !c !w !l
      | i >= end = Metrics b c w l
      | otherwise = case indexChildren cs i of
          Inner (Metrics b' c' w' l') _ _ _ -> spelled (i + 1) (b + b') (c + c') (w + w') (l + l')
          PackedLeaf{} -> unreachable "sumMetrics"

-- | Build an inner node with known height and metrics and at least one
-- child. Reusing metrics avoids scanning the children again.
inner :: Monoid a => Int -> Metrics -> Children a -> Node a
inner h m cs = Inner m h (foldAnn cs) cs
{-# INLINE inner #-}

-- | Build an inner node out of at least one child.
mkInner :: Monoid a => Children a -> Node a
mkInner cs = inner (nodeHeight (indexChildren cs 0) + 1) (sumMetrics cs 0 (sizeofChildren cs)) cs
{-# INLINABLE mkInner #-}
{-# SPECIALIZE mkInner :: Children () -> Node () #-}

mkInner2 :: Monoid a => Node a -> Node a -> Node a
mkInner2 x y = inner (nodeHeight x + 1) (nodeMetrics x <> nodeMetrics y) $ runChildren $ do
  m <- newChildren 2 x
  writeChildren m 1 y
  pure m
{-# INLINABLE mkInner2 #-}
{-# SPECIALIZE mkInner2 :: Node () -> Node () -> Node () #-}

replaceAt :: Children a -> Int -> Node a -> Children a
replaceAt arr = replaceIn arr 0 (sizeofChildren arr)

-- | Children @[off, off + cnt)@ with the @i@-th of them replaced.
replaceIn :: Children a -> Int -> Int -> Int -> Node a -> Children a
replaceIn arr off cnt i x = runChildren $ do
  m <- thawChildren arr off cnt
  writeChildren m i x
  pure m

-- | The first @cnt@ elements followed by two more. One allocation if the
-- array has two elements to spare, which are copied along and overwritten.
snoc2 :: Children a -> Int -> Node a -> Node a -> Children a
snoc2 arr cnt x y = runChildren $ do
  m <-
    if cnt + 2 <= sizeofChildren arr
      then thawChildren arr 0 (cnt + 2)
      else do
        out <- newChildren (cnt + 2) y
        copyChildren out 0 arr 0 cnt
        pure out
  writeChildren m cnt x
  writeChildren m (cnt + 1) y
  pure m

-- | Two elements followed by all but the first @off@ of an array.
cons2 :: Node a -> Node a -> Children a -> Int -> Children a
cons2 x y arr off = runChildren $ do
  let cnt = sizeofChildren arr - off
  m <-
    if off >= 2
      then thawChildren arr (off - 2) (cnt + 2)
      else do
        out <- newChildren (cnt + 2) x
        copyChildren out 2 arr off cnt
        pure out
  writeChildren m 0 x
  writeChildren m 1 y
  pure m

-- | Replace element @c@ by two.
insert2 :: Children a -> Int -> Node a -> Node a -> Children a
insert2 arr c x y = runChildren $ do
  let n = sizeofChildren arr
  m <- newChildren (n + 1) x
  copyChildren m 0 arr 0 c
  writeChildren m (c + 1) y
  copyChildren m (c + 2) arr (c + 1) (n - c - 1)
  pure m

-- | A slice of one array followed by a slice of another, at least one of
-- them not empty.
append2 :: Children a -> Int -> Int -> Children a -> Int -> Int -> Children a
append2 a offa cnta b offb cntb = runChildren $ do
  m <- newChildren (cnta + cntb) (if cnta > 0 then indexChildren a offa else indexChildren b offb)
  copyChildren m 0 a offa cnta
  copyChildren m cnta b offb cntb
  pure m

------------------------------------------------------------------------------
-- Scanning chunks

isContByte :: Word8 -> Bool
isContByte b = b .&. 0xC0 == 0x80
{-# INLINE isContByte #-}

byteAt :: ByteArray -> Int -> Word8
byteAt = indexByteArray
{-# INLINE byteAt #-}

-- | Unaligned read of 8 bytes. Only ever used for counting, so the byte
-- order does not matter.
indexWord64 :: ByteArray -> Int -> Word64
indexWord64 (ByteArray ba) (I# i) = W64# (indexWord8ArrayAsWord64# ba i)
{-# INLINE indexWord64 #-}

lows, highs :: Word64
lows = 0x0101010101010101
highs = 0x8080808080808080

-- | Sum of the bytes of a word whose bytes are all 0 or 1.
byteSum :: Word64 -> Int
byteSum m = fromIntegral ((m * lows) `unsafeShiftR` 56)
{-# INLINE byteSum #-}

-- | Number of UTF-8 continuation bytes (@10xxxxxx@) in a word.
contCount :: Word64 -> Int
contCount w = byteSum ((w `unsafeShiftR` 7) .&. (complement w `unsafeShiftR` 6) .&. lows)
{-# INLINE contCount #-}

-- | Count 4-byte sequence leaders (@1111xxxx@). For valid UTF-8, each marks
-- a code point requiring two UTF-16 code units.
fourCount :: Word64 -> Int
fourCount w =
  byteSum
    ( (w .&. (w `unsafeShiftL` 1) .&. (w `unsafeShiftL` 2) .&. (w `unsafeShiftL` 3) .&. highs)
        `unsafeShiftR` 7
    )
{-# INLINE fourCount #-}

-- | Number of @\\n@ bytes in a word.
nlCount :: Word64 -> Int
nlCount w = byteSum ((complement t .&. highs) `unsafeShiftR` 7)
  where
    x = w `xor` 0x0A0A0A0A0A0A0A0A
    -- High bit of every non-zero byte of x. Exact: no carry crosses a byte.
    t = ((x .&. 0x7F7F7F7F7F7F7F7F) + 0x7F7F7F7F7F7F7F7F) .|. x
{-# INLINE nlCount #-}

-- | Metrics of @len@ bytes of valid UTF-8 starting at @off@.
sliceMetrics :: ByteArray -> Int -> Int -> Metrics
sliceMetrics !arr !off !len
  | len >= simdMin = cMetrics simdLevel arr off len
  | otherwise = swarMetrics arr off len
{-# NOINLINE sliceMetrics #-}

-- | 'sliceMetrics', 8 bytes at a time.
swarMetrics :: ByteArray -> Int -> Int -> Metrics
swarMetrics !arr !off !len = goWord off 0 0 0
  where
    end = off + len
    goWord !i !conts !fours !nls
      | i + 8 <= end =
          let w = indexWord64 arr i
           in if w .&. highs == 0
                then -- ASCII needs only the newline count.
                  goWord (i + 8) conts fours (nls + nlCount w)
                else goWord (i + 8) (conts + contCount w) (fours + fourCount w) (nls + nlCount w)
      | otherwise = goByte i conts fours nls
    goByte !i !conts !fours !nls
      | i >= end =
          let cs = len - conts
           in Metrics len cs (cs + fours) nls
      | otherwise =
          let b = byteAt arr i
           in goByte
                (i + 1)
                (conts + fromEnum (isContByte b))
                (fours + fromEnum (b >= 0xF0))
                (nls + fromEnum (b == 0x0A))

-- | Largest code point boundary @<= i@. Relies on some byte at or before @i@
-- (and inside the text) being a boundary.
roundDownFrom :: ByteArray -> Int -> Int
roundDownFrom !arr (I# i) = I# (go i)
  where
    -- On unboxed offsets: a loop that returns its argument would box it on
    -- every turn.
    go j
      | isContByte (byteAt arr (I# j)) = go (j -# 1#)
      | otherwise = j
{-# INLINE roundDownFrom #-}

-- | Largest code point boundary @<= i@ of a chunk, clamped to the chunk.
roundDown :: ByteArray -> Int -> Int
roundDown arr i
  | i <= 0 = 0
  | i >= sizeofByteArray arr = sizeofByteArray arr
  | otherwise = roundDownFrom arr i

-- | Smallest code point boundary @>= i@ of a chunk, for @i >= 0@.
roundUp :: ByteArray -> Int -> Int
roundUp arr = go
  where
    len = sizeofByteArray arr
    go !j
      | j >= len = len
      | isContByte (byteAt arr j) = go (j + 1)
      | otherwise = j

-- | Byte offset of a location within a chunk: the largest code point boundary
-- with at most @k@ units before it, or for 'Lines' the offset just after the
-- @k@-th line feed. Clamped to the chunk.
offsetInChunk :: Unit -> Int -> ByteArray -> Int
offsetInChunk !u !k !arr
  | k <= 0 = 0
  | otherwise = case u of
      Bytes -> roundDown arr k
      Lines -> scanLines k arr
      _ -> scanUnits (u == Utf16) k arr 0 (sizeofByteArray arr)

-- | @scanUnits wide k arr from to@ walks over code points from the boundary
-- @from@ and stops in front of the first one that does not fit into @k@
-- units, or at @to@.
scanUnits :: Bool -> Int -> ByteArray -> Int -> Int -> Int
scanUnits !wide !k !arr !from !to
  | to - from >= simdMin = cScanUnits simdLevel wide k arr from to
  | otherwise = swarScanUnits wide k arr from to
{-# NOINLINE scanUnits #-}

-- | 'scanUnits', skipping whole words as long as everything in them fits.
swarScanUnits :: Bool -> Int -> ByteArray -> Int -> Int -> Int
swarScanUnits !wide !k !arr !from !to = goWord from 0
  where
    goWord !i !n
      | i + 8 <= to =
          let w = indexWord64 arr i
              c
                | w .&. highs == 0 = 8
                | wide = 8 - contCount w + fourCount w
                | otherwise = 8 - contCount w
           in if n + c <= k then goWord (i + 8) (n + c) else goByte i n
      | otherwise = goByte i n
    goByte !i !n
      | i >= to = to
      | isContByte b = goByte (i + 1) n
      | n + u > k = i
      | otherwise = goByte (i + 1) (n + u)
      where
        b = byteAt arr i
        u = if wide && b >= 0xF0 then 2 else 1
{-# INLINE swarScanUnits #-}

-- | Whether the text is ASCII, so byte, code point, and UTF-16 offsets
-- coincide without a scan.
isAscii :: Metrics -> Bool
isAscii m = bytes m == chars m
{-# INLINE isAscii #-}

-- | 'offsetInChunk' for a leaf with known metrics.
leafOffset :: Unit -> Int -> Metrics -> ByteArray -> Int
leafOffset !u !k !m !arr
  | k <= 0 = 0
  | u == Lines = scanLines k arr
  | isAscii m = min k (bytes m)
  | otherwise = offsetInChunk u k arr
{-# NOINLINE leafOffset #-}

-- | Metrics of the first @b@ bytes of a leaf with known metrics, counting
-- whichever side of the cut is shorter.
leafPrefixMetrics :: Metrics -> ByteArray -> Int -> Metrics
leafPrefixMetrics m arr b
  | b <= 0 = mempty
  | b >= bytes m = m
  | otherwise = leafCutMetrics m arr b
{-# INLINE leafPrefixMetrics #-}

-- | Like 'leafPrefixMetrics', with a known newline count. ASCII leaves
-- need no additional scan.
leafPrefixWithLines :: Metrics -> ByteArray -> Int -> Int -> Metrics
leafPrefixWithLines m arr b nls
  | isAscii m && 0 < b && b < bytes m = Metrics b b b nls
  | otherwise = leafPrefixMetrics m arr b
{-# INLINE leafPrefixWithLines #-}

-- | Measure a cut strictly inside a leaf. Kept separate from
-- 'leafPrefixMetrics' so GHC can pass the input metrics unboxed.
leafCutMetrics :: Metrics -> ByteArray -> Int -> Metrics
leafCutMetrics !m !arr !b
  | 2 * b > size = m `subMetrics` leafSliceMetrics m arr b (size - b)
  | otherwise = leafSliceMetrics m arr 0 b
  where
    size = bytes m
{-# NOINLINE leafCutMetrics #-}

-- | Metrics of a slice of a leaf with known metrics. (Not local to
-- 'leafCutMetrics', where it would be allocated as a closure.)
leafSliceMetrics :: Metrics -> ByteArray -> Int -> Int -> Metrics
leafSliceMetrics !m !arr !off !len
  | isAscii m = Metrics len len len (if newlines m == 0 then 0 else countNewlines arr off len)
  | otherwise = sliceMetrics arr off len

countNewlines :: ByteArray -> Int -> Int -> Int
countNewlines !arr !off !len
  | len >= simdMin = cNewlines simdLevel arr off len
  | otherwise = swarNewlines arr off len
{-# NOINLINE countNewlines #-}

swarNewlines :: ByteArray -> Int -> Int -> Int
swarNewlines !arr !off !len = goWord off 0
  where
    end = off + len
    goWord !i !n
      | i + 8 <= end = goWord (i + 8) (n + nlCount (indexWord64 arr i))
      | otherwise = goByte i n
    goByte !i !n
      | i >= end = n
      | otherwise = goByte (i + 1) (n + fromEnum (byteAt arr i == 0x0A))

-- | Offset of the first @\\n@ at or after @from@, or the size of the chunk.
findNewline :: ByteArray -> Int -> Int
findNewline !arr !from
  | sizeofByteArray arr - from >= simdMin = cFindNewline simdLevel arr from
  | otherwise = swarFindNewline arr from
{-# NOINLINE findNewline #-}

swarFindNewline :: ByteArray -> Int -> Int
swarFindNewline !arr = goWord
  where
    len = sizeofByteArray arr
    goWord !i
      | i + 8 <= len && nlCount (indexWord64 arr i) == 0 = goWord (i + 8)
      | otherwise = goByte i
    goByte !i
      | i >= len || byteAt arr i == 0x0A = i
      | otherwise = goByte (i + 1)

-- | Offset of the last @\\n@ before @to@, or @-1@.
findNewlineBack :: ByteArray -> Int -> Int
findNewlineBack !arr !to
  | to >= simdMin = cFindNewlineBack simdLevel arr to
  | otherwise = swarFindNewlineBack arr to
{-# NOINLINE findNewlineBack #-}

swarFindNewlineBack :: ByteArray -> Int -> Int
swarFindNewlineBack !arr = goWord
  where
    goWord !i
      | i >= 8 && nlCount (indexWord64 arr (i - 8)) == 0 = goWord (i - 8)
      | otherwise = goByte i
    goByte !i
      | i <= 0 = -1
      | byteAt arr (i - 1) == 0x0A = i - 1
      | otherwise = goByte (i - 1)

-- | The number of line feeds among the first @b@ bytes of a leaf with known
-- metrics, counting whichever side of the cut is shorter.
leafNewlinesBefore :: Metrics -> ByteArray -> Int -> Int
leafNewlinesBefore !m !arr !b
  | newlines m == 0 || b <= 0 = 0
  | b >= size = newlines m
  | 2 * b > size = newlines m - countNewlines arr b (size - b)
  | otherwise = countNewlines arr 0 b
  where
    size = bytes m

-- | The offset just after the @k@-th @\\n@ of a chunk, for @k >= 1@, or the
-- size of the chunk.
scanLines :: Int -> ByteArray -> Int
scanLines !k !arr
  | sizeofByteArray arr >= simdMin = cNthNewline simdLevel k arr
  | otherwise = swarScanLines k arr
{-# NOINLINE scanLines #-}

swarScanLines :: Int -> ByteArray -> Int
swarScanLines !k !arr = goWord 0 0
  where
    len = sizeofByteArray arr
    goWord !i !n
      | i + 8 <= len =
          let c = nlCount (indexWord64 arr i)
           in if n + c < k then goWord (i + 8) (n + c) else goByte i n
      | otherwise = goByte i n
    goByte !i !n
      | i >= len = len
      | byteAt arr i == 0x0A = if n + 1 == k then i + 1 else goByte (i + 1) (n + 1)
      | otherwise = goByte (i + 1) n

-- | Byte offsets of a line's start and terminating @\\n@ within a chunk.
-- A missing endpoint is represented by the chunk size.
data ChunkLine = ChunkLine {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Show)

-- | A chunk's line @k@ start and terminator in one foreign call.
chunkLine :: Int -> ByteArray -> ChunkLine
chunkLine !k !arr
  | sizeofByteArray arr >= simdMin = cLineSpan simdLevel k arr
  | otherwise = swarLineSpan k arr
{-# NOINLINE chunkLine #-}

swarLineSpan :: Int -> ByteArray -> ChunkLine
swarLineSpan !k !arr = ChunkLine from (swarFindNewline arr from)
  where
    from = if k <= 0 then 0 else swarScanLines k arr

------------------------------------------------------------------------------
-- Scanning chunks with SIMD

-- | A set of chunk scan implementations. Exposed so tests can compare
-- every available implementation against the same model.
data Kernels = Kernels
  { kernelsName :: String
  -- ^ Implementation name, such as Haskell, SSE2, or AVX2.
  , kernelMetrics :: ByteArray -> Int -> Int -> Metrics
  -- ^ Like 'sliceMetrics'.
  , kernelNewlines :: ByteArray -> Int -> Int -> Int
  -- ^ Line feeds in a slice.
  , kernelFindNewline :: ByteArray -> Int -> Int
  -- ^ The first line feed at or after an offset, or the size.
  , kernelFindNewlineBack :: ByteArray -> Int -> Int
  -- ^ The last line feed before an offset, or -1.
  , kernelScanUnits :: Bool -> Int -> ByteArray -> Int -> Int -> Int
  -- ^ @kernelScanUnits wide k arr from to@: from a code point boundary, the
  -- offset in front of the first code point that does not fit into @k@ code
  -- points (UTF-16 code units if @wide@), or @to@.
  , kernelLineSpan :: Int -> ByteArray -> ChunkLine
  -- ^ Just after the @k@-th line feed, or 0 for @k <= 0@, and the first line
  -- feed at or after that, or the size.
  }

-- | The scans in Haskell, 8 bytes at a time.
swarKernels :: Kernels
swarKernels = Kernels "Haskell" swarMetrics swarNewlines swarFindNewline swarFindNewlineBack swarScanUnits swarLineSpan

-- | Available scans: Haskell, then C implementations in increasing SIMD
-- order. The last is used on long slices. Without the @simd@ flag, only
-- the Haskell implementation is included.
kernels :: [Kernels]
kernels = swarKernels : map simdKernels [0 .. simdLevel]

-- | Minimum slice length worth a foreign call. Scans dispatch on it to known
-- functions: picking from a t'Kernels' record can box arguments.
simdMin :: Int

-- | The best level of SIMD support of the machine, as the C numbers them.
simdLevel :: Int

-- | The scans in C at a level of SIMD support, which must be supported.
simdKernels :: Int -> Kernels
simdKernels level =
  Kernels
    (["portable C", "SSE2", "AVX2"] !! level)
    (cMetrics level)
    (cNewlines level)
    (cFindNewline level)
    (cFindNewlineBack level)
    (cScanUnits level)
    (cLineSpan level)

-- The scans of a t'Kernels' in C, given the level of SIMD support.
cMetrics :: Int -> ByteArray -> Int -> Int -> Metrics
cNewlines :: Int -> ByteArray -> Int -> Int -> Int
cFindNewline :: Int -> ByteArray -> Int -> Int
cFindNewlineBack :: Int -> ByteArray -> Int -> Int
cNthNewline :: Int -> Int -> ByteArray -> Int
cScanUnits :: Int -> Bool -> Int -> ByteArray -> Int -> Int -> Int
cLineSpan :: Int -> Int -> ByteArray -> ChunkLine

#ifdef NANO_ROPE_SIMD
-- Unsafe foreign calls keep the unpinned array payload stable for the call:
-- GHC cannot perform a moving garbage collection until the call returns.

#ifdef NANO_ROPE_SMALL
-- Exercise C scans even with the test suite's tiny chunks.
simdMin = 0
#else
simdMin = 32
#endif

-- Cache CPU detection. A pure foreign call could be inlined and repeated
-- on every scan.
simdLevel = unsafeDupablePerformIO c_simdLevel
{-# NOINLINE simdLevel #-}

-- C packs each count into 21 bits. Use Haskell for larger slices to avoid
-- overflow (normal chunks are much smaller).
cMetrics level arr@(ByteArray ba) off len
  | len >= 0x200000 = swarMetrics arr off len
  | otherwise =
      let w = c_metrics level ba off len
          field s = fromIntegral ((w `unsafeShiftR` s) .&. 0x1FFFFF)
          cs = len - field 0
       in Metrics len cs (cs + field 21) (field 42)
{-# INLINE cMetrics #-}

cNewlines level (ByteArray ba) off len = c_newlines level ba off len
{-# INLINE cNewlines #-}

cFindNewline level arr@(ByteArray ba) from = c_findNewline level ba from (sizeofByteArray arr)
{-# INLINE cFindNewline #-}

cFindNewlineBack level (ByteArray ba) to = c_findNewlineBack level ba to
{-# INLINE cFindNewlineBack #-}

cNthNewline level k arr@(ByteArray ba) = c_nthNewline level ba (sizeofByteArray arr) k
{-# INLINE cNthNewline #-}

cScanUnits level wide k (ByteArray ba) from to = c_scanUnits level ba from to k (fromEnum wide)
{-# INLINE cScanUnits #-}

-- Two offsets into a chunk, 32 bits each.
cLineSpan level k arr@(ByteArray ba) =
  let w = c_lineSpan level ba (sizeofByteArray arr) k
   in ChunkLine (fromIntegral (w .&. 0xFFFFFFFF)) (fromIntegral (w `unsafeShiftR` 32))
{-# INLINE cLineSpan #-}

foreign import ccall unsafe "nano_rope_simd_level" c_simdLevel :: IO Int
foreign import ccall unsafe "nano_rope_metrics" c_metrics :: Int -> ByteArray# -> Int -> Int -> Word64
foreign import ccall unsafe "nano_rope_newlines" c_newlines :: Int -> ByteArray# -> Int -> Int -> Int
foreign import ccall unsafe "nano_rope_find_newline" c_findNewline :: Int -> ByteArray# -> Int -> Int -> Int
foreign import ccall unsafe "nano_rope_find_newline_back" c_findNewlineBack :: Int -> ByteArray# -> Int -> Int
foreign import ccall unsafe "nano_rope_nth_newline" c_nthNewline :: Int -> ByteArray# -> Int -> Int -> Int
foreign import ccall unsafe "nano_rope_scan_units" c_scanUnits :: Int -> ByteArray# -> Int -> Int -> Int -> Int -> Int
foreign import ccall unsafe "nano_rope_line_span" c_lineSpan :: Int -> ByteArray# -> Int -> Int -> Word64
#else
simdMin = maxBound
simdLevel = -1
cMetrics _ = swarMetrics
cNewlines _ = swarNewlines
cFindNewline _ = swarFindNewline
cFindNewlineBack _ = swarFindNewlineBack
cNthNewline _ = swarScanLines
cScanUnits _ = swarScanUnits
cLineSpan _ = swarLineSpan
#endif

------------------------------------------------------------------------------
-- Leaves and bulk construction

-- | Zero-copy view of a slice of a chunk.
viewSlice :: ByteArray -> Int -> Int -> Text
viewSlice (ByteArray ba) off len
  | len <= 0 = T.empty
  | otherwise = TI.Text (A.ByteArray ba) off len
{-# INLINE viewSlice #-}

-- | Zero-copy view of a whole chunk.
chunkText :: ByteArray -> Text
chunkText arr = viewSlice arr 0 (sizeofByteArray arr)
{-# INLINE chunkText #-}

mkLeaf :: Measure a => ByteArray -> Node a
mkLeaf arr = Leaf (sliceMetrics arr 0 (sizeofByteArray arr)) (measureChunk (chunkText arr)) arr
{-# INLINE mkLeaf #-}

-- | A leaf whose metrics are already known.
mkLeafWith :: Measure a => Metrics -> ByteArray -> Node a
mkLeafWith m arr = Leaf m (measureChunk (chunkText arr)) arr
{-# INLINE mkLeafWith #-}

-- | A slice of one chunk followed by a slice of another.
concatSlices :: ByteArray -> Int -> Int -> ByteArray -> Int -> Int -> ByteArray
concatSlices a offa la b offb lb = runByteArray $ do
  out <- newByteArray (la + lb)
  copyByteArray out 0 a offa la
  copyByteArray out la b offb lb
  pure out

-- | @spliceArray arr i j src off len@ replaces bytes @[i, j)@ of @arr@
-- with @len@ bytes of @src@ starting at @off@.
spliceArray :: ByteArray -> Int -> Int -> ByteArray -> Int -> Int -> ByteArray
spliceArray arr i j src soff slen = runByteArray $ do
  out <- newByteArray (sizeofByteArray arr - (j - i) + slen)
  copyByteArray out 0 arr 0 i
  copyByteArray out i src soff slen
  copyByteArray out (i + slen) arr j (sizeofByteArray arr - j)
  pure out

-- | Read a byte from the result of 'spliceArray' without allocating that result.
splicedByte :: ByteArray -> Int -> Int -> ByteArray -> Int -> Int -> Int -> Word8
splicedByte arr i j src soff slen at
  | at < i = byteAt arr at
  | at < i + slen = byteAt src (soff + at - i)
  | otherwise = byteAt arr (j + at - i - slen)
{-# INLINE splicedByte #-}

-- | Copy bytes @[from, to)@ of a splice directly, without allocating the
-- full splice. Used to split an overflowing leaf into two buffers.
splicedSlice :: ByteArray -> Int -> Int -> ByteArray -> Int -> Int -> Int -> Int -> ByteArray
splicedSlice !arr !i !j !src !soff !slen !from !to = runByteArray $ do
  out <- newByteArray (to - from)
  -- Copy the overlap with each piece: prefix, inserted text, and suffix.
  let piece !start !end source !sourceOff =
        let lo = max from start
            hi = min to end
         in when (lo < hi) $ copyByteArray out (lo - from) source (sourceOff + lo - start) (hi - lo)
  piece 0 i arr 0
  piece i (i + slen) src soff
  piece (i + slen) (sizeofByteArray arr - (j - i) + slen) arr j
  pure out

-- | A tree over more than 'maxChunk' bytes in evenly sized leaves, built
-- top-down into parent arrays (no intermediate lists). Leaves aim a little
-- under 'maxChunk' so cuts can round down to code points.
treeFromSlice :: Measure a => ByteArray -> Int -> Int -> Node a
treeFromSlice !arr !off !len = node (levelSizes leaves) 0
  where
    target = maxChunk - 4
    leaves = (len + target - 1) `quot` target
    (q, r) = len `quotRem` leaves
    -- Where leaf @j@ starts.
    cut j
      | j <= 0 = off
      | j >= leaves = off + len
      | otherwise = roundDownFrom arr (off + j * q + (j * r) `quot` leaves)
    -- Node @j@ of a level, given the sizes of that level and the ones below.
    -- Strict in everything: a lazy index is a thunk for every node.
    node sizes !j = case sizes of
      parents : below@(children : _) ->
        -- Spread children evenly so no parent falls below 'minChildren'.
        let !cq = children `quot` parents
            !cr = children `rem` parents
            !first = j * cq + min j cr
            !size = if j < cr then cq + 1 else cq
         in mkInner (generateChildren size (\i -> node below (first + i)))
      [_] -> let !from = cut j in mkLeaf (cloneByteArray arr from (cut (j + 1) - from))
      [] -> emptyNode
{-# INLINABLE treeFromSlice #-}
{-# SPECIALIZE treeFromSlice :: ByteArray -> Int -> Int -> Node () #-}

-- | Node counts per level for the given leaf count, from root to leaves.
levelSizes :: Int -> [Int]
levelSizes = go []
  where
    go above n
      | n <= 1 = n : above
      | otherwise = go (n : above) ((n + maxChildren - 1) `quot` maxChildren)

fromTextNode :: Measure a => Text -> Node a
fromTextNode (TI.Text (A.ByteArray ba) off len)
  | len <= 0 = emptyNode
  | len <= maxChunk =
      -- Share the array when the text owns all of it.
      mkLeaf (if off == 0 && len == sizeofByteArray arr then arr else cloneByteArray arr off len)
  | otherwise = treeFromSlice arr off len
  where
    arr = ByteArray ba
{-# INLINABLE fromTextNode #-}
{-# SPECIALIZE fromTextNode :: Text -> Node () #-}

------------------------------------------------------------------------------
-- Concatenation

-- | One or two nodes of the taller tree's height, or 'None' when an edit
-- leaves its leaf. An unboxed sum: passing a node up allocates nothing.
type Result a = (# (# #) | Node a | (# Node a, Node a #) #)

pattern None :: Result a
pattern None = (# (# #) | | #)

pattern One :: Node a -> Result a
pattern One n = (# | n | #)

pattern Two :: Node a -> Node a -> Result a
pattern Two x y = (# | | (# x, y #) #)

{-# COMPLETE None, One, Two #-}

appendNode :: Measure a => Node a -> Node a -> Node a
appendNode l r
  | nodeIsEmpty l = r
  | nodeIsEmpty r = l
  | otherwise = case merge l r of
      One n -> n
      Two x y -> mkInner2 x y
      None -> unreachable "appendNode"
{-# INLINABLE appendNode #-}
{-# SPECIALIZE appendNode :: Node () -> Node () -> Node () #-}

unreachable :: forall r (a :: TYPE r). String -> a
unreachable fun = error ("Data.Text.NanoRope: " ++ fun)
{-# NOINLINE unreachable #-}

-- | The outcome of an edit with the room it leaves, see 'editNode'.
roomy :: Result a -> Int -> (# Result a, Int# #)
roomy r (I# room) = (# r, room #)
{-# INLINE roomy #-}

-- | Merge two non-empty trees whose roots are allowed to be undersized.
-- Walks down the spine of the taller tree to the height of the shorter one,
-- merges there, and propagates at most one extra node back up.
merge :: Measure a => Node a -> Node a -> Result a
merge l r = case compare (nodeHeight l) (nodeHeight r) of
  EQ -> mergeEq l r
  GT -> case l of
    Inner ml h _ cs ->
      let k = sizeofChildren cs - 1
          m = ml <> nodeMetrics r
       in case merge (indexChildren cs k) r of
            One x -> One (inner h m (replaceAt cs k x))
            Two x y -> fromChildren h m (snoc2 cs k x y)
            None -> None
    Leaf{} -> unreachable "merge"
  LT -> case r of
    Inner mr h _ cs ->
      let m = nodeMetrics l <> mr
       in case merge l (indexChildren cs 0) of
            One x -> One (inner h m (replaceAt cs 0 x))
            Two x y -> fromChildren h m (cons2 x y cs 1)
            None -> None
    Leaf{} -> unreachable "merge"
{-# INLINABLE merge #-}
{-# SPECIALIZE merge :: Node () -> Node () -> Result () #-}

-- | Merge two trees of equal height. Nodes that are both large enough become
-- siblings untouched; otherwise their contents are pooled and redistributed.
mergeEq :: Measure a => Node a -> Node a -> Result a
mergeEq l@(Leaf ml al bl) r@(Leaf mr ar br)
  | sl >= minChunk && sr >= minChunk = Two l r
  | total <= maxChunk = One (Leaf (ml <> mr) (al <> ar) (concatSlices bl 0 sl br 0 sr))
  | half < sl =
      -- Even them out: the end of the left leaf goes to the right one.
      let cut = roundDownFrom bl half
          mx = leafPrefixMetrics ml bl cut
       in Two
            (mkLeafWith mx (cloneByteArray bl 0 cut))
            (mkLeafWith ((ml <> mr) `subMetrics` mx) (concatSlices bl cut (sl - cut) br 0 sr))
  | otherwise =
      let cut = roundDownFrom br (half - sl)
          mx = leafPrefixMetrics mr br cut
       in Two
            (mkLeafWith (ml <> mx) (concatSlices bl 0 sl br 0 cut))
            (mkLeafWith (mr `subMetrics` mx) (cloneByteArray br cut (sr - cut)))
  where
    sl = sizeofByteArray bl
    sr = sizeofByteArray br
    total = sl + sr
    half = total `quot` 2
mergeEq l@(Inner ml h _ csl) r@(Inner mr _ _ csr)
  | nl >= minChildren && nr >= minChildren = Two l r
  | nl + nr <= maxChildren = One (inner h (ml <> mr) (append2 csl 0 nl csr 0 nr))
  | half <= nl =
      -- Hand the last children of the left node over to the right one.
      let moved = sumMetrics csl half (nl - half)
       in Two
            (inner h (ml `subMetrics` moved) (cloneChildren csl 0 half))
            (inner h (moved <> mr) (append2 csl half (nl - half) csr 0 nr))
  | otherwise =
      let cnt = half - nl
          moved = sumMetrics csr 0 cnt
       in Two
            (inner h (ml <> moved) (append2 csl 0 nl csr 0 cnt))
            (inner h (mr `subMetrics` moved) (cloneChildren csr cnt (nr - cnt)))
  where
    nl = sizeofChildren csl
    nr = sizeofChildren csr
    half = (nl + nr + 1) `quot` 2
mergeEq _ _ = unreachable "mergeEq"
{-# INLINABLE mergeEq #-}
{-# SPECIALIZE mergeEq :: Node () -> Node () -> Result () #-}

-- | One node of height @h@ if the children fit, else two.
fromChildren :: Monoid a => Int -> Metrics -> Children a -> Result a
fromChildren !h !m !cs
  | n <= maxChildren = One (inner h m cs)
  | otherwise =
      let half = (n + 1) `quot` 2
          ml = sumMetrics cs 0 half
       in Two
            (inner h ml (cloneChildren cs 0 half))
            (inner h (m `subMetrics` ml) (cloneChildren cs half (n - half)))
  where
    n = sizeofChildren cs
{-# INLINABLE fromChildren #-}
{-# SPECIALIZE fromChildren :: Int -> Metrics -> Children () -> Result () #-}

------------------------------------------------------------------------------
-- Take, drop and split

-- | Whether @k@ reaches the document end. In 'Lines', an offset equal to
-- the newline count selects the final line start, which may precede the end.
beyondEnd :: Unit -> Int -> Metrics -> Bool
beyondEnd u k total = k > n || (k == n && u /= Lines)
  where
    n = count u total
{-# INLINE beyondEnd #-}

takeRoot :: Measure a => Unit -> Int -> Node a -> Node a
takeRoot u k root
  | k <= 0 = emptyNode
  | beyondEnd u k (nodeMetrics root) = root
  | otherwise = takeNode u k root
{-# INLINABLE takeRoot #-}
{-# SPECIALIZE takeRoot :: Unit -> Int -> Node () -> Node () #-}

dropRoot :: Measure a => Unit -> Int -> Node a -> Node a
dropRoot u k root
  | k <= 0 = root
  | beyondEnd u k (nodeMetrics root) = emptyNode
  | otherwise = dropNode u k root
{-# INLINABLE dropRoot #-}
{-# SPECIALIZE dropRoot :: Unit -> Int -> Node () -> Node () #-}

splitRoot :: Measure a => Unit -> Int -> Node a -> (# Node a, Node a #)
splitRoot u k root
  | k <= 0 = (# emptyNode, root #)
  | beyondEnd u k (nodeMetrics root) = (# root, emptyNode #)
  | otherwise = splitNode u k root
{-# INLINABLE splitRoot #-}
{-# SPECIALIZE splitRoot :: Unit -> Int -> Node () -> (# Node (), Node () #) #-}

-- | Join children @[0, i)@ (with metrics @before@) to a lower tree.
-- Merge the boundary sibling as needed to repair an undersized root.
joinLeft :: Measure a => Int -> Children a -> Int -> Metrics -> Node a -> Node a
joinLeft !h !cs !i !before !l
  | i == 0 = l
  | nodeIsEmpty l = if i == 1 then indexChildren cs 0 else inner h before (cloneChildren cs 0 i)
  | otherwise = case merge (indexChildren cs (i - 1)) l of
      One x
        | i == 1 -> x
        | otherwise -> inner h m (replaceIn cs 0 i (i - 1) x)
      Two x y -> inner h m (snoc2 cs (i - 1) x y)
      None -> unreachable "joinLeft"
  where
    m = before <> nodeMetrics l
{-# INLINABLE joinLeft #-}
{-# SPECIALIZE joinLeft :: Int -> Children () -> Int -> Metrics -> Node () -> Node () #-}

-- | A lower tree followed by the children after child @i@ of a node of
-- height @h@, whose metrics are @after@.
joinRight :: Measure a => Int -> Children a -> Int -> Metrics -> Node a -> Node a
joinRight !h !cs !i !after !r
  | rest == 0 = r
  | nodeIsEmpty r = if rest == 1 then indexChildren cs (i + 1) else inner h after (cloneChildren cs (i + 1) rest)
  | otherwise = case merge r (indexChildren cs (i + 1)) of
      One x
        | rest == 1 -> x
        | otherwise -> inner h m (replaceIn cs (i + 1) rest 0 x)
      Two x y -> inner h m (cons2 x y cs (i + 2))
      None -> unreachable "joinRight"
  where
    rest = sizeofChildren cs - i - 1
    m = nodeMetrics r <> after
{-# INLINABLE joinRight #-}
{-# SPECIALIZE joinRight :: Int -> Children () -> Int -> Metrics -> Node () -> Node () #-}

takeNode :: Measure a => Unit -> Int -> Node a -> Node a
takeNode u k node = case node of
  Leaf m _ arr
    | b <= 0 -> emptyNode
    | b >= sizeofByteArray arr -> node
    | otherwise -> mkLeafWith (leafPrefixMetrics m arr b) (cloneByteArray arr 0 b)
    where
      b = leafOffset u k m arr
  Inner total h _ cs -> case seekChild u k total cs of
    Seek i before -> joinLeft h cs i before (takeNode u (k - count u before) (indexChildren cs i))
{-# INLINABLE takeNode #-}
{-# SPECIALIZE takeNode :: Unit -> Int -> Node () -> Node () #-}

dropNode :: Measure a => Unit -> Int -> Node a -> Node a
dropNode u k node = case node of
  Leaf m _ arr
    | b <= 0 -> node
    | b >= size -> emptyNode
    | otherwise -> mkLeafWith (m `subMetrics` leafPrefixMetrics m arr b) (cloneByteArray arr b (size - b))
    where
      size = sizeofByteArray arr
      b = leafOffset u k m arr
  Inner total h _ cs -> case seekChild u k total cs of
    Seek i before ->
      let child = indexChildren cs i
          after = total `subMetrics` before `subMetrics` nodeMetrics child
       in joinRight h cs i after (dropNode u (k - count u before) child)
{-# INLINABLE dropNode #-}
{-# SPECIALIZE dropNode :: Unit -> Int -> Node () -> Node () #-}

-- | 'takeNode' and 'dropNode' in one descent.
splitNode :: Measure a => Unit -> Int -> Node a -> (# Node a, Node a #)
splitNode u k node = case node of
  Leaf m _ arr
    | b <= 0 -> (# emptyNode, node #)
    | b >= size -> (# node, emptyNode #)
    | otherwise ->
        let pm = leafPrefixMetrics m arr b
         in (# mkLeafWith pm (cloneByteArray arr 0 b), mkLeafWith (m `subMetrics` pm) (cloneByteArray arr b (size - b)) #)
    where
      size = sizeofByteArray arr
      b = leafOffset u k m arr
  Inner total h _ cs -> case seekChild u k total cs of
    Seek i before ->
      let child = indexChildren cs i
          after = total `subMetrics` before `subMetrics` nodeMetrics child
       in case splitNode u (k - count u before) child of
            (# l, r #) -> (# joinLeft h cs i before l, joinRight h cs i after r #)
{-# INLINABLE splitNode #-}
{-# SPECIALIZE splitNode :: Unit -> Int -> Node () -> (# Node (), Node () #) #-}

------------------------------------------------------------------------------
-- Read-only descents

metricsAtNode :: Unit -> Int -> Node a -> Metrics
metricsAtNode u k root
  | k <= 0 = mempty
  | beyondEnd u k (nodeMetrics root) = nodeMetrics root
  | otherwise = go mempty k root
  where
    go !acc !j node = case node of
      Leaf m _ arr ->
        let !b = leafOffset u j m arr
         in -- Sought by lines, the offset is just after the j-th line feed
            -- of this leaf, which it has: there are j of them before it.
            acc <> if u == Lines then leafPrefixWithLines m arr b j else leafPrefixMetrics m arr b
      Inner total _ _ cs -> case seekChild u j total cs of
        Seek i before -> go (acc <> before) (j - count u before) (indexChildren cs i)

-- | Find only the byte offset, avoiding prefix measurement within the leaf.
byteOffsetAtNode :: Unit -> Int -> Node a -> Int
byteOffsetAtNode u k root
  | k <= 0 = 0
  | beyondEnd u k (nodeMetrics root) = nodeBytes root
  | otherwise = go 0 k root
  where
    go !acc !j node = case node of
      Leaf m _ arr -> acc + leafOffset u j m arr
      Inner total _ _ cs -> case seekUnitBytes u j total cs of
        SoughtBytes i j' b -> go (acc + b) j' (indexChildren cs i)

-- | The byte at offset @i@, for @0 <= i < size@.
indexByteNode :: Int -> Node a -> Word8
indexByteNode !i node = case node of
  Leaf _ _ arr -> byteAt arr i
  Inner _ _ _ cs -> case seekByte i cs of
    Sought c i' -> indexByteNode i' (indexChildren cs c)

-- | Bytes @i .. j-1@ as a 'Text', given boundaries @0 <= i <= j <= size@.
-- A range inside a single chunk is returned as a view of that chunk.
sliceToText :: Int -> Int -> Node a -> Text
sliceToText !i !j node
  | i >= j = T.empty
  | otherwise = case node of
      Leaf _ _ arr -> viewSlice arr i (j - i)
      Inner _ _ _ cs -> case seekByte i cs of
        Sought c i'
          | j' <= nodeBytes child -> sliceToText i' j' child
          | otherwise ->
              let !(ByteArray ba) = runByteArray $ do
                    out <- newByteArray (j - i)
                    copyRange out 0 i j node
                    pure out
               in TI.Text (A.ByteArray ba) 0 (j - i)
          where
            child = indexChildren cs c
            j' = j - (i - i')

-- | Copy bytes @i .. j-1@ of a node to offset @d@ of a buffer. Only boundary
-- children are clamped; covered ones go to 'copyNode'.
copyRange :: MutableByteArray s -> Int -> Int -> Int -> Node a -> ST s ()
copyRange !out !d !i !j node = case node of
  Leaf _ _ arr -> copyByteArray out d arr i (j - i)
  Inner _ _ _ cs -> case seekByte i cs of
    Sought c0 i0 -> go c0 (i - i0)
    where
      n = sizeofChildren cs
      go !c !start = when (c < n && start < j) $ do
        let child = indexChildren cs c
            end = start + nodeBytes child
        if i <= start && end <= j
          then () <$ copyNode out (d + start - i) child
          else do
            let lo = max i start
                hi = min j end
            when (lo < hi) $ copyRange out (d + lo - i) (lo - start) (hi - start) child
        go (c + 1) end

-- | Copy a whole subtree to offset @d@; return the next offset.
copyNode :: MutableByteArray s -> Int -> Node a -> ST s Int
copyNode !out !d node = case node of
  Leaf _ _ arr -> do
    let size = sizeofByteArray arr
    copyByteArray out d arr 0 size
    pure (d + size)
  Inner _ _ _ cs -> go 0 d
    where
      n = sizeofChildren cs
      go !c !d'
        | c >= n = pure d'
        | otherwise = copyNode out d' (indexChildren cs c) >>= go (c + 1)

-- | Two locations: a line start and the end of its content.
data Span = Span {-# UNPACK #-} !Metrics {-# UNPACK #-} !Metrics

-- | Start of line @l@ and end of its content (before @\\n@ or @\\r\\n@, or
-- at the end of the rope).
lineSpan :: Int -> Node a -> Span
lineSpan !l root
  | newlines next == newlines start = Span start next
  | bytes lf > bytes start && indexByteNode (bytes lf - 1) root == 0x0D = Span start (lf `subMetrics` Metrics 1 1 1 0)
  | otherwise = Span start lf
  where
    start = metricsAtNode Lines l root
    next = metricsAtNode Lines (max 0 l + 1) root
    lf = next `subMetrics` Metrics 1 1 1 1
{-# NOINLINE lineSpan #-}

-- | The end of the content of a line that starts at @from@ and is terminated
-- by the line feed at @lf@ of the same chunk.
contentEnd :: ByteArray -> Int -> Int -> Int
contentEnd arr from lf
  | lf > from && byteAt arr (lf - 1) == 0x0D = lf - 1
  | otherwise = lf
{-# INLINE contentEnd #-}

-- | Text of line @l >= 0@. If its start and terminator are in one leaf,
-- return a view after one descent. Otherwise use the general range lookup.
lineText :: Int -> Node a -> Text
lineText !l root
  | l > newlines (nodeMetrics root) = T.empty
  | otherwise = go l root
  where
    go !j node = case node of
      Leaf _ _ arr -> case chunkLine j arr of
        ChunkLine from lf
          | lf >= sizeofByteArray arr -> across
          | otherwise -> viewSlice arr from (contentEnd arr from lf - from)
      Inner total _ _ cs
        | j <= 0 -> go j (indexChildren cs 0)
        | otherwise -> case seekUnit Lines j (newlines total) cs of
            Sought i j' -> go j' (indexChildren cs i)
    -- The general case: a line across leaves.
    across = case lineSpan l root of
      Span start end -> sliceToText (bytes start) (bytes end) root

-- | Find an offset's position. One descent suffices when its line starts
-- in the same leaf or at the document start.
positionAtNode :: Unit -> Unit -> Int -> Node a -> Position
positionAtNode !from !to !k root
  | k <= 0 = Position 0 0
  | beyondEnd from k (nodeMetrics root) = general
  | otherwise = go 0 0 k root
  where
    go !ls !bs !j node = case node of
      Leaf m _ arr ->
        let !b = leafOffset from j m arr
            lf = if newlines m == 0 then -1 else findNewlineBack arr b
            start = lf + 1
            column
              | to == Lines = 0
              | to == Bytes || isAscii m = b - start
              | otherwise = count to (sliceMetrics arr start (b - start))
         in if lf < 0 && bs > 0
              then general
              else Position (ls + leafNewlinesBefore m arr b) column
      Inner total _ _ cs -> case seekChild from j total cs of
        Seek i before -> go (ls + newlines before) (bs + bytes before) (j - count from before) (indexChildren cs i)
    -- The general case: a line that starts in another leaf.
    general = positionOfMetrics to (metricsAtNode from k root) root

-- | The position, with its column in the given unit, of a location.
positionOfMetrics :: Unit -> Metrics -> Node a -> Position
positionOfMetrics u m root =
  Position (newlines m) (count u m - count u (metricsAtNode Lines (newlines m) root))
{-# INLINE positionOfMetrics #-}

-- | Locate a position, optionally computing its line start as well.
-- When the line start is not requested, its field may repeat the position.
linePositionNode :: Bool -> Unit -> Position -> Node a -> Span
linePositionNode !wanted !u (Position l0 c) root
  | l > newlines (nodeMetrics root) = general
  | otherwise = go mempty l root
  where
    l = max 0 l0
    -- Fast path: the line start and terminator are in the same leaf.
    go !acc !j node = case node of
      Leaf m _ arr -> case chunkLine j arr of
        ChunkLine from lf ->
          if lf >= sizeofByteArray arr
            then general
            else
              let !to = contentEnd arr from lf
                  !b = column m arr from to
                  -- Reuse the newline count; get the line start by
                  -- measuring only the column.
                  !at = acc <> leafPrefixWithLines m arr b j
               in Span (if wanted then at `subMetrics` sliceOfLine m arr from b else at) at
      Inner total _ _ cs
        | j <= 0 -> go acc j (indexChildren cs 0)
        | otherwise -> case seekChild Lines j total cs of
            Seek i before -> go (acc <> before) (j - newlines before) (indexChildren cs i)
    -- The offset of the column within a leaf, given those of the start of
    -- the line and of the end of its content.
    column !m !arr !from !to
      | c <= 0 = from
      | u == Lines || (u == Bytes || isAscii m) && c >= to - from = to
      | isAscii m = from + c
      | u == Bytes = roundDownFrom arr (from + c)
      | otherwise = scanUnits (u == Utf16) c arr from to
    -- The general case: a line across leaves, or no such line.
    general = case lineSpan l0 root of
      Span start end
        | c <= 0 -> Span start start
        | u == Lines || bytes there > bytes end -> Span start end
        | otherwise -> Span start there
        where
          there = metricsAtNode u (count u start + min c (count u (nodeMetrics root))) root

-- | Metrics of bytes @from .. to-1@ of a leaf with known metrics, which are
-- on one line.
sliceOfLine :: Metrics -> ByteArray -> Int -> Int -> Metrics
sliceOfLine m arr from to
  | to <= from = mempty
  | isAscii m = let d = to - from in Metrics d d d 0
  | otherwise = sliceMetrics arr from (to - from)
{-# INLINE sliceOfLine #-}

-- | Location of the end of the longest prefix not satisfying a monotone
-- predicate.
metricsWhereNode :: Measure a => (Metrics -> a -> Bool) -> Node a -> Metrics
metricsWhereNode p root
  | p mempty mempty = mempty
  | not (p (nodeMetrics root) (nodeAnn root)) = nodeMetrics root
  | otherwise = go mempty mempty root
  where
    -- Invariant: the predicate fails at the start of the node and holds at
    -- its end.
    go !m !a node = case node of
      Leaf _ _ arr ->
        let at b = p (m <> sliceMetrics arr 0 b) (a <> measureChunk (viewSlice arr 0 b))
            -- Bisect over code point boundaries: fails at lo, holds at hi.
            search !lo !hi
              | mid <= lo || mid >= hi = lo
              | at mid = search lo mid
              | otherwise = search mid hi
              where
                half = (lo + hi) `quot` 2
                down = roundDown arr half
                mid = if down > lo then down else roundUp arr (half + 1)
         in m <> sliceMetrics arr 0 (search 0 (sizeofByteArray arr))
      Inner _ _ _ cs ->
        let n = sizeofChildren cs
            loop !i !m' !a'
              | i >= n - 1 || p m'' a'' = go m' a' c
              | otherwise = loop (i + 1) m'' a''
              where
                c = indexChildren cs i
                m'' = m' <> nodeMetrics c
                a'' = a' <> nodeAnn c
         in loop 0 m a
{-# INLINABLE metricsWhereNode #-}
{-# SPECIALIZE metricsWhereNode :: (Metrics -> () -> Bool) -> Node () -> Metrics #-}

------------------------------------------------------------------------------
-- Editing the tree

-- | Replace @[k, k + d)@ with a slice of @src@ in one descent, copying the
-- leaf and its path and splitting an overflowing leaf. 'None' if the range
-- spans leaves, the leaf would drop below @least@ ('minChunk', 0 for a root
-- leaf), or it is too big to split. Also returns a free-space estimate.
-- @least@ is an Int, not a Bool, so constructor specialisation cannot run
-- before measure specialisation.
editNode :: Measure a => Int -> Unit -> Int -> Int -> ByteArray -> Int -> Int -> Node a -> (# Result a, Int# #)
editNode !least u !k !d !src !soff !slen node = case node of
  Leaf m _ arr
    | d > 0 && kj > count u m -> (# None, 0# #)
    | slen <= 0 && bj <= bi -> (# None, 0# #)
    | size' < least || size' > 2 * maxChunk - 8 -> (# None, 0# #)
    | size' <= maxChunk -> roomy (One (mkLeafWith m' (spliceArray arr bi bj src soff slen))) (maxChunk - size')
    | otherwise ->
        -- The 8-byte margin above leaves room to align the split to UTF-8.
        let cut = boundary (size' `quot` 2)
            boundary !at
              | isContByte (splicedByte arr bi bj src soff slen at) = boundary (at - 1)
              | otherwise = at
            left = splicedSlice arr bi bj src soff slen 0 cut
            mx = sliceMetrics left 0 cut
         in roomy
              ( Two
                  (mkLeafWith mx left)
                  (mkLeafWith (m' `subMetrics` mx) (splicedSlice arr bi bj src soff slen cut size'))
              )
              (maxChunk - max cut (size' - cut))
    where
      !kj = max 0 k + d
      bi = leafOffset u k m arr
      bj = if d > 0 then leafOffset u kj m arr else bi
      size' = sizeofByteArray arr - (bj - bi) + slen
      kept = if bj > bi then m `subMetrics` sliceMetrics arr bi (bj - bi) else m
      m' = kept <> sliceMetrics src soff slen
  Inner m h _ cs -> case seekUnit u k (count u m) cs of
    Sought c k' ->
      let old = indexChildren cs c
       in case editNode least u k' d src soff slen old of
            (# None, _ #) -> (# None, 0# #)
            (# One new, room #) ->
              (# One (inner h (m <> (nodeMetrics new `subMetrics` nodeMetrics old)) (replaceAt cs c new)), room #)
            (# Two x y, room #) ->
              (# fromChildren h (m <> ((nodeMetrics x <> nodeMetrics y) `subMetrics` nodeMetrics old)) (insert2 cs c x y), room #)
{-# INLINABLE editNode #-}
{-# SPECIALIZE editNode :: Int -> Unit -> Int -> Int -> ByteArray -> Int -> Int -> Node () -> (# Result (), Int# #) #-}

-- | Replace @[i, j)@, returning the free-space estimate from 'editNode'
-- or zero when the general split-and-append path is needed.
editRoot :: Measure a => Unit -> Int -> Int -> Text -> Node a -> (# Node a, Int# #)
editRoot u i j t@(TI.Text (A.ByteArray ba) off len) root =
  case editNode (if nodeHeight root == 0 then 0 else minChunk) u from (max 0 (j - from)) (ByteArray ba) off len root of
    (# One node, room #) -> (# node, room #)
    (# Two x y, room #) -> (# mkInner2 x y, room #)
    (# None, _ #)
      | bi < bj -> (# takeRoot Bytes bi root `appendNode` fromTextNode t `appendNode` dropRoot Bytes bj root, 0# #)
      | len <= 0 -> (# root, 0# #)
      | otherwise -> case splitRoot Bytes bi root of
          (# l, r #) -> (# l `appendNode` fromTextNode t `appendNode` r, 0# #)
  where
    from = max 0 i
    -- Resolve both endpoints in the original rope so rounding is consistent.
    bi = byteOffsetAtNode u i root
    bj = if j <= i then bi else byteOffsetAtNode u j root
{-# INLINABLE editRoot #-}
{-# SPECIALIZE editRoot :: Unit -> Int -> Int -> Text -> Node () -> (# Node (), Int# #) #-}

edited :: Measure a => Unit -> Int -> Int -> Text -> Node a -> Node a
edited u i j t root = case editRoot u i j t root of
  (# root', _ #) -> root'
{-# INLINE edited #-}

------------------------------------------------------------------------------
-- Typing

-- Buffering is sound for Bytes, Chars and Utf16, even for clamped or rounded
-- @i@ (not Lines: inserted text need not end at a line boundary):
--
-- > insert u (max 0 i + n) t2 (insert u i t1 r) == insert u i (t1 <> t2) r
-- >   where n = count u (metrics (fromText t1))
--
-- A run is capped by its leaf's free space (estimated by the previous
-- insertion), so re-applying it after every keystroke does not keep splitting
-- the leaf. A bad estimate costs speed, not correctness.

-- | A rope with a run of keystrokes, of the given metrics, to be inserted at
-- an offset.
typing :: Measure a => Node a -> Unit -> Int -> ByteArray -> Metrics -> Int -> Rope a
typing base u start run typed room =
  -- The lifted wrapper defers the insertion until a reader needs the tree.
  let root = Lazy (edited u start start (chunkText run) base)
   in Typing root base u start run (packMetrics typed) room
{-# INLINE typing #-}

-- | The offset at which a keystroke would continue a run.
typingNext :: Unit -> Int -> PackedMetrics -> Int
typingNext u start typed = start + count u (unpackMetrics typed)
{-# INLINE typingNext #-}

-- | Insert immediately at a new location and remember the endpoint.
-- A subsequent insertion there may start or extend a bounded input buffer.
insertText :: Measure a => Unit -> Int -> Text -> Rope a -> Rope a
insertText u i t@(TI.Text (A.ByteArray ba) off len) r = case r of
  Typing lazyRoot base ru start run typed room
    | typingNext ru start typed == i && ru == u && len <= room ->
        let tm = sliceMetrics src off len
         in typing base u start (concatSlices run 0 (sizeofByteArray run) src off len) (unpackMetrics typed <> tm) (room - len)
    | otherwise -> case lazyRoot of
        Lazy root -> settled root ru (typingNext ru start typed) room
  Settled root hu hint room -> settled root hu hint room
  where
    src = ByteArray ba
    settled root !hu !hint !room
      | hint == i && i >= 0 && hu == u && len <= min room maxPending =
          let tm = sliceMetrics src off len
           in typing root u i (cloneByteArray src off len) tm (min room maxPending - len)
      | otherwise = case editRoot u i i t root of
          (# root', room' #) ->
            let grown = count u (nodeMetrics root') - count u (nodeMetrics root)
             in Settled root' u (if u == Lines then -1 else max 0 i + grown) (I# room')
{-# INLINABLE insertText #-}
{-# SPECIALIZE insertText :: Unit -> Int -> Text -> Rope () -> Rope () #-}

-- | Delete @[i, j)@ for @j > i@. Shorten a buffered suffix directly when
-- both operations use 'Chars' and the buffer's start was not clamped.
deleteRange :: Measure a => Unit -> Int -> Int -> Rope a -> Rope a
deleteRange u i j r = case r of
  Typing _ base Chars start run typed room
    | u == Chars && j == typingNext Chars start typed && i >= start && start <= chars (nodeMetrics base) ->
        let size = sizeofByteArray run
            keep = dropCharsEnd (j - i) run
            typed' = unpackMetrics typed `subMetrics` sliceMetrics run keep (size - keep)
         in if keep <= 0
              then Settled base Chars start (room + size)
              else typing base Chars start (cloneByteArray run 0 keep) typed' (room + size - keep)
  _ -> Rope (edited u i j T.empty (rootOf r))
{-# INLINABLE deleteRange #-}
{-# SPECIALIZE deleteRange :: Unit -> Int -> Int -> Rope () -> Rope () #-}

-- | The size of a chunk without its last @k@ code points.
dropCharsEnd :: Int -> ByteArray -> Int
dropCharsEnd k0 arr = go k0 (sizeofByteArray arr)
  where
    go !k !end
      | k <= 0 || end <= 0 = end
      | otherwise = go (k - 1) (roundDownFrom arr (end - 1))

------------------------------------------------------------------------------
-- Instances

instance Eq (Rope a) where
  Rope a == Rope b = nodeMetrics a == nodeMetrics b && compareNodes a b == EQ

-- | Lexicographic by code point, like 'Text'.
instance Ord (Rope a) where
  compare (Rope a) (Rope b) = compareNodes a b

-- | Compare the UTF-8 (whose byte order is code point order) of two trees
-- with unrelated chunk boundaries.
compareNodes :: Node a -> Node b -> Ordering
compareNodes a b = go (chunksOf a) 0 (chunksOf b) 0
  where
    chunksOf = foldrNode (:) []
    go [] _ [] _ = EQ
    go [] _ _ _ = LT
    go _ _ [] _ = GT
    go xs@(x : xs') !i ys@(y : ys') !j =
      let rx = sizeofByteArray x - i
          ry = sizeofByteArray y - j
          n = min rx ry
       in case compareByteArrays x i y j n of
            EQ
              | rx == ry -> go xs' 0 ys' 0
              | rx < ry -> go xs' 0 ys (j + n)
              | otherwise -> go xs (i + n) ys' 0
            o -> o

instance Show (Rope a) where
  showsPrec p = showsPrec p . toLazyText

instance Measure a => Semigroup (Rope a) where
  (<>) = append
  {-# INLINE (<>) #-}

instance Measure a => Monoid (Rope a) where
  mempty = empty
  {-# INLINE mempty #-}

instance Measure a => IsString (Rope a) where
  fromString = fromText . T.pack
  {-# INLINE fromString #-}

instance NFData a => NFData (Rope a) where
  rnf (Rope root) = go root
    where
      go (Leaf _ a _) = rnf a
      go (Inner _ _ a cs) = rnf a `seq` children 0
        where
          children !i
            | i >= sizeofChildren cs = ()
            | otherwise = go (indexChildren cs i) `seq` children (i + 1)

------------------------------------------------------------------------------
-- Construction

-- | The empty rope.
empty :: Measure a => Rope a
empty = Rope emptyNode
{-# INLINE empty #-}

-- | A rope of one character.
singleton :: Measure a => Char -> Rope a
singleton = fromText . T.singleton
{-# INLINE singleton #-}

-- | /O(n)/. Build a rope from strict text. Copies the text into chunks,
-- unless it is at most 'maxChunk' bytes and occupies its entire backing buffer.
fromText :: Measure a => Text -> Rope a
fromText t = Rope (fromTextNode t)
{-# INLINE fromText #-}

-- | Build a rope by appending the chunks of a lazy 'TL.Text'.
fromLazyText :: Measure a => TL.Text -> Rope a
fromLazyText = TL.foldlChunks (\acc t -> acc <> fromText t) empty
{-# INLINABLE fromLazyText #-}

------------------------------------------------------------------------------
-- Deconstruction

-- | /O(n)/. Flatten the rope to strict text. A single chunk is shared
-- without copying; multiple chunks are copied into one buffer.
toText :: Rope a -> Text
toText (Rope root) = sliceToText 0 (nodeBytes root) root

-- | /O(n)/. Convert to lazy text, sharing the chunk buffers.
toLazyText :: Rope a -> TL.Text
toLazyText = TL.fromChunks . toChunks

-- | /O(n)/. Decode the rope to a 'String'.
toString :: Rope a -> String
toString = TL.unpack . toLazyText

-- | The chunks of the rope as zero-copy views, in order. They are non-empty,
-- at most 'maxChunk' bytes long and produced lazily.
toChunks :: Rope a -> [Text]
toChunks = foldrChunks (:) []

-- | Lazy right fold over non-empty chunks in document order, without
-- building the list returned by 'toChunks'.
foldrChunks :: (Text -> b -> b) -> b -> Rope a -> b
-- Keep the rope argument behind a lambda, here and in foldlChunks', so GHC
-- can inline a partial application and specialise the per-chunk function.
foldrChunks f z = \(Rope root) -> foldrNode (f . chunkText) z root
{-# INLINE foldrChunks #-}

foldrNode :: (ByteArray -> b -> b) -> b -> Node a -> b
foldrNode f = go
  where
    go z (Leaf _ _ arr)
      | sizeofByteArray arr == 0 = z
      | otherwise = f arr z
    go z (Inner _ _ _ cs) = children 0
      where
        children !i
          | i >= sizeofChildren cs = z
          | otherwise = go (children (i + 1)) (indexChildren cs i)
{-# INLINE foldrNode #-}

-- | Strict left fold over non-empty chunks in document order. Walks the
-- tree directly, sharing text buffers and avoiding an intermediate list.
-- Useful for consumers such as hashes and parsers.
foldlChunks' :: (b -> Text -> b) -> b -> Rope a -> b
foldlChunks' f z = \(Rope root) -> foldlNode' (\acc arr -> f acc (chunkText arr)) z root
{-# INLINE foldlChunks' #-}

foldlNode' :: (b -> ByteArray -> b) -> b -> Node a -> b
foldlNode' f = go
  where
    go !acc (Leaf _ _ arr)
      | sizeofByteArray arr == 0 = acc
      | otherwise = f acc arr
    go !acc (Inner _ _ _ cs) = children acc 0
      where
        children !acc' !i
          | i >= sizeofChildren cs = acc'
          | otherwise = children (go acc' (indexChildren cs i)) (i + 1)
{-# INLINE foldlNode' #-}

------------------------------------------------------------------------------
-- Output

-- | /O(n)/. Write UTF-8 to a handle through a fixed-size buffer, without
-- constructing a 'Text' for the whole document. See 'outputBuffer'.
--
-- Like 'hPutBuf', this bypasses the handle's encoding and newline
-- translation, preserving the rope's bytes on every platform. To use the
-- handle's text encoding instead, pass 'toLazyText' to text I/O.
hPutUtf8 :: Handle -> Rope a -> IO ()
hPutUtf8 h (Rope root) = do
  buf <- newPinnedByteArray outputBuffer
  withMutableByteArrayContents buf $ \ptr -> do
    I# used <- pourNode h buf ptr 0 root
    flushBuffer h ptr used

-- | Copy a subtree into the buffer from @used@, flushing when a chunk will
-- not fit; return the bytes still buffered. Top level so the count unboxes.
pourNode :: Handle -> MutableByteArray RealWorld -> Ptr Word8 -> Int -> Node a -> IO Int
pourNode h !buf !ptr used@(I# used#) node = case node of
  Leaf _ _ arr
    | used + size <= outputBuffer -> used + size <$ copyByteArray buf used arr 0 size
    | otherwise -> do
        flushBuffer h ptr used#
        size <$ copyByteArray buf 0 arr 0 size
    where
      size = sizeofByteArray arr
  Inner _ _ _ cs -> go 0 used
    where
      n = sizeofChildren cs
      go !c !used'
        | c >= n = pure used'
        | otherwise = pourNode h buf ptr used' (indexChildren cs c) >>= go (c + 1)

-- | Write the used part of the buffer. Takes 'Int#' so the box needed by
-- 'hPutBuf' does not spread into 'pourNode'.
flushBuffer :: Handle -> Ptr Word8 -> Int# -> IO ()
flushBuffer h ptr used# = when (used > 0) $ hPutBuf h ptr used
  where
    used = I# used#
{-# NOINLINE flushBuffer #-}

-- | Write UTF-8 to a file with 'hPutUtf8', replacing its contents. Forces
-- the tree (pending input, annotations to WHNF) before opening the file, so
-- a failure there leaves the file untouched. The write is not atomic.
writeFileUtf8 :: FilePath -> Rope a -> IO ()
writeFileUtf8 path rope@(Rope _) = withBinaryFile path WriteMode (`hPutUtf8` rope)

-- | /O(log n)/. Zero-copy view of the rest of the chunk containing an
-- offset (clamped and rounded as at 'Unit'); empty at the end. For a parser
-- read callback, ask for a byte offset and advance by the result's length.
chunkAt :: Unit -> Int -> Rope a -> Text
chunkAt u k (Rope root)
  | b >= nodeBytes root = T.empty
  | otherwise = go b root
  where
    b = byteOffsetAtNode u k root
    go !i node = case node of
      Leaf _ _ arr -> viewSlice arr i (sizeofByteArray arr - i)
      Inner _ _ _ cs -> case seekByte i cs of
        Sought c i' -> go i' (indexChildren cs c)

------------------------------------------------------------------------------
-- Queries

-- | /O(1)/. Whether the rope is empty, including pending input.
null :: Rope a -> Bool
null r = bytes (metrics r) == 0
{-# INLINE null #-}

-- | /O(1)/. Length in any unit; for 'Lines' this is the number of @\\n@.
length :: Unit -> Rope a -> Int
length u = count u . metrics
{-# INLINE length #-}

-- | /O(1)/. Number of @\\n@ characters plus one. An empty rope has one line;
-- a trailing @\\n@ adds an empty final line. Valid indices range from zero
-- to @lineCount rope - 1@. See 'lines' for a list that omits that final empty line.
lineCount :: Rope a -> Int
lineCount r = newlines (metrics r) + 1
{-# INLINE lineCount #-}

-- | /O(1)/. All built-in measurements, including pending input.
metrics :: Rope a -> Metrics
metrics (Settled root _ _ _) = nodeMetrics root
metrics (Typing _ base _ _ _ typed _) = nodeMetrics base <> unpackMetrics typed
{-# INLINE metrics #-}

-- | /O(1)/ on an evaluated tree. Return the cached custom measure.
-- Applies any pending insertion first, which may take /O(log n)/ plus
-- the cost of updating the measure.
measure :: Rope a -> a
measure (Rope root) = nodeAnn root
{-# INLINE measure #-}

-- | Number of levels of inner nodes above the leaves.
height :: Rope a -> Int
height (Rope root) = nodeHeight root

------------------------------------------------------------------------------
-- Combining and breaking

-- | /O(log n)/. Concatenate two ropes, sharing unaffected subtrees.
-- Equivalent to '<>'. The traversal follows the difference in tree heights.
append :: Measure a => Rope a -> Rope a -> Rope a
append (Rope l) (Rope r) = Rope (appendNode l r)
{-# INLINABLE append #-}

-- | /O(log n)/. Split at an offset, clamped to the rope and rounded down to
-- a code point boundary (see 'Unit'). Finds both halves in one descent.
-- Use 'take' or 'drop' if you need only one half.
--
-- >>> splitAt Lines 1 "fst\nsnd\n"
-- ("fst\n","snd\n")
splitAt :: Measure a => Unit -> Int -> Rope a -> (Rope a, Rope a)
splitAt u k (Rope root) = case splitRoot u k root of
  (# l, r #) -> (Rope l, Rope r)
{-# INLINABLE splitAt #-}

-- | /O(log n)/. The prefix before an offset, clamped and rounded as in 'splitAt'.
take :: Measure a => Unit -> Int -> Rope a -> Rope a
take u k (Rope root) = Rope (takeRoot u k root)
{-# INLINABLE take #-}

-- | /O(log n)/. The suffix from an offset, clamped and rounded as in 'splitAt'.
drop :: Measure a => Unit -> Int -> Rope a -> Rope a
drop u k (Rope root) = Rope (dropRoot u k root)
{-# INLINABLE drop #-}

-- | /O(log n)/. Extract the half-open range @[i, j)@. Both offsets are
-- clamped and rounded in the original rope. Returns empty when @j <= i@.
-- Descends both endpoints together, avoiding reconstruction above the
-- lowest node containing the range.
slice :: Measure a => Unit -> Int -> Int -> Rope a -> Rope a
slice u i j (Rope root)
  | j <= i || j <= 0 = empty
  | beyondEnd u j (nodeMetrics root) = Rope (dropRoot u i root)
  | otherwise = Rope (sliceNode u (max 0 i) j root)
{-# INLINABLE slice #-}

-- | The lowest node containing a range, with both offsets relative to it.
data Sliced a = Sliced !(Node a) {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | Descend while both ends of @[i, j)@ fall in one child, for @0 <= i < j@
-- and @j@ not beyond the end. Shared by 'slice' and 'sliceText'.
sliceDescend :: Unit -> Int -> Int -> Node a -> Sliced a
sliceDescend !u !i !j node = case node of
  Leaf{} -> Sliced node i j
  Inner total _ _ cs -> case seekUnit u i (count u total) cs of
    Sought c i'
      | j' <= count u (nodeMetrics child) -> sliceDescend u i' j' child
      | otherwise -> Sliced node i j
      where
        child = indexChildren cs c
        j' = j - (i - i')

-- | The text from offset @i@ up to offset @j@ of a node, for @0 <= i < j@
-- and @j@ not beyond its end.
sliceNode :: Measure a => Unit -> Int -> Int -> Node a -> Node a
sliceNode u i j root = case sliceDescend u i j root of
  Sliced node@(Leaf m _ arr) i' j'
    | bi <= 0 && bj >= sizeofByteArray arr -> node
    | bj <= bi -> emptyNode
    | otherwise -> mkLeafWith (leafSliceMetrics m arr bi (bj - bi)) (cloneByteArray arr bi (bj - bi))
    where
      !bi = leafOffset u i' m arr
      !bj = leafOffset u j' m arr
  -- Cutting the tail does not move where @i'@ rounds to.
  Sliced node i' j' -> dropRoot u i' (takeRoot u j' node)
{-# INLINABLE sliceNode #-}
{-# SPECIALIZE sliceNode :: Unit -> Int -> Int -> Node () -> Node () #-}

-- | /O(log n + result bytes)/. Like 'slice', but returns 'Text' directly.
-- A range within one chunk is found in one descent and returned as a
-- zero-copy view; a range spanning chunks is copied into one buffer.
sliceText :: Unit -> Int -> Int -> Rope a -> Text
sliceText u i j (Rope root)
  | j <= i || j <= 0 = T.empty
  | beyondEnd u j (nodeMetrics root) = sliceToText (byteOffsetAtNode u i root) (nodeBytes root) root
  | otherwise = sliceTextNode u (max 0 i) j root

-- | 'sliceNode' as a 'Text'.
sliceTextNode :: Unit -> Int -> Int -> Node a -> Text
sliceTextNode u i j root = case sliceDescend u i j root of
  Sliced (Leaf m _ arr) i' j' ->
    let !bi = leafOffset u i' m arr
        !bj = leafOffset u j' m arr
     in viewSlice arr bi (bj - bi)
  Sliced node i' j' -> sliceToText (byteOffsetAtNode u i' node) (byteOffsetAtNode u j' node) node

------------------------------------------------------------------------------
-- Editing

-- | /O(log n + inserted bytes)/. Insert text at a clamped, code-point-aligned
-- offset; empty input is a no-op. Copies only the target chunk and its path.
--
-- Consecutive insertions in the same unit ('Bytes', 'Chars' or 'Utf16') are
-- buffered in /O(1)/, up to 'maxPending' bytes and the chunk's free space.
-- A tree read, an edit elsewhere or a full buffer applies it; 'length' and
-- 'metrics' do not, and WHNF may leave it pending.
insert :: Measure a => Unit -> Int -> Text -> Rope a -> Rope a
insert u i t r
  | T.null t = r
  | otherwise = insertText u i t r
{-# INLINABLE insert #-}

-- | /O(log n)/. Remove the half-open range @[i, j)@, clamping and rounding
-- both offsets in the original rope. Does nothing when @j <= i@.
-- Deleting a suffix of buffered 'Chars' input can take /O(1)/; see 'insert'.
delete :: Measure a => Unit -> Int -> Int -> Rope a -> Rope a
delete u i j = replace u i j T.empty
{-# INLINABLE delete #-}

-- | /O(log n + inserted bytes)/. Replace the half-open range @[i, j)@ with
-- text, clamping and rounding both offsets in the original rope. When
-- @j <= i@, insert at @i@ instead.
--
-- An edit that stays within one chunk and keeps it within its size bounds
-- copies only that chunk and the path to it.
replace :: Measure a => Unit -> Int -> Int -> Text -> Rope a -> Rope a
replace u i j t r
  | j <= i = insert u i t r
  | T.null t = deleteRange u i j r
  | otherwise = Rope (edited u i j t (rootOf r))
{-# INLINABLE replace #-}

------------------------------------------------------------------------------
-- Lines

-- | /O(log n + length of the line)/. The content of a line by 0-based index,
-- without its terminating @\\n@ or @\\r\\n@; empty if there is no such line.
-- A line within a single chunk is returned as a zero-copy view.
getLine :: Int -> Rope a -> Text
getLine l (Rope root)
  | l < 0 = T.empty
  | otherwise = lineText l root

-- | /O(n)/. Lines without their @\\n@ or @\\r\\n@ terminators, produced
-- lazily. Returns @[]@ for an empty rope and omits the empty line after a
-- trailing @\\n@. A lone @\\r@ is preserved. Lines within one chunk share
-- its buffer.
lines :: Rope a -> [Text]
lines (Rope root) = go [] (foldrNode (:) [] root)
  where
    -- Carry non-empty pieces of an unfinished line in reverse order.
    go carry [] = [T.concat (reverse carry) | not (L.null carry)]
    go carry (arr : arrs) = from carry arr 0 arrs
    from carry arr i arrs
      | i >= size = go carry arrs
      | lf >= size = go (viewSlice arr i (size - i) : carry) arrs
      | otherwise = stripCR (T.concat (reverse (viewSlice arr i (lf - i) : carry))) : from [] arr (lf + 1) arrs
      where
        size = sizeofByteArray arr
        lf = findNewline arr i
    stripCR t
      | not (T.null t) && T.last t == '\r' = T.init t
      | otherwise = t

------------------------------------------------------------------------------
-- Conversions

-- | /O(log n)/. Measure the prefix ending at an offset to express that
-- location in all four units. The offset is clamped and rounded as in 'splitAt'.
--
-- >>> metricsAt Chars 3 "a😀\nb"
-- Metrics {bytes = 6, chars = 3, utf16Units = 4, newlines = 1}
metricsAt :: Unit -> Int -> Rope a -> Metrics
metricsAt u k (Rope root) = metricsAtNode u k root

-- | /O(log n)/. @convert from to@ re-expresses an offset in another unit.
-- Converting to 'Lines' gives the index of the line containing the offset,
-- converting from 'Lines' the offset of the start of a line.
--
-- >>> convert Bytes Utf16 5 "a😀\nb"
-- 3
convert :: Unit -> Unit -> Int -> Rope a -> Int
convert from to k = count to . metricsAt from k
{-# INLINE convert #-}

------------------------------------------------------------------------------
-- Positions

-- | /O(log n)/. Split at a zero-based line and column, with the column in
-- the given unit. Negative coordinates clamp to zero. A column beyond the
-- line's content clamps to before its @\\n@ or @\\r\\n@; a line beyond the
-- document clamps to its end. Offsets inside code points round down.
-- For 'Lines' columns, zero means the line start and any positive value
-- means the end of its content.
splitAtPosition :: Measure a => Unit -> Position -> Rope a -> (Rope a, Rope a)
splitAtPosition u pos r = splitAt Bytes (bytes (metricsAtPosition u pos r)) r
{-# INLINE splitAtPosition #-}

-- | /O(log n)/. The location of a position in every unit, clamped like
-- 'splitAtPosition'.
metricsAtPosition :: Unit -> Position -> Rope a -> Metrics
metricsAtPosition u pos (Rope root) = case linePositionNode False u pos root of
  Span _ at -> at

-- | /O(log n)/. Return prefix metrics for the line start and the position,
-- sharing their lookup. Clamps coordinates as in 'metricsAtPosition'.
-- Subtract corresponding counts to get the reached column in any unit.
-- Comparing it with the requested column detects clamping or rounding:
--
-- >>> let (line, at) = metricsAtLineAndPosition Utf16 (Position 1 3) "a😀\nb😀c"
-- >>> (utf16Units at - utf16Units line, chars at - chars line, bytes at)
-- (3,2,11)
metricsAtLineAndPosition :: Unit -> Position -> Rope a -> (Metrics, Metrics)
metricsAtLineAndPosition u pos (Rope root) = case linePositionNode True u pos root of
  Span line at -> (line, at)
{-# INLINE metricsAtLineAndPosition #-}

-- | /O(log n)/. The position, with its column in the given unit, of a
-- location obtained from 'metricsAt', 'metricsAtPosition', or 'metricsWhere'
-- on the same rope. Does not clamp or validate the supplied metrics.
metricsToPosition :: Unit -> Metrics -> Rope a -> Position
metricsToPosition u m (Rope root) = positionOfMetrics u m root

-- | /O(log n)/. @offsetToPosition from to@ turns an offset in unit @from@
-- into a position with its column in unit @to@.
-- An offset inside a line terminator remains there; converting the result
-- back with 'positionToOffset' clamps it to the end of the line's content.
--
-- >>> offsetToPosition Bytes Utf16 11 "a😀\nb😀c"
-- Position {posLine = 1, posColumn = 3}
offsetToPosition :: Unit -> Unit -> Int -> Rope a -> Position
offsetToPosition from to k (Rope root) = positionAtNode from to k root

-- | /O(log n)/. @positionToOffset from to@ turns a position with its column
-- in unit @from@ into an offset in unit @to@.
-- Coordinates are clamped as in 'splitAtPosition'.
--
-- >>> positionToOffset Utf16 Bytes (Position 1 3) "a😀\nb😀c"
-- 11
positionToOffset :: Unit -> Unit -> Position -> Rope a -> Int
positionToOffset from to pos = count to . metricsAtPosition from pos
{-# INLINE positionToOffset #-}

------------------------------------------------------------------------------
-- Searching by measure

-- | /O(log n)/ for constant-time measure combination and predicates.
-- Split after the longest code-point-aligned prefix for which the predicate
-- is false. The predicate receives both built-in metrics and the custom
-- measure, and must stay true once it becomes true as the prefix grows.
--
-- If true for the empty prefix, split at the start; if never true, split
-- at the end. See "Data.Text.NanoRope.Measured" for a tab-count example.
splitWhere :: Measure a => (Metrics -> a -> Bool) -> Rope a -> (Rope a, Rope a)
splitWhere p r = splitAt Bytes (bytes (metricsWhere p r)) r
{-# INLINE splitWhere #-}

-- | Prefix metrics at the split point chosen by 'splitWhere', without
-- constructing either half. Has the same search cost as 'splitWhere'.
metricsWhere :: Measure a => (Metrics -> a -> Bool) -> Rope a -> Metrics
metricsWhere p (Rope root) = metricsWhereNode p root
{-# INLINABLE metricsWhere #-}

-- | /O(n)/. Annotate the same text with another measure. The text itself is
-- shared, not copied.
remeasure :: forall a b. Measure b => Rope a -> Rope b
remeasure (Rope root) = Rope (go root)
  where
    go :: Node a -> Node b
    go (Leaf m _ arr) = mkLeafWith m arr
    go (Inner m h _ cs) = inner h m (generateChildren (sizeofChildren cs) (\i -> go (indexChildren cs i)))
{-# INLINABLE remeasure #-}

------------------------------------------------------------------------------
-- Debugging

-- | List violated tree invariants. Returns @[]@ for ropes built through
-- the public API with a lawful 'Measure'.
invariants :: (Measure a, Eq a) => Rope a -> [String]
invariants rope = case rope of
  Settled root _ _ _ -> go True root
  Typing (Lazy root) base u start run typed room ->
    go True root
      ++ map ("without what was typed: " ++) (go True base)
      ++ [ "a run of " ++ show (sizeofByteArray run) ++ " bytes with room for " ++ show room | sizeofByteArray run <= 0 || room < 0 || sizeofByteArray run + room > maxPending ]
      ++ [ "a run by lines" | u == Lines ]
      ++ [ "a run at " ++ show start | start < 0 ]
      ++ [ "the run caches " ++ show (unpackMetrics typed) ++ " instead of " ++ show (naive run) | unpackMetrics typed /= naive run ]
      ++ [ "the rope reports " ++ show (metrics rope) ++ " instead of " ++ show (nodeMetrics root) | metrics rope /= nodeMetrics root ]
  where
    go :: (Measure a, Eq a) => Bool -> Node a -> [String]
    go isRoot node = case node of
      Leaf m a arr ->
        let size = sizeofByteArray arr
         in [ "leaf of " ++ show size ++ " bytes is too large" | size > maxChunk ]
              ++ [ "leaf of " ++ show size ++ " bytes is too small" | not isRoot, size < minChunk ]
              ++ [ "leaf starts inside a code point" | size > 0, isContByte (byteAt arr 0) ]
              ++ [ "leaf caches " ++ show m ++ " instead of " ++ show (naive arr) | m /= naive arr ]
              ++ [ "leaf caches a wrong annotation" | a /= measureChunk (chunkText arr) ]
      Inner m h a cs ->
        let n = sizeofChildren cs
            -- Lists require lifted elements, so wrap each unlifted node.
            kids = [Lazy (indexChildren cs i) | i <- [0 .. n - 1]]
            total = mconcat [nodeMetrics c | Lazy c <- kids]
         in [ "inner node with " ++ show n ++ " children is too large" | n > maxChildren ]
              ++ [ "inner node with " ++ show n ++ " children is too small" | n < (if isRoot then 2 else minChildren) ]
              ++ [ "child of height " ++ show (nodeHeight c) ++ " below a node of height " ++ show h | Lazy c <- kids, nodeHeight c /= h - 1 ]
              ++ [ "inner node caches " ++ show m ++ " instead of " ++ show total | m /= total ]
              ++ [ "inner node caches a wrong annotation" | a /= mconcat [nodeAnn c | Lazy c <- kids] ]
              ++ concat [go False c | Lazy c <- kids]
    naive arr =
      let bs = [ byteAt arr i | i <- [0 .. sizeofByteArray arr - 1] ]
          cs = L.length (filter (not . isContByte) bs)
       in Metrics (L.length bs) cs (cs + L.length (filter (>= 0xF0) bs)) (L.length (filter (== 0x0A) bs))
