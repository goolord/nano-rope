{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
-- Constructor specialisation clones the recursive workers before they can be
-- specialised to a measure, and the clones then take the measure as a
-- dictionary at run time. The SPECIALIZE pragmas below are what we want.
{-# OPTIONS_GHC -fno-spec-constr #-}

-- |
-- Module      : Data.Text.NanoRope.Internal
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- Internals of the rope: the B-tree representation, the chunk scanning
-- primitives and an invariant checker. Everything is exposed for testing,
-- benchmarking and the adventurous, with __no stability guarantees__.
-- Import "Data.Text.NanoRope" or "Data.Text.NanoRope.Measured" instead.
--
-- = Representation
--
-- A rope is a B-tree. Leaves hold a flat, unpinned, exactly-sized
-- 'ByteArray' of UTF-8 (at most 'maxChunk' bytes, always cut at code point
-- boundaries). Inner nodes hold up to 'maxChildren' children. Every node
-- carries the 'Metrics' of its own subtree unpacked next to its header, so
-- seeking by any unit is a linear scan over the heads of at most
-- 'maxChildren' children per level.
--
-- Nothing about a child is kept in its parent but the pointer. An edit
-- therefore copies one small array of pointers per level: persistence is
-- paid for in allocation, and this is what keeps the bill short.
module Data.Text.NanoRope.Internal
  ( -- * Types
    Rope (.., Rope)
  , Node (..)
  , Measure (..)
  , Metrics (..)
  , Unit (..)
  , Position (..)
  , count
  , subMetrics

    -- * Tuning constants
  , maxChunk
  , minChunk
  , maxChildren
  , minChildren
  , maxPending

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
  , chunkAt

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
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits (complement, unsafeShiftL, unsafeShiftR, xor, (.&.), (.|.))
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Primitive.ByteArray
import Data.Primitive.SmallArray
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import qualified Data.Text.Lazy as TL
import Data.Word (Word8)
import GHC.Exts (Int (..), indexWord8ArrayAsWord64#)
import GHC.Word (Word64 (..))
import Prelude hiding (drop, getLine, length, lines, null, splitAt, take)

------------------------------------------------------------------------------
-- Tuning constants

-- | Maximum number of bytes in a leaf.
maxChunk :: Int

-- | Maximum number of children of an inner node.
maxChildren :: Int
#ifdef NANO_ROPE_SMALL
-- Tiny nodes, so that the test suite grows deep trees out of short inputs.
-- Six children rather than four, because an inner node can only be undersized
-- if the minimum is more than two.
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

-- | Maximum number of bytes of keystrokes waiting to be inserted, see
-- 'Typing'. Every keystroke copies what is waiting, so this is kept well
-- below a chunk.
maxPending :: Int
maxPending = maxChunk `quot` 4

------------------------------------------------------------------------------
-- Metrics

-- | The built-in measurements of a piece of text, all maintained at every
-- node of the tree. 'Metrics' of a /prefix/ of a rope double as a location
-- expressed in every unit at once, see 'metricsAt'.
data Metrics = Metrics
  { bytes :: {-# UNPACK #-} !Int
  -- ^ UTF-8 code units.
  , chars :: {-# UNPACK #-} !Int
  -- ^ Unicode code points.
  , utf16Units :: {-# UNPACK #-} !Int
  -- ^ UTF-16 code units, as used by the Language Server Protocol.
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

-- | A unit for offsets and lengths.
data Unit
  = -- | UTF-8 code units. An offset inside a code point is rounded down to
    -- the start of that code point.
    Bytes
  | -- | Unicode code points.
    Chars
  | -- | UTF-16 code units. An offset between the two halves of a surrogate
    -- pair is rounded down to the start of that code point.
    Utf16
  | -- | Lines. Offset @n@ is the start of the @n@-th line (0-based), that is
    -- the location just after the @n@-th @\\n@.
    Lines
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Project one unit out of 'Metrics'.
count :: Unit -> Metrics -> Int
count Bytes = bytes
count Chars = chars
count Utf16 = utf16Units
count Lines = newlines
{-# INLINE count #-}

-- | A 0-based line and a 0-based column within that line. The unit of the
-- column is given separately to the functions consuming a 'Position'.
data Position = Position
  { posLine :: !Int
  , posColumn :: !Int
  }
  deriving (Eq, Ord, Show)

instance NFData Position where
  rnf !_ = ()

------------------------------------------------------------------------------
-- Custom measures

-- | A user-defined monoidal summary of text, cached at every node of the
-- tree alongside the built-in 'Metrics'.
--
-- Chunk boundaries are an implementation detail and can fall between any two
-- code points, so 'measureChunk' must be a monoid homomorphism:
--
-- > measureChunk (x <> y) == measureChunk x <> measureChunk y
-- > measureChunk mempty   == mempty
--
-- Measures which seem to need context across a boundary can usually be made
-- lawful by remembering a little about their edges: to count @\\r\\n@ as a
-- single line break, track whether a piece ends in @\\r@ and whether it starts
-- with @\\n@, and fix the count up in '<>'.
--
-- Annotations are kept in weak head normal form; give your measure strict
-- fields to avoid building up thunks.
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

-- | A rope of text annotated with a custom measure @a@. Use @()@ (or the
-- monomorphic interface in "Data.Text.NanoRope") when the built-in 'Metrics'
-- are all you need.
--
-- To everything but 'insert' and 'delete' a rope is its tree, which is what
-- the pattern v'Rope' matches and builds.
data Rope a
  = -- | A tree, and where the last insertion into it ended, as a unit and an
    -- offset: the one place at which a keystroke would continue it. The
    -- offset is negative if there is no such place. Last, the room left in
    -- the leaf that insertion went to, as far as known.
    Settled
      !(Node a)
      !Unit
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | -- | A tree and a run of keystrokes at one spot of it which are yet to be
    -- inserted. Typing on appends to the run, copying neither a leaf nor the
    -- path to it; whoever wants to read the rope gets the first field, the
    -- one lazy thing in here, which inserts the whole run at once.
    --
    -- The others: the tree without the run, the unit and the offset at
    -- which the run is to go in, the offset at which a keystroke would
    -- continue it, its text (at most 'maxPending' bytes), the metrics of
    -- everything and the room left for the run to grow.
    Typing
      (Node a)
      !(Node a)
      !Unit
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !Metrics
      {-# UNPACK #-} !Int

-- | The tree of a rope, with everything typed in it.
pattern Rope :: Node a -> Rope a
pattern Rope root <- (rootOf -> root)
  where
    Rope root = Settled root Bytes (-1) 0

{-# COMPLETE Rope #-}

rootOf :: Rope a -> Node a
rootOf (Settled root _ _ _) = root
rootOf (Typing root _ _ _ _ _ _ _) = root
{-# INLINE rootOf #-}

-- | A node of the B-tree. Every field is strict, so a tree in weak head
-- normal form is fully built.
--
-- Both constructors start with the metrics of their subtree, which is all
-- that seeking reads of a node it does not descend into.
data Node a
  = -- | Metrics, annotation and UTF-8 payload. The payload occupies the
    -- whole array: there is no offset or length to chase.
    Leaf
      {-# UNPACK #-} !Metrics
      !a
      {-# UNPACK #-} !ByteArray
  | -- | Metrics, height (at least 1), annotation and children.
    Inner
      {-# UNPACK #-} !Metrics
      {-# UNPACK #-} !Int
      !a
      {-# UNPACK #-} !(SmallArray (Node a))

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
nodeBytes = bytes . nodeMetrics
{-# INLINE nodeBytes #-}

nodeIsEmpty :: Node a -> Bool
nodeIsEmpty node = nodeBytes node == 0
{-# INLINE nodeIsEmpty #-}

emptyNode :: Monoid a => Node a
emptyNode = Leaf mempty mempty emptyByteArray

------------------------------------------------------------------------------
-- Seeking

-- | The child holding offset @k@ of a unit: the first one at which the
-- running total reaches @k@, or the last one. Comes with the metrics of the
-- children in front of it.
--
-- The unit is dispatched on once, in front of the loop.
seekChild :: Unit -> Int -> SmallArray (Node a) -> (# Int, Metrics #)
seekChild u !k !cs = case u of
  Bytes -> scan bytes
  Chars -> scan chars
  Utf16 -> scan utf16Units
  Lines -> scan newlines
  where
    n = sizeofSmallArray cs
    scan sel = go 0 mempty
      where
        go !i !acc
          | i >= n - 1 || sel acc' >= k = (# i, acc #)
          | otherwise = go (i + 1) acc'
          where
            acc' = acc <> nodeMetrics (indexSmallArray cs i)
    {-# INLINE scan #-}
{-# INLINE seekChild #-}

-- | 'seekChild' for when nothing but the unit sought matters: the child and
-- the offset relative to it.
seekUnit :: Unit -> Int -> SmallArray (Node a) -> (# Int, Int #)
seekUnit u !k !cs = case u of
  Bytes -> scan bytes
  Chars -> scan chars
  Utf16 -> scan utf16Units
  Lines -> scan newlines
  where
    n = sizeofSmallArray cs
    scan sel = go 0 k
      where
        go !i !j
          | i >= n - 1 || m >= j = (# i, j #)
          | otherwise = go (i + 1) (j - m)
          where
            m = sel (nodeMetrics (indexSmallArray cs i))
    {-# INLINE scan #-}
{-# INLINE seekUnit #-}

-- | 'seekUnit' which also counts the bytes in front of the child.
seekUnitBytes :: Unit -> Int -> SmallArray (Node a) -> (# Int, Int, Int #)
seekUnitBytes u !k !cs = case u of
  Bytes -> case seekUnit Bytes k cs of (# i, j #) -> (# i, j, k - j #)
  Chars -> scan chars
  Utf16 -> scan utf16Units
  Lines -> scan newlines
  where
    n = sizeofSmallArray cs
    scan sel = go 0 k 0
      where
        go !i !j !b
          | i >= n - 1 || sel m >= j = (# i, j, b #)
          | otherwise = go (i + 1) (j - sel m) (b + bytes m)
          where
            m = nodeMetrics (indexSmallArray cs i)
    {-# INLINE scan #-}
{-# INLINE seekUnitBytes #-}

-- | The child holding the byte at offset @i@: the first one whose running
-- total exceeds @i@, or the last one. Comes with the offset relative to it.
seekByte :: Int -> SmallArray (Node a) -> (# Int, Int #)
seekByte !i !cs = go 0 i
  where
    n = sizeofSmallArray cs
    go !c !j
      | c >= n - 1 || j < m = (# c, j #)
      | otherwise = go (c + 1) (j - m)
      where
        m = nodeBytes (indexSmallArray cs c)
{-# INLINE seekByte #-}

------------------------------------------------------------------------------
-- Building inner nodes

-- | The annotation of a node with these children. A right fold, which for
-- the measure @()@ never gets going, because its '<>' does not look.
foldAnn :: Monoid a => SmallArray (Node a) -> a
foldAnn cs = go 0
  where
    n = sizeofSmallArray cs
    go !i
      | i >= n - 1 = nodeAnn (indexSmallArray cs i)
      | otherwise = nodeAnn (indexSmallArray cs i) <> go (i + 1)
{-# INLINE foldAnn #-}

-- | Metrics of children @off .. off + cnt - 1@.
sumMetrics :: SmallArray (Node a) -> Int -> Int -> Metrics
sumMetrics cs off cnt = go off mempty
  where
    end = off + cnt
    go !i !acc
      | i >= end = acc
      | otherwise = go (i + 1) (acc <> nodeMetrics (indexSmallArray cs i))

-- | An inner node of known height and metrics, out of at least one child.
-- Most nodes are built from pieces of others, whose metrics add up without
-- another look at the children.
inner :: Monoid a => Int -> Metrics -> SmallArray (Node a) -> Node a
inner h m cs = Inner m h (foldAnn cs) cs
{-# INLINE inner #-}

-- | Build an inner node out of at least one child.
mkInner :: Monoid a => SmallArray (Node a) -> Node a
mkInner cs = inner (nodeHeight (indexSmallArray cs 0) + 1) (sumMetrics cs 0 (sizeofSmallArray cs)) cs
{-# INLINABLE mkInner #-}
{-# SPECIALIZE mkInner :: SmallArray (Node ()) -> Node () #-}

mkInner2 :: Monoid a => Node a -> Node a -> Node a
mkInner2 x y = inner (nodeHeight x + 1) (nodeMetrics x <> nodeMetrics y) $ runSmallArray $ do
  m <- newSmallArray 2 x
  writeSmallArray m 1 y
  pure m
{-# INLINABLE mkInner2 #-}
{-# SPECIALIZE mkInner2 :: Node () -> Node () -> Node () #-}

replaceAt :: SmallArray a -> Int -> a -> SmallArray a
replaceAt arr i x = runSmallArray $ do
  m <- thawSmallArray arr 0 (sizeofSmallArray arr)
  writeSmallArray m i x
  pure m

-- | The first @cnt@ elements followed by two more. One allocation if the
-- array has two elements to spare, which are copied along and overwritten.
snoc2 :: SmallArray a -> Int -> a -> a -> SmallArray a
snoc2 arr cnt x y = runSmallArray $ do
  m <-
    if cnt + 2 <= sizeofSmallArray arr
      then thawSmallArray arr 0 (cnt + 2)
      else do
        out <- newSmallArray (cnt + 2) y
        copySmallArray out 0 arr 0 cnt
        pure out
  writeSmallArray m cnt x
  writeSmallArray m (cnt + 1) y
  pure m

-- | Two elements followed by all but the first @off@ of an array.
cons2 :: a -> a -> SmallArray a -> Int -> SmallArray a
cons2 x y arr off = runSmallArray $ do
  let cnt = sizeofSmallArray arr - off
  m <-
    if off >= 2
      then thawSmallArray arr (off - 2) (cnt + 2)
      else do
        out <- newSmallArray (cnt + 2) x
        copySmallArray out 2 arr off cnt
        pure out
  writeSmallArray m 0 x
  writeSmallArray m 1 y
  pure m

-- | Replace element @c@ by two.
insert2 :: SmallArray a -> Int -> a -> a -> SmallArray a
insert2 arr c x y = runSmallArray $ do
  let n = sizeofSmallArray arr
  m <- newSmallArray (n + 1) x
  copySmallArray m 0 arr 0 c
  writeSmallArray m (c + 1) y
  copySmallArray m (c + 2) arr (c + 1) (n - c - 1)
  pure m

-- | A slice of one array followed by a slice of another.
append2 :: SmallArray a -> Int -> Int -> SmallArray a -> Int -> Int -> SmallArray a
append2 a offa cnta b offb cntb = runSmallArray $ do
  m <- newSmallArray (cnta + cntb) (error "Data.Text.NanoRope: append2")
  copySmallArray m 0 a offa cnta
  copySmallArray m cnta b offb cntb
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

-- | Number of leaders of 4-byte sequences (@1111xxxx@) in a word. These are
-- exactly the code points taking two UTF-16 code units.
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

-- | Metrics of @len@ bytes of valid UTF-8 starting at @off@, 8 bytes at a
-- time.
sliceMetrics :: ByteArray -> Int -> Int -> Metrics
sliceMetrics arr off len = goWord off 0 0 0
  where
    end = off + len
    goWord !i !conts !fours !nls
      | i + 8 <= end =
          let w = indexWord64 arr i
           in if w .&. highs == 0
                then -- Eight bytes of ASCII, as most are.
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
roundDownFrom arr = go
  where
    go !j
      | isContByte (byteAt arr j) = go (j - 1)
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
offsetInChunk u k arr
  | k <= 0 = 0
  | otherwise = case u of
      Bytes -> roundDown arr k
      Chars -> scanUnits False k arr 0 (sizeofByteArray arr)
      Utf16 -> scanUnits True k arr 0 (sizeofByteArray arr)
      Lines -> scanLines k arr

-- | @scanUnits wide k arr from to@ walks over code points from the boundary
-- @from@ and stops in front of the first one that does not fit into @k@
-- units, or at @to@. Whole words are skipped as long as everything in them
-- fits.
scanUnits :: Bool -> Int -> ByteArray -> Int -> Int -> Int
scanUnits wide k arr from to = goWord from 0
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
{-# INLINE scanUnits #-}

-- | Is this all ASCII? Then bytes, code points and UTF-16 code units are the
-- same thing and nothing needs to be scanned to convert between them.
isAscii :: Metrics -> Bool
isAscii m = bytes m == chars m
{-# INLINE isAscii #-}

-- | 'offsetInChunk' for a leaf with known metrics.
leafOffset :: Unit -> Int -> Metrics -> ByteArray -> Int
leafOffset u k m arr
  | k <= 0 = 0
  | u == Lines = scanLines k arr
  | isAscii m = min k (bytes m)
  | otherwise = offsetInChunk u k arr
{-# INLINE leafOffset #-}

-- | Metrics of the first @b@ bytes of a leaf with known metrics, counting
-- whichever side of the cut is shorter.
leafPrefixMetrics :: Metrics -> ByteArray -> Int -> Metrics
leafPrefixMetrics m arr b
  | b <= 0 = mempty
  | b >= size = m
  | 2 * b > size = m `subMetrics` range b (size - b)
  | otherwise = range 0 b
  where
    size = bytes m
    range off len
      | isAscii m = Metrics len len len (if newlines m == 0 then 0 else countNewlines arr off len)
      | otherwise = sliceMetrics arr off len

countNewlines :: ByteArray -> Int -> Int -> Int
countNewlines arr off len = goWord off 0
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
findNewline arr = goWord
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
findNewlineBack arr = goWord
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
leafNewlinesBefore m arr b
  | newlines m == 0 || b <= 0 = 0
  | b >= size = newlines m
  | 2 * b > size = newlines m - countNewlines arr b (size - b)
  | otherwise = countNewlines arr 0 b
  where
    size = bytes m

scanLines :: Int -> ByteArray -> Int
scanLines k arr = goWord 0 0
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

------------------------------------------------------------------------------
-- Leaves

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

concat2 :: ByteArray -> ByteArray -> ByteArray
concat2 a b = concatSlices a 0 (sizeofByteArray a) b 0 (sizeofByteArray b)

-- | A slice of one chunk followed by a slice of another.
concatSlices :: ByteArray -> Int -> Int -> ByteArray -> Int -> Int -> ByteArray
concatSlices a offa la b offb lb = runByteArray $ do
  out <- newByteArray (la + lb)
  copyByteArray out 0 a offa la
  copyByteArray out la b offb lb
  pure out

-- | @spliceArray arr i j src off len@ replaces bytes @i .. j-1@ of @arr@ with
-- @len@ bytes of @src@.
spliceArray :: ByteArray -> Int -> Int -> ByteArray -> Int -> Int -> ByteArray
spliceArray arr i j src soff slen = runByteArray $ do
  out <- newByteArray (sizeofByteArray arr - (j - i) + slen)
  copyByteArray out 0 arr 0 i
  copyByteArray out i src soff slen
  copyByteArray out (i + slen) arr j (sizeofByteArray arr - j)
  pure out

-- | First @b@ bytes of a leaf.
leafPrefix :: Measure a => Int -> Node a -> Node a
leafPrefix b node = case node of
  Leaf m _ arr
    | b <= 0 -> emptyNode
    | b >= sizeofByteArray arr -> node
    | otherwise -> mkLeafWith (leafPrefixMetrics m arr b) (cloneByteArray arr 0 b)
  Inner{} -> error "Data.Text.NanoRope: leafPrefix on an inner node"
{-# INLINE leafPrefix #-}

-- | All but the first @b@ bytes of a leaf.
leafSuffix :: Measure a => Int -> Node a -> Node a
leafSuffix b node = case node of
  Leaf m _ arr
    | b <= 0 -> node
    | b >= sizeofByteArray arr -> emptyNode
    | otherwise ->
        mkLeafWith (m `subMetrics` leafPrefixMetrics m arr b) (cloneByteArray arr b (sizeofByteArray arr - b))
  Inner{} -> error "Data.Text.NanoRope: leafSuffix on an inner node"
{-# INLINE leafSuffix #-}

-- | Cut more than 'maxChunk' bytes into evenly sized leaves. Aiming a little
-- below 'maxChunk' leaves room for moving every cut back to a code point
-- boundary.
leavesFromSlice :: Measure a => ByteArray -> Int -> Int -> [Node a]
leavesFromSlice arr off len = go 1 off
  where
    target = maxChunk - 4
    k = (len + target - 1) `quot` target
    (q, r) = len `quotRem` k
    end = off + len
    go !j !start
      | j >= k = [leaf start end]
      | otherwise =
          let cut = roundDownFrom arr (off + j * q + (j * r) `quot` k)
           in leaf start cut : go (j + 1) cut
    leaf from to = mkLeaf (cloneByteArray arr from (to - from))
{-# INLINABLE leavesFromSlice #-}
{-# SPECIALIZE leavesFromSlice :: ByteArray -> Int -> Int -> [Node ()] #-}

-- | Build a tree over nodes of equal height, bottom up.
buildTree :: Monoid a => [Node a] -> Node a
buildTree [] = emptyNode
buildTree [n] = n
buildTree ns = buildTree (groupNodes (L.length ns) ns)
{-# INLINABLE buildTree #-}
{-# SPECIALIZE buildTree :: [Node ()] -> Node () #-}

-- | One level of parents, with the children spread evenly so that no parent
-- ends up below 'minChildren'.
groupNodes :: Monoid a => Int -> [Node a] -> [Node a]
groupNodes total = go 0
  where
    groups = (total + maxChildren - 1) `quot` maxChildren
    (q, r) = total `quotRem` groups
    go !j ns
      | j >= groups = []
      | otherwise =
          let size = if j < r then q + 1 else q
              (now, later) = L.splitAt size ns
           in mkInner (strictArray size now) : go (j + 1) later
{-# INLINABLE groupNodes #-}
{-# SPECIALIZE groupNodes :: Int -> [Node ()] -> [Node ()] #-}

strictArray :: Int -> [Node a] -> SmallArray (Node a)
strictArray n ns = runSmallArray $ do
  m <- newSmallArray n (error "Data.Text.NanoRope: strictArray")
  let go !_ [] = pure m
      go !i (x : xs) = x `seq` writeSmallArray m i x >> go (i + 1) xs
  go 0 ns

fromTextNode :: Measure a => Text -> Node a
fromTextNode (TI.Text (A.ByteArray ba) off len)
  | len <= 0 = emptyNode
  | len <= maxChunk =
      -- Share the array when the text owns all of it.
      mkLeaf (if off == 0 && len == sizeofByteArray arr then arr else cloneByteArray arr off len)
  | otherwise = buildTree (leavesFromSlice arr off len)
  where
    arr = ByteArray ba
{-# INLINABLE fromTextNode #-}
{-# SPECIALIZE fromTextNode :: Text -> Node () #-}

------------------------------------------------------------------------------
-- Concatenation

-- | Outcome of merging two trees: one or two nodes of the height of the
-- taller tree. Also the outcome of an edit within a leaf, which is 'None' if
-- the edit does not stay there.
data Result a
  = None
  | One !(Node a)
  | Two !(Node a) !(Node a)

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

unreachable :: String -> a
unreachable fun = error ("Data.Text.NanoRope: " ++ fun)
{-# NOINLINE unreachable #-}

-- | An unboxed pair is lazy in its components, which would make a thunk of
-- every node on the way back up. This one is not.
pair :: a -> b -> (# a, b #)
pair !a !b = (# a, b #)
{-# INLINE pair #-}

-- | Merge two non-empty trees whose roots are allowed to be undersized.
-- Walks down the spine of the taller tree to the height of the shorter one,
-- merges there, and propagates at most one extra node back up.
merge :: Measure a => Node a -> Node a -> Result a
merge l r = case compare (nodeHeight l) (nodeHeight r) of
  EQ -> mergeEq l r
  GT -> case l of
    Inner ml h _ cs ->
      let k = sizeofSmallArray cs - 1
          m = ml <> nodeMetrics r
       in case merge (indexSmallArray cs k) r of
            One x -> One (inner h m (replaceAt cs k x))
            Two x y -> fromChildren h m (snoc2 cs k x y)
            None -> None
    Leaf{} -> unreachable "merge"
  LT -> case r of
    Inner mr h _ cs ->
      let m = nodeMetrics l <> mr
       in case merge l (indexSmallArray cs 0) of
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
  | total <= maxChunk = One (Leaf (ml <> mr) (al <> ar) (concat2 bl br))
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
  | nl + nr <= maxChildren = One (inner h (ml <> mr) (csl <> csr))
  | half <= nl =
      -- Hand the last children of the left node over to the right one.
      let moved = sumMetrics csl half (nl - half)
       in Two
            (inner h (ml `subMetrics` moved) (cloneSmallArray csl 0 half))
            (inner h (moved <> mr) (append2 csl half (nl - half) csr 0 nr))
  | otherwise =
      let cnt = half - nl
          moved = sumMetrics csr 0 cnt
       in Two
            (inner h (ml <> moved) (append2 csl 0 nl csr 0 cnt))
            (inner h (mr `subMetrics` moved) (cloneSmallArray csr cnt (nr - cnt)))
  where
    nl = sizeofSmallArray csl
    nr = sizeofSmallArray csr
    half = (nl + nr + 1) `quot` 2
mergeEq _ _ = unreachable "mergeEq"
{-# INLINABLE mergeEq #-}
{-# SPECIALIZE mergeEq :: Node () -> Node () -> Result () #-}

-- | One node of height @h@ if the children fit, else two.
fromChildren :: Monoid a => Int -> Metrics -> SmallArray (Node a) -> Result a
fromChildren h m cs
  | n <= maxChildren = One (inner h m cs)
  | otherwise =
      let half = (n + 1) `quot` 2
          ml = sumMetrics cs 0 half
       in Two
            (inner h ml (cloneSmallArray cs 0 half))
            (inner h (m `subMetrics` ml) (cloneSmallArray cs half (n - half)))
  where
    n = sizeofSmallArray cs
{-# INLINABLE fromChildren #-}
{-# SPECIALIZE fromChildren :: Int -> Metrics -> SmallArray (Node ()) -> Result () #-}

------------------------------------------------------------------------------
-- Breaking

-- | Is offset @k@ at or beyond the end? The start of the last line is the
-- one offset equal to the total which is not necessarily the end.
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
  | k <= 0 = pair emptyNode root
  | beyondEnd u k (nodeMetrics root) = pair root emptyNode
  | otherwise = splitNode u k root
{-# INLINABLE splitRoot #-}
{-# SPECIALIZE splitRoot :: Unit -> Int -> Node () -> (# Node (), Node () #) #-}

-- | Children @0 .. i-1@ of a node of height @h@, whose metrics are @before@,
-- followed by a lower tree, which may be empty or undersized: it is settled
-- with its sibling, and the rest is lined up once.
joinLeft :: Measure a => Int -> SmallArray (Node a) -> Int -> Metrics -> Node a -> Node a
joinLeft h cs i before l
  | i == 0 = l
  | nodeIsEmpty l = if i == 1 then indexSmallArray cs 0 else inner h before (cloneSmallArray cs 0 i)
  | otherwise = case merge (indexSmallArray cs (i - 1)) l of
      One x
        | i == 1 -> x
        | otherwise -> inner h m $ runSmallArray $ do
            out <- thawSmallArray cs 0 i
            writeSmallArray out (i - 1) x
            pure out
      Two x y -> inner h m (snoc2 cs (i - 1) x y)
      None -> unreachable "joinLeft"
  where
    m = before <> nodeMetrics l
{-# INLINABLE joinLeft #-}
{-# SPECIALIZE joinLeft :: Int -> SmallArray (Node ()) -> Int -> Metrics -> Node () -> Node () #-}

-- | A lower tree followed by the children after child @i@ of a node of
-- height @h@, whose metrics are @after@.
joinRight :: Measure a => Int -> SmallArray (Node a) -> Int -> Metrics -> Node a -> Node a
joinRight h cs i after r
  | rest == 0 = r
  | nodeIsEmpty r = if rest == 1 then indexSmallArray cs (i + 1) else inner h after (cloneSmallArray cs (i + 1) rest)
  | otherwise = case merge r (indexSmallArray cs (i + 1)) of
      One x
        | rest == 1 -> x
        | otherwise -> inner h m $ runSmallArray $ do
            out <- thawSmallArray cs (i + 1) rest
            writeSmallArray out 0 x
            pure out
      Two x y -> inner h m (cons2 x y cs (i + 2))
      None -> unreachable "joinRight"
  where
    rest = sizeofSmallArray cs - i - 1
    m = nodeMetrics r <> after
{-# INLINABLE joinRight #-}
{-# SPECIALIZE joinRight :: Int -> SmallArray (Node ()) -> Int -> Metrics -> Node () -> Node () #-}

takeNode :: Measure a => Unit -> Int -> Node a -> Node a
takeNode u k node = case node of
  Leaf m _ arr -> leafPrefix (leafOffset u k m arr) node
  Inner _ h _ cs -> case seekChild u k cs of
    (# i, before #) -> joinLeft h cs i before (takeNode u (k - count u before) (indexSmallArray cs i))
{-# INLINABLE takeNode #-}
{-# SPECIALIZE takeNode :: Unit -> Int -> Node () -> Node () #-}

dropNode :: Measure a => Unit -> Int -> Node a -> Node a
dropNode u k node = case node of
  Leaf m _ arr -> leafSuffix (leafOffset u k m arr) node
  Inner total h _ cs -> case seekChild u k cs of
    (# i, before #) ->
      let child = indexSmallArray cs i
          after = total `subMetrics` before `subMetrics` nodeMetrics child
       in joinRight h cs i after (dropNode u (k - count u before) child)
{-# INLINABLE dropNode #-}
{-# SPECIALIZE dropNode :: Unit -> Int -> Node () -> Node () #-}

-- | 'takeNode' and 'dropNode' in one descent.
splitNode :: Measure a => Unit -> Int -> Node a -> (# Node a, Node a #)
splitNode u k node = case node of
  Leaf m _ arr
    | b <= 0 -> pair emptyNode node
    | b >= size -> pair node emptyNode
    | otherwise ->
        let pm = leafPrefixMetrics m arr b
         in pair (mkLeafWith pm (cloneByteArray arr 0 b)) (mkLeafWith (m `subMetrics` pm) (cloneByteArray arr b (size - b)))
    where
      size = sizeofByteArray arr
      b = leafOffset u k m arr
  Inner total h _ cs -> case seekChild u k cs of
    (# i, before #) ->
      let child = indexSmallArray cs i
          after = total `subMetrics` before `subMetrics` nodeMetrics child
       in case splitNode u (k - count u before) child of
            (# l, r #) -> pair (joinLeft h cs i before l) (joinRight h cs i after r)
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
      Leaf m _ arr -> acc <> leafPrefixMetrics m arr (leafOffset u j m arr)
      Inner _ _ _ cs -> case seekChild u j cs of
        (# i, before #) -> go (acc <> before) (j - count u before) (indexSmallArray cs i)

-- | Just the byte offset of 'metricsAtNode', which is all that editing needs
-- and spares measuring the prefix of a leaf.
byteOffsetAtNode :: Unit -> Int -> Node a -> Int
byteOffsetAtNode u k root
  | k <= 0 = 0
  | beyondEnd u k (nodeMetrics root) = nodeBytes root
  | otherwise = go 0 k root
  where
    go !acc !j node = case node of
      Leaf m _ arr -> acc + leafOffset u j m arr
      Inner _ _ _ cs -> case seekUnitBytes u j cs of
        (# i, j', b #) -> go (acc + b) j' (indexSmallArray cs i)

-- | The byte at offset @i@, for @0 <= i < size@.
indexByteNode :: Int -> Node a -> Word8
indexByteNode !i node = case node of
  Leaf _ _ arr -> byteAt arr i
  Inner _ _ _ cs -> case seekByte i cs of
    (# c, i' #) -> indexByteNode i' (indexSmallArray cs c)

-- | Bytes @i .. j-1@ as a 'Text', given boundaries @0 <= i <= j <= size@.
-- A range inside a single chunk is returned as a view of that chunk.
sliceToText :: Int -> Int -> Node a -> Text
sliceToText !i !j node
  | i >= j = T.empty
  | otherwise = case node of
      Leaf _ _ arr -> viewSlice arr i (j - i)
      Inner _ _ _ cs -> case seekByte i cs of
        (# c, i' #)
          | j' <= nodeBytes child -> sliceToText i' j' child
          | otherwise ->
              let !(ByteArray ba) = runByteArray $ do
                    out <- newByteArray (j - i)
                    copyRange out 0 i j node
                    pure out
               in TI.Text (A.ByteArray ba) 0 (j - i)
          where
            child = indexSmallArray cs c
            j' = j - (i - i')

-- | Copy bytes @i .. j-1@ of a node to offset @d@ of a buffer.
copyRange :: MutableByteArray s -> Int -> Int -> Int -> Node a -> ST s ()
copyRange out !d !i !j node = case node of
  Leaf _ _ arr -> copyByteArray out d arr i (j - i)
  Inner _ _ _ cs -> case seekByte i cs of
    (# c0, i0 #) -> go c0 (i - i0)
    where
      n = sizeofSmallArray cs
      go !c !start = when (c < n && start < j) $ do
        let child = indexSmallArray cs c
            end = start + nodeBytes child
            lo = max i start
            hi = min j end
        when (lo < hi) $
          copyRange out (d + lo - i) (lo - start) (hi - start) child
        go (c + 1) end

-- | Locations of the start of line @l@ and of the end of its content, that
-- is before the terminating @\\n@ or @\\r\\n@, or at the end of the rope.
lineSpan :: Int -> Node a -> (Metrics, Metrics)
lineSpan l root = (start, lineEnd start (metricsAtNode Lines (max 0 l + 1) root) root)
  where
    start = metricsAtNode Lines l root

-- | End of the content of the line starting at @start@, given the start of
-- the next line.
lineEnd :: Metrics -> Metrics -> Node a -> Metrics
lineEnd start next root
  | newlines next == newlines start = next
  | bytes lf > bytes start && indexByteNode (bytes lf - 1) root == 0x0D = lf `subMetrics` Metrics 1 1 1 0
  | otherwise = lf
  where
    lf = next `subMetrics` Metrics 1 1 1 1

-- | A line that starts and is terminated within one leaf, as most lines are:
-- the metrics in front of that leaf, the leaf, and the offsets within it of
-- the start of the line and of the end of its content.
data LocalLine = LocalLine !Metrics !Metrics !ByteArray !Int !Int

-- | Find line @l >= 0@ in a single descent, if it is local to a leaf.
localLine :: Int -> Node a -> Maybe LocalLine
localLine l root
  | l > newlines (nodeMetrics root) = Nothing
  | otherwise = go mempty l root
  where
    go !acc !j node = case node of
      Leaf m _ arr ->
        let from = if j <= 0 then 0 else scanLines j arr
            lf = findNewline arr from
         in if lf >= sizeofByteArray arr
              then Nothing
              else Just (LocalLine acc m arr from (if lf > from && byteAt arr (lf - 1) == 0x0D then lf - 1 else lf))
      Inner _ _ _ cs
        | j <= 0 -> go acc j (indexSmallArray cs 0)
        | otherwise -> case seekChild Lines j cs of
            (# i, before #) -> go (acc <> before) (j - newlines before) (indexSmallArray cs i)

-- | 'localLine' for when the location of the leaf does not matter.
localLineText :: Int -> Node a -> Maybe Text
localLineText l root
  | l > newlines (nodeMetrics root) = Nothing
  | otherwise = go l root
  where
    go !j node = case node of
      Leaf _ _ arr ->
        let from = if j <= 0 then 0 else scanLines j arr
            lf = findNewline arr from
         in if lf >= sizeofByteArray arr
              then Nothing
              else Just (viewSlice arr from ((if lf > from && byteAt arr (lf - 1) == 0x0D then lf - 1 else lf) - from))
      Inner _ _ _ cs
        | j <= 0 -> go j (indexSmallArray cs 0)
        | otherwise -> case seekUnit Lines j cs of
            (# i, j' #) -> go j' (indexSmallArray cs i)

-- | The position of an offset in a single descent, if the line it is on
-- starts within the same leaf (as most lines do) or with the rope.
localPosition :: Unit -> Unit -> Int -> Node a -> Maybe Position
localPosition from to k root
  | k <= 0 = Just (Position 0 0)
  | beyondEnd from k (nodeMetrics root) = Nothing
  | otherwise = go 0 0 k root
  where
    go !ls !bs !j node = case node of
      Leaf m _ arr ->
        let b = leafOffset from j m arr
            lf = if newlines m == 0 then -1 else findNewlineBack arr b
            start = lf + 1
            column
              | to == Lines = 0
              | to == Bytes || isAscii m = b - start
              | otherwise = count to (sliceMetrics arr start (b - start))
         in if lf < 0 && bs > 0
              then Nothing
              else Just (Position (ls + leafNewlinesBefore m arr b) column)
      Inner _ _ _ cs -> case seekChild from j cs of
        (# i, before #) -> go (ls + newlines before) (bs + bytes before) (j - count from before) (indexSmallArray cs i)

metricsAtPositionNode :: Unit -> Position -> Node a -> Metrics
metricsAtPositionNode u (Position l c) root = case localLine (max 0 l) root of
  Just (LocalLine acc m arr from to) ->
    let b
          | c <= 0 = from
          | u == Lines || (u == Bytes || isAscii m) && c >= to - from = to
          | isAscii m = from + c
          | otherwise = case u of
              Bytes -> roundDownFrom arr (from + c)
              Utf16 -> scanUnits True c arr from to
              _ -> scanUnits False c arr from to
     in acc <> leafPrefixMetrics m arr b
  Nothing
    | c <= 0 -> start
    | u == Lines || bytes there > bytes end -> end
    | otherwise -> there
  where
    -- The general case: a line across leaves, or no such line.
    (start, end) = lineSpan l root
    there = metricsAtNode u (count u start + min c (count u (nodeMetrics root))) root

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
        let n = sizeofSmallArray cs
            loop !i !m' !a'
              | i >= n - 1 || p m'' a'' = go m' a' c
              | otherwise = loop (i + 1) m'' a''
              where
                c = indexSmallArray cs i
                m'' = m' <> nodeMetrics c
                a'' = a' <> nodeAnn c
         in loop 0 m a
{-# INLINABLE metricsWhereNode #-}
{-# SPECIALIZE metricsWhereNode :: (Metrics -> () -> Bool) -> Node () -> Metrics #-}

------------------------------------------------------------------------------
-- Editing

-- | @editNode least u k d src off len@ replaces the text from offset @k@ up
-- to offset @k + d@ by a slice of a byte array /if/ that stays within one
-- leaf, in a single descent. This is what typing and erasing come to: the
-- leaf is copied once and the path to it is patched. A leaf that outgrows
-- 'maxChunk' splits in two and hands its parent another child, like in any
-- other B-tree. 'None' is a range across leaves, a leaf that would shrink
-- below 'minChunk' or more text than fits into two leaves.
--
-- Comes with the room that is left in the leaf the new text ended up in.
--
-- The first argument is the least size the leaf may shrink to: 'minChunk', or
-- nothing at all for a leaf that is the root. (It is not a 'Bool': with one,
-- GHC's constructor specialisation gets ahead of specialising the measure.)
editNode :: Measure a => Int -> Unit -> Int -> Int -> ByteArray -> Int -> Int -> Node a -> (# Result a, Int #)
editNode !least u !k !d !src !soff !slen node = case node of
  Leaf m _ arr
    | d > 0 && kj > count u m -> (# None, 0 #)
    | slen <= 0 && bj <= bi -> (# None, 0 #)
    | size' < least || size' > 2 * maxChunk - 8 -> (# None, 0 #)
    | size' <= maxChunk -> pair (One (mkLeafWith m' (spliceArray arr bi bj src soff slen))) (maxChunk - size')
    | otherwise ->
        -- Room for moving the cut back to a code point boundary is what the
        -- 8 bytes above are for.
        let both = spliceArray arr bi bj src soff slen
            cut = roundDownFrom both (size' `quot` 2)
            mx = sliceMetrics both 0 cut
            halves =
              Two
                (mkLeafWith mx (cloneByteArray both 0 cut))
                (mkLeafWith (m' `subMetrics` mx) (cloneByteArray both cut (size' - cut)))
         in pair halves (maxChunk - max cut (size' - cut))
    where
      !kj = max 0 k + d
      bi = leafOffset u k m arr
      bj = if d > 0 then leafOffset u kj m arr else bi
      size' = sizeofByteArray arr - (bj - bi) + slen
      kept = if bj > bi then m `subMetrics` sliceMetrics arr bi (bj - bi) else m
      m' = kept <> sliceMetrics src soff slen
  Inner m h _ cs -> case seekUnit u k cs of
    (# c, k' #) ->
      let old = indexSmallArray cs c
       in case editNode least u k' d src soff slen old of
            (# None, _ #) -> (# None, 0 #)
            (# One new, room #) ->
              pair (One (inner h (m <> (nodeMetrics new `subMetrics` nodeMetrics old)) (replaceAt cs c new))) room
            (# Two x y, room #) ->
              pair (fromChildren h (m <> ((nodeMetrics x <> nodeMetrics y) `subMetrics` nodeMetrics old)) (insert2 cs c x y)) room
{-# INLINABLE editNode #-}
{-# SPECIALIZE editNode :: Int -> Unit -> Int -> Int -> ByteArray -> Int -> Int -> Node () -> (# Result (), Int #) #-}

-- | Replace the text from offset @i@ up to offset @j@. Comes with the room
-- of 'editNode', or none if it does not know.
editRoot :: Measure a => Unit -> Int -> Int -> Text -> Node a -> (# Node a, Int #)
editRoot u i j t@(TI.Text (A.ByteArray ba) off len) root =
  case editNode (if nodeHeight root == 0 then 0 else minChunk) u from (max 0 (j - from)) (ByteArray ba) off len root of
    (# One node, room #) -> (# node, room #)
    (# Two x y, room #) -> pair (mkInner2 x y) room
    (# None, _ #)
      | bi < bj -> pair (takeRoot Bytes bi root `appendNode` fromTextNode t `appendNode` dropRoot Bytes bj root) 0
      | len <= 0 -> (# root, 0 #)
      | otherwise -> case splitRoot Bytes bi root of
          (# l, r #) -> pair (l `appendNode` fromTextNode t `appendNode` r) 0
  where
    from = max 0 i
    -- Both offsets as bytes. Offsets of the original rope rather than of some
    -- intermediate result, so that they round the same way as everywhere
    -- else.
    bi = byteOffsetAtNode u i root
    bj = if j <= i then bi else byteOffsetAtNode u j root
{-# INLINABLE editRoot #-}
{-# SPECIALIZE editRoot :: Unit -> Int -> Int -> Text -> Node () -> (# Node (), Int #) #-}

edited :: Measure a => Unit -> Int -> Int -> Text -> Node a -> Node a
edited u i j t root = case editRoot u i j t root of
  (# root', _ #) -> root'
{-# INLINE edited #-}

------------------------------------------------------------------------------
-- Typing

-- The run of keystrokes of a 'Typing' rope stands for one insertion, and it
-- has to come to the same as the insertions it is made of. For offsets in
-- bytes, code points or UTF-16 code units it does:
--
-- > insert u (max 0 i + n) t2 (insert u i t1 r) == insert u i (t1 <> t2) r
-- >   where n = count u (metrics t1)
--
-- even if @i@ is clamped or rounded. Beyond the end, both insertions go to
-- the end. Inside a code point, @t1@ goes in front of that code point, and
-- @i + n@ is as far inside the same code point as @i@ was. Lines are out:
-- text does not end where the line after it starts.
--
-- Whoever reads the rope after every keystroke has the run inserted every
-- time, into the same tree. That is as good as inserting the keystrokes one
-- by one as long as the run fits into the leaf it goes to, which would
-- otherwise split over and over. So a run is given no more room than that
-- leaf has. The insertion before the run went to the same leaf and reports
-- it; should it be wrong, inserting the run is slower, not wrong.

-- | A rope with a run of keystrokes to be inserted at an offset.
typing :: Measure a => Node a -> Unit -> Int -> Int -> ByteArray -> Metrics -> Int -> Rope a
typing base u start next run total room =
  Typing (edited u start start (chunkText run) base) base u start next run total room
{-# INLINE typing #-}

-- | Insert text at an offset. The first insertion at some place goes into the
-- tree and leaves a note of where it ended. One that starts there is taken
-- for typing and begins a run, which the ones after it add to.
insertText :: Measure a => Unit -> Int -> Text -> Rope a -> Rope a
insertText u i t@(TI.Text (A.ByteArray ba) off len) r = case r of
  Typing root base ru start next run total room
    | next == i && ru == u && len <= room ->
        let tm = sliceMetrics src off len
         in typing base u start (next + count u tm) (concatSlices run 0 (sizeofByteArray run) src off len) (total <> tm) (room - len)
    | otherwise -> settled root ru next room
  Settled root hu hint room -> settled root hu hint room
  where
    src = ByteArray ba
    settled root hu hint room
      | hint == i && i >= 0 && hu == u && len <= min room maxPending =
          let tm = sliceMetrics src off len
           in typing root u i (i + count u tm) (cloneByteArray src off len) (nodeMetrics root <> tm) (min room maxPending - len)
      | otherwise = case editRoot u i i t root of
          (# root', room' #) ->
            let grown = count u (nodeMetrics root') - count u (nodeMetrics root)
             in Settled root' u (if u == Lines then -1 else max 0 i + grown) room'
{-# INLINABLE insertText #-}
{-# SPECIALIZE insertText :: Unit -> Int -> Text -> Rope () -> Rope () #-}

-- | Delete the text from offset @i@ up to offset @j > i@. Erasing the end of
-- what has just been typed shortens the run, given that the run is where its
-- offsets say, which is known of code points that are not beyond the end.
deleteRange :: Measure a => Unit -> Int -> Int -> Rope a -> Rope a
deleteRange u i j r = case r of
  Typing _ base Chars start next run total room
    | u == Chars && j == next && i >= start && start <= chars (nodeMetrics base) ->
        let size = sizeofByteArray run
            keep = dropCharsEnd (j - i) run
            total' = total `subMetrics` sliceMetrics run keep (size - keep)
         in if keep <= 0
              then Settled base Chars start (room + size)
              else typing base Chars start i (cloneByteArray run 0 keep) total' (room + size - keep)
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
      go (Inner _ _ a cs) = rnf a `seq` F.foldl' (\() c -> go c) () cs

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

-- | /O(n)/. The text is copied into chunks, except that a text of at most
-- 'maxChunk' bytes which owns its whole buffer is shared.
fromText :: Measure a => Text -> Rope a
fromText = Rope . fromTextNode
{-# INLINE fromText #-}

-- | /O(n)/.
fromLazyText :: Measure a => TL.Text -> Rope a
fromLazyText = TL.foldlChunks (\acc t -> acc <> fromText t) empty
{-# INLINABLE fromLazyText #-}

------------------------------------------------------------------------------
-- Deconstruction

-- | /O(n)/. A rope of a single chunk is converted without copying.
toText :: Rope a -> Text
toText (Rope root) = sliceToText 0 (nodeBytes root) root

-- | /O(n)/, without copying any text: the lazy text shares the chunks.
toLazyText :: Rope a -> TL.Text
toLazyText = TL.fromChunks . toChunks

-- | /O(n)/.
toString :: Rope a -> String
toString = TL.unpack . toLazyText

-- | The chunks of the rope as zero-copy views, in order. They are non-empty,
-- at most 'maxChunk' bytes long and produced lazily.
toChunks :: Rope a -> [Text]
toChunks = foldrChunks (:) []

-- | Lazy right fold over the chunks of 'toChunks'.
foldrChunks :: (Text -> b -> b) -> b -> Rope a -> b
foldrChunks f z (Rope root) = foldrNode (f . chunkText) z root
{-# INLINE foldrChunks #-}

foldrNode :: (ByteArray -> b -> b) -> b -> Node a -> b
foldrNode f = go
  where
    go z (Leaf _ _ arr)
      | sizeofByteArray arr == 0 = z
      | otherwise = f arr z
    go z (Inner _ _ _ cs) = F.foldr (flip go) z cs
{-# INLINE foldrNode #-}

-- | /O(log n)/. Zero-copy view of the rest of the chunk containing the given
-- offset; empty exactly when the offset is at or beyond the end.
--
-- This is the shape of a parser's read callback (such as tree-sitter's
-- @TSInput@): ask for the text at a byte offset, consume it, ask again at
-- the following offset.
chunkAt :: Unit -> Int -> Rope a -> Text
chunkAt u k (Rope root)
  | b >= nodeBytes root = T.empty
  | otherwise = go b root
  where
    b = byteOffsetAtNode u k root
    go !i node = case node of
      Leaf _ _ arr -> viewSlice arr i (sizeofByteArray arr - i)
      Inner _ _ _ cs -> case seekByte i cs of
        (# c, i' #) -> go i' (indexSmallArray cs c)

------------------------------------------------------------------------------
-- Queries

-- | /O(1)/.
null :: Rope a -> Bool
null r = bytes (metrics r) == 0
{-# INLINE null #-}

-- | /O(1)/. Length in any unit; for 'Lines' this is the number of @\\n@.
length :: Unit -> Rope a -> Int
length u = count u . metrics
{-# INLINE length #-}

-- | /O(1)/. Number of lines: one more than the number of @\\n@, so that the
-- valid line indices are @[0 .. lineCount - 1]@. The last line may be empty.
lineCount :: Rope a -> Int
lineCount r = newlines (metrics r) + 1
{-# INLINE lineCount #-}

-- | /O(1)/. All built-in measurements of the rope.
metrics :: Rope a -> Metrics
metrics (Settled root _ _ _) = nodeMetrics root
metrics (Typing _ _ _ _ _ _ total _) = total
{-# INLINE metrics #-}

-- | /O(1)/. The custom measure of the rope.
measure :: Rope a -> a
measure (Rope root) = nodeAnn root
{-# INLINE measure #-}

-- | Number of levels of inner nodes above the leaves.
height :: Rope a -> Int
height (Rope root) = nodeHeight root

------------------------------------------------------------------------------
-- Combining and breaking

-- | /O(log n)/, more precisely proportional to the difference in height.
-- Same as '<>'.
append :: Measure a => Rope a -> Rope a -> Rope a
append (Rope l) (Rope r) = Rope (appendNode l r)
{-# INLINABLE append #-}

-- | /O(log n)/. Split at an offset, clamped to the rope and rounded down to
-- a code point boundary (see 'Unit'). Both halves come out of one descent;
-- 'take' and 'drop' are cheaper if you are after just one of them.
--
-- >>> splitAt Lines 1 "fst\nsnd\n"
-- ("fst\n","snd\n")
splitAt :: Measure a => Unit -> Int -> Rope a -> (Rope a, Rope a)
splitAt u k (Rope root) = case splitRoot u k root of
  (# l, r #) -> (Rope l, Rope r)
{-# INLINABLE splitAt #-}

-- | /O(log n)/. The prefix up to an offset.
take :: Measure a => Unit -> Int -> Rope a -> Rope a
take u k (Rope root) = Rope (takeRoot u k root)
{-# INLINABLE take #-}

-- | /O(log n)/. The suffix from an offset.
drop :: Measure a => Unit -> Int -> Rope a -> Rope a
drop u k (Rope root) = Rope (dropRoot u k root)
{-# INLINABLE drop #-}

-- | Both offsets as bytes. Offsets of the original rope rather than of some
-- intermediate result, so that they round the same way as everywhere else.
byteRange :: Unit -> Int -> Int -> Node a -> (Int, Int)
byteRange u i j root = (bi, if j <= i then bi else byteOffsetAtNode u j root)
  where
    bi = byteOffsetAtNode u i root
{-# INLINE byteRange #-}

-- | /O(log n)/. @slice u i j@ is the text from offset @i@ up to offset @j@.
slice :: Measure a => Unit -> Int -> Int -> Rope a -> Rope a
slice u i j (Rope root) = Rope (dropRoot Bytes bi (takeRoot Bytes bj root))
  where
    (bi, bj) = byteRange u i j root
{-# INLINABLE slice #-}

-- | /O(log n + length of the result)/. Like 'slice', but straight to 'Text'
-- without building a rope in between. A range within a single chunk is
-- returned as a zero-copy view of that chunk.
sliceText :: Unit -> Int -> Int -> Rope a -> Text
sliceText u i j (Rope root) = sliceToText bi bj root
  where
    (bi, bj) = byteRange u i j root

------------------------------------------------------------------------------
-- Editing

-- | /O(log n + length of the text)/. Insert text at an offset.
--
-- An insertion confined to one chunk, as nearly all are, copies that chunk
-- and the path to it and nothing else; a chunk that overflows splits in two.
--
-- Typing is cheaper still. An insertion that starts where the one before it
-- ended (in the same unit, which is not 'Lines') is held back: up to
-- 'maxPending' bytes of such keystrokes wait next to the tree and go into it
-- at once, when the rope is read or edited elsewhere. A keystroke then costs
-- a copy of what is waiting, /O(1)/, and 'length' and 'metrics' answer without
-- looking at the tree. This is the one lazy spot of a rope: evaluating it to
-- weak head normal form leaves up to one such insertion undone.
insert :: Measure a => Unit -> Int -> Text -> Rope a -> Rope a
insert u i t r
  | T.null t = r
  | otherwise = insertText u i t r
{-# INLINABLE insert #-}

-- | /O(log n)/. @delete u i j@ removes the text from offset @i@ up to
-- offset @j@. Erasing the end of what was just typed (see 'insert') by code
-- points is /O(1)/ as well.
delete :: Measure a => Unit -> Int -> Int -> Rope a -> Rope a
delete u i j r
  | j <= i = r
  | otherwise = deleteRange u i j r
{-# INLINABLE delete #-}

-- | /O(log n + length of the text)/. @replace u i j t@ replaces the text
-- from offset @i@ up to offset @j@ by @t@.
--
-- An edit confined to one chunk that neither overflows nor underflows, as
-- nearly all keystrokes are, copies that chunk and the path to it and
-- nothing else.
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
  | otherwise = case localLineText l root of
      Just line -> line
      Nothing ->
        let (start, end) = lineSpan l root
         in sliceToText (bytes start) (bytes end) root

-- | /O(n)/. The lines of the rope without their terminators, lazily. Like
-- 'Data.Text.lines', a trailing @\\n@ does not start another line; unlike
-- it, @\\r\\n@ is stripped too. Lines within a single chunk are zero-copy
-- views.
lines :: Rope a -> [Text]
lines = go [] . toChunks
  where
    -- The pieces of an unfinished line, last one first. Never just empty
    -- pieces: a piece is only carried over if it is a whole chunk.
    go carry [] = [T.concat (reverse carry) | not (L.null carry)]
    go carry (chunk : chunks) = case T.breakOn (T.singleton '\n') chunk of
      (piece, rest)
        | T.null rest -> go (piece : carry) chunks
        | otherwise ->
            let after = T.drop 1 rest
             in stripCR (T.concat (reverse (piece : carry)))
                  : go [] (if T.null after then chunks else after : chunks)
    stripCR t
      | not (T.null t) && T.last t == '\r' = T.init t
      | otherwise = t

------------------------------------------------------------------------------
-- Conversions

-- | /O(log n)/. The 'Metrics' of the prefix ending at an offset, in other
-- words the same location expressed in every unit at once. The offset is
-- clamped and rounded as described at 'Unit'.
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

-- | /O(log n)/. Split at a line and column, the column counted in the given
-- unit. A column beyond the end of the line is clamped to the end of its
-- content (before the @\\n@ or @\\r\\n@) and a line beyond the last one to the
-- end of the rope, as the Language Server Protocol asks for. (Columns counted
-- in 'Lines' are accepted for uniformity: any positive one is the end of the
-- line.)
splitAtPosition :: Measure a => Unit -> Position -> Rope a -> (Rope a, Rope a)
splitAtPosition u pos r = splitAt Bytes (bytes (metricsAtPosition u pos r)) r
{-# INLINE splitAtPosition #-}

-- | /O(log n)/. The location of a position in every unit, clamped like
-- 'splitAtPosition'.
metricsAtPosition :: Unit -> Position -> Rope a -> Metrics
metricsAtPosition u pos (Rope root) = metricsAtPositionNode u pos root

-- | /O(log n)/. The position, with its column in the given unit, of a
-- location obtained from 'metricsAt', 'metricsAtPosition' or 'metricsWhere'.
metricsToPosition :: Unit -> Metrics -> Rope a -> Position
metricsToPosition u m (Rope root) =
  Position (newlines m) (count u m - count u (metricsAtNode Lines (newlines m) root))

-- | /O(log n)/. @offsetToPosition from to@ turns an offset in unit @from@
-- into a position with its column in unit @to@.
--
-- >>> offsetToPosition Bytes Utf16 11 "a😀\nb😀c"
-- Position {posLine = 1, posColumn = 3}
offsetToPosition :: Unit -> Unit -> Int -> Rope a -> Position
offsetToPosition from to k r@(Rope root) = case localPosition from to k root of
  Just pos -> pos
  Nothing -> metricsToPosition to (metricsAt from k r) r

-- | /O(log n)/. @positionToOffset from to@ turns a position with its column
-- in unit @from@ into an offset in unit @to@.
--
-- >>> positionToOffset Utf16 Bytes (Position 1 3) "a😀\nb😀c"
-- 11
positionToOffset :: Unit -> Unit -> Position -> Rope a -> Int
positionToOffset from to pos = count to . metricsAtPosition from pos
{-# INLINE positionToOffset #-}

------------------------------------------------------------------------------
-- Custom measures

-- | /O(log n)/. Split where a predicate on the measurements of the prefix
-- turns true: the first half is the longest prefix (of whole code points)
-- for which the predicate is false. The predicate has to be monotone, that
-- is stay true once it is true.
--
-- For example, with a measure of display width, the part of a line that fits
-- into 80 columns is @fst . splitWhere (\\_ w -> w > 80)@.
splitWhere :: Measure a => (Metrics -> a -> Bool) -> Rope a -> (Rope a, Rope a)
splitWhere p r = splitAt Bytes (bytes (metricsWhere p r)) r
{-# INLINE splitWhere #-}

-- | /O(log n)/. The location where 'splitWhere' splits.
metricsWhere :: Measure a => (Metrics -> a -> Bool) -> Rope a -> Metrics
metricsWhere p (Rope root) = metricsWhereNode p root
{-# INLINABLE metricsWhere #-}

-- | /O(n)/. Annotate the same text with another measure. The text itself is
-- shared, not copied.
remeasure :: Measure b => Rope a -> Rope b
remeasure (Rope root) = Rope (go root)
  where
    go (Leaf m _ arr) = mkLeafWith m arr
    go (Inner m h _ cs) = inner h m (mapSmallArray' go cs)
{-# INLINABLE remeasure #-}

------------------------------------------------------------------------------
-- Debugging

-- | Violated invariants of the tree; empty for every rope you can build
-- through the public interface with a lawful 'Measure'.
invariants :: (Measure a, Eq a) => Rope a -> [String]
invariants rope = case rope of
  Settled root _ _ _ -> go True root
  Typing root base u start next run total room ->
    go True root
      ++ map ("without what was typed: " ++) (go True base)
      ++ [ "a run of " ++ show (sizeofByteArray run) ++ " bytes with room for " ++ show room | sizeofByteArray run <= 0 || room < 0 || sizeofByteArray run + room > maxPending ]
      ++ [ "a run by lines" | u == Lines ]
      ++ [ "a run from " ++ show start ++ " to " ++ show next ++ " of " ++ show (count u typed) | start < 0 || next - start /= count u typed ]
      ++ [ "the rope caches " ++ show total ++ " instead of " ++ show (nodeMetrics root) | total /= nodeMetrics root ]
    where
      typed = naive run
  where
    go isRoot node = case node of
      Leaf m a arr ->
        let size = sizeofByteArray arr
         in [ "leaf of " ++ show size ++ " bytes is too large" | size > maxChunk ]
              ++ [ "leaf of " ++ show size ++ " bytes is too small" | not isRoot, size < minChunk ]
              ++ [ "leaf starts inside a code point" | size > 0, isContByte (byteAt arr 0) ]
              ++ [ "leaf caches " ++ show m ++ " instead of " ++ show (naive arr) | m /= naive arr ]
              ++ [ "leaf caches a wrong annotation" | a /= measureChunk (chunkText arr) ]
      Inner m h a cs ->
        let n = sizeofSmallArray cs
            kids = F.toList cs
            total = mconcat (map nodeMetrics kids)
         in [ "inner node with " ++ show n ++ " children is too large" | n > maxChildren ]
              ++ [ "inner node with " ++ show n ++ " children is too small" | n < (if isRoot then 2 else minChildren) ]
              ++ [ "child of height " ++ show (nodeHeight c) ++ " below a node of height " ++ show h | c <- kids, nodeHeight c /= h - 1 ]
              ++ [ "inner node caches " ++ show m ++ " instead of " ++ show total | m /= total ]
              ++ [ "inner node caches a wrong annotation" | a /= mconcat (map nodeAnn kids) ]
              ++ concatMap (go False) kids
    naive arr =
      let bs = [ byteAt arr i | i <- [0 .. sizeofByteArray arr - 1] ]
          cs = L.length (filter (not . isContByte) bs)
       in Metrics (L.length bs) cs (cs + L.length (filter (>= 0xF0) bs)) (L.length (filter (== 0x0A) bs))
