{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
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
-- boundaries). Inner nodes hold up to 'maxChildren' children together with a
-- flat unboxed array of /prefix sums/ of the children's 'Metrics', laid out
-- one metric after another. Seeking by any unit is therefore a linear scan
-- over at most 'maxChildren' adjacent machine words per level, without
-- touching the children themselves.
module Data.Text.NanoRope.Internal
  ( -- * Types
    Rope (..)
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
import Data.Primitive.PrimArray
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
maxChunk = 1024
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
newtype Rope a = Rope (Node a)

-- | A node of the B-tree. Every field is strict, so a rope in weak head
-- normal form is fully built.
data Node a
  = -- | Metrics, annotation and UTF-8 payload. The payload occupies the
    -- whole array: there is no offset or length to chase.
    Leaf
      {-# UNPACK #-} !Metrics
      !a
      {-# UNPACK #-} !ByteArray
  | -- | Height (at least 1), annotation, children and prefix sums of the
    -- children's metrics. For @n@ children the sums array has @4 * n@
    -- entries: @n@ running totals of bytes, then of chars, UTF-16 units and
    -- newlines, indexed by @'fromEnum' unit * n + child@.
    Inner
      {-# UNPACK #-} !Int
      !a
      {-# UNPACK #-} !(SmallArray (Node a))
      {-# UNPACK #-} !(PrimArray Int)

nodeMetrics :: Node a -> Metrics
nodeMetrics (Leaf m _ _) = m
nodeMetrics (Inner _ _ cs sums) =
  let n = sizeofSmallArray cs
   in Metrics
        (indexPrimArray sums (n - 1))
        (indexPrimArray sums (2 * n - 1))
        (indexPrimArray sums (3 * n - 1))
        (indexPrimArray sums (4 * n - 1))
{-# INLINE nodeMetrics #-}

nodeAnn :: Node a -> a
nodeAnn (Leaf _ a _) = a
nodeAnn (Inner _ a _ _) = a
{-# INLINE nodeAnn #-}

nodeHeight :: Node a -> Int
nodeHeight Leaf{} = 0
nodeHeight (Inner h _ _ _) = h
{-# INLINE nodeHeight #-}

nodeBytes :: Node a -> Int
nodeBytes (Leaf _ _ arr) = sizeofByteArray arr
nodeBytes (Inner _ _ cs sums) = indexPrimArray sums (sizeofSmallArray cs - 1)
{-# INLINE nodeBytes #-}

nodeIsEmpty :: Node a -> Bool
nodeIsEmpty (Leaf _ _ arr) = sizeofByteArray arr == 0
nodeIsEmpty Inner{} = False
{-# INLINE nodeIsEmpty #-}

emptyNode :: Monoid a => Node a
emptyNode = Leaf mempty mempty emptyByteArray

-- | Running total of one unit over children @0..i@.
sumAt :: PrimArray Int -> Int -> Int -> Int -> Int
sumAt sums n ui i = indexPrimArray sums (ui * n + i)
{-# INLINE sumAt #-}

-- | Metrics of children @0..i-1@.
prefixBefore :: PrimArray Int -> Int -> Int -> Metrics
prefixBefore sums n i
  | i == 0 = mempty
  | otherwise =
      Metrics
        (indexPrimArray sums (i - 1))
        (indexPrimArray sums (n + i - 1))
        (indexPrimArray sums (2 * n + i - 1))
        (indexPrimArray sums (3 * n + i - 1))
{-# INLINE prefixBefore #-}

-- | First child whose running total is @>= k@, or the last child.
findChildGE :: PrimArray Int -> Int -> Int -> Int -> Int
findChildGE sums n ui k = go 0
  where
    base = ui * n
    go !i
      | i >= n - 1 = n - 1
      | indexPrimArray sums (base + i) >= k = i
      | otherwise = go (i + 1)
{-# INLINE findChildGE #-}

-- | First child whose running total is @> k@, or the last child.
findChildGT :: PrimArray Int -> Int -> Int -> Int -> Int
findChildGT sums n ui k = go 0
  where
    base = ui * n
    go !i
      | i >= n - 1 = n - 1
      | indexPrimArray sums (base + i) > k = i
      | otherwise = go (i + 1)
{-# INLINE findChildGT #-}

foldAnn :: Monoid a => SmallArray (Node a) -> a
foldAnn cs = go 1 (nodeAnn (indexSmallArray cs 0))
  where
    n = sizeofSmallArray cs
    go !i !acc
      | i >= n = acc
      | otherwise = go (i + 1) (acc <> nodeAnn (indexSmallArray cs i))
{-# INLINE foldAnn #-}

-- | Build an inner node out of at least one child.
mkInner :: Monoid a => SmallArray (Node a) -> Node a
mkInner cs = Inner (nodeHeight (indexSmallArray cs 0) + 1) (foldAnn cs) cs sums
  where
    n = sizeofSmallArray cs
    sums = runPrimArray $ do
      out <- newPrimArray (4 * n)
      let go !i !b !c !u !l
            | i >= n = pure out
            | otherwise = do
                let Metrics b1 c1 u1 l1 = nodeMetrics (indexSmallArray cs i)
                    !b' = b + b1
                    !c' = c + c1
                    !u' = u + u1
                    !l' = l + l1
                writePrimArray out i b'
                writePrimArray out (n + i) c'
                writePrimArray out (2 * n + i) u'
                writePrimArray out (3 * n + i) l'
                go (i + 1) b' c' u' l'
      go 0 0 0 0 0
{-# INLINABLE mkInner #-}
{-# SPECIALIZE mkInner :: SmallArray (Node ()) -> Node () #-}

mkInner2 :: Monoid a => Node a -> Node a -> Node a
mkInner2 x y = mkInner $ runSmallArray $ do
  m <- newSmallArray 2 x
  writeSmallArray m 1 y
  pure m
{-# INLINABLE mkInner2 #-}
{-# SPECIALIZE mkInner2 :: Node () -> Node () -> Node () #-}

-- | Replace child @c@ of an inner node. The prefix sums are patched from the
-- old ones, without visiting the siblings.
replaceChild :: Monoid a => Node a -> Int -> Node a -> Node a
replaceChild Leaf{} _ _ = error "Data.Text.NanoRope: replaceChild on a leaf"
replaceChild (Inner h _ cs sums) c new = Inner h (foldAnn cs') cs' sums'
  where
    n = sizeofSmallArray cs
    Metrics db dc du dl = nodeMetrics new `subMetrics` nodeMetrics (indexSmallArray cs c)
    cs' = replaceAt cs c new
    sums' = runPrimArray $ do
      out <- newPrimArray (4 * n)
      copyPrimArray out 0 sums 0 (4 * n)
      let bump !base !d = when (d /= 0) (loop c)
            where
              loop !i = when (i < n) $ do
                v <- readPrimArray out (base + i)
                writePrimArray out (base + i) (v + d)
                loop (i + 1)
      bump 0 db
      bump n dc
      bump (2 * n) du
      bump (3 * n) dl
      pure out
{-# INLINABLE replaceChild #-}
{-# SPECIALIZE replaceChild :: Node () -> Int -> Node () -> Node () #-}

replaceAt :: SmallArray a -> Int -> a -> SmallArray a
replaceAt arr i x = runSmallArray $ do
  m <- thawSmallArray arr 0 (sizeofSmallArray arr)
  writeSmallArray m i x
  pure m

-- | The children of an inner node together with their prefix sums.
data Kids a = Kids !(SmallArray (Node a)) !(PrimArray Int)

innerOf :: Monoid a => Int -> Kids a -> Node a
innerOf h (Kids cs sums) = Inner h (foldAnn cs) cs sums
{-# INLINE innerOf #-}

-- | @assemble before kids off cnt after@ lines up a few new nodes, children
-- @off .. off + cnt - 1@ of an existing node and a few more new nodes.
--
-- The point is where the prefix sums come from: those of the existing
-- children are the old sums shifted by a constant, so that none of these
-- children is visited. Only the new nodes, which the caller has just built,
-- are asked for their metrics.
assemble :: [Node a] -> Kids a -> Int -> Int -> [Node a] -> Kids a
assemble before (Kids cs sums) off cnt after = Kids cs' sums'
  where
    n = sizeofSmallArray cs
    nb = L.length before
    n' = nb + cnt + L.length after
    cs' = runSmallArray $ do
      out <- newSmallArray n' (error "Data.Text.NanoRope: assemble")
      let put !_ [] = pure ()
          put !i (x : xs) = writeSmallArray out i x >> put (i + 1) xs
      put 0 before
      copySmallArray out nb cs off cnt
      put (nb + cnt) after
      pure out
    sums' = runPrimArray $ do
      out <- newPrimArray (4 * n')
      let fresh !_ !_ !acc [] = pure acc
          fresh u !i !acc (x : xs) = do
            let !acc' = acc + count u (nodeMetrics x)
            writePrimArray out i acc'
            fresh u (i + 1) acc' xs
          unit u = do
            let !ui = fromEnum u
                !to = ui * n'
                !from = ui * n + off
            !acc <- fresh u to 0 before
            -- What the old sums have to be shifted by.
            let !shift = if off == 0 then acc else acc - indexPrimArray sums (from - 1)
                kept !i = when (i < cnt) $ do
                  writePrimArray out (to + nb + i) (indexPrimArray sums (from + i) + shift)
                  kept (i + 1)
            kept 0
            let !acc' = if cnt == 0 then acc else indexPrimArray sums (from + cnt - 1) + shift
            _ <- fresh u (to + nb + cnt) acc' after
            pure ()
      unit Bytes
      unit Chars
      unit Utf16
      unit Lines
      pure out

-- | One set of children after another.
concatKids :: Kids a -> Kids a -> Kids a
concatKids (Kids csl sl) (Kids csr sr) = Kids (csl <> csr) sums'
  where
    nl = sizeofSmallArray csl
    nr = sizeofSmallArray csr
    n' = nl + nr
    sums' = runPrimArray $ do
      out <- newPrimArray (4 * n')
      let unit !ui = do
            copyPrimArray out (ui * n') sl (ui * nl) nl
            let !total = sumAt sl nl ui (nl - 1)
                right !i = when (i < nr) $ do
                  writePrimArray out (ui * n' + nl + i) (sumAt sr nr ui i + total)
                  right (i + 1)
            right 0
      unit 0
      unit 1
      unit 2
      unit 3
      pure out

-- | A root for children @off .. off + cnt - 1@ of some node of height @h@.
sliceNode :: Monoid a => Int -> Kids a -> Int -> Int -> Node a
sliceNode h kids@(Kids cs _) off cnt
  | cnt <= 0 = emptyNode
  | cnt == 1 = indexSmallArray cs off
  | otherwise = innerOf h (assemble [] kids off cnt [])
{-# INLINABLE sliceNode #-}
{-# SPECIALIZE sliceNode :: Int -> Kids () -> Int -> Int -> Node () #-}


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
           in goWord (i + 8) (conts + contCount w) (fours + fourCount w) (nls + nlCount w)
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
              c = 8 - contCount w + (if wide then fourCount w else 0)
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
concat2 a b = runByteArray $ do
  let la = sizeofByteArray a
      lb = sizeofByteArray b
  out <- newByteArray (la + lb)
  copyByteArray out 0 a 0 la
  copyByteArray out la b 0 lb
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
-- taller tree.
data Result a
  = One !(Node a)
  | Two !(Node a) !(Node a)

appendNode :: Measure a => Node a -> Node a -> Node a
appendNode l r
  | nodeIsEmpty l = r
  | nodeIsEmpty r = l
  | otherwise = case merge l r of
      One n -> n
      Two x y -> mkInner2 x y
{-# INLINABLE appendNode #-}
{-# SPECIALIZE appendNode :: Node () -> Node () -> Node () #-}

-- | Merge two non-empty trees whose roots are allowed to be undersized.
-- Walks down the spine of the taller tree to the height of the shorter one,
-- merges there, and propagates at most one extra node back up.
merge :: Measure a => Node a -> Node a -> Result a
merge l r = case compare (nodeHeight l) (nodeHeight r) of
  EQ -> mergeEq l r
  GT -> case l of
    Inner h _ cs sums ->
      let k = sizeofSmallArray cs - 1
       in case merge (indexSmallArray cs k) r of
            One x -> One (replaceChild l k x)
            Two x y -> fromKids h (assemble [] (Kids cs sums) 0 k [x, y])
    Leaf{} -> error "Data.Text.NanoRope: merge"
  LT -> case r of
    Inner h _ cs sums ->
      case merge l (indexSmallArray cs 0) of
        One x -> One (replaceChild r 0 x)
        Two x y -> fromKids h (assemble [x, y] (Kids cs sums) 1 (sizeofSmallArray cs - 1) [])
    Leaf{} -> error "Data.Text.NanoRope: merge"
{-# INLINABLE merge #-}
{-# SPECIALIZE merge :: Node () -> Node () -> Result () #-}

-- | Merge two trees of equal height. Nodes that are both large enough become
-- siblings untouched; otherwise their contents are pooled and redistributed.
mergeEq :: Measure a => Node a -> Node a -> Result a
mergeEq l@(Leaf ml al bl) r@(Leaf mr ar br)
  | sl >= minChunk && sr >= minChunk = Two l r
  | total <= maxChunk = One (Leaf (ml <> mr) (al <> ar) both)
  | otherwise =
      let cut = roundDown both (total `quot` 2)
          mx = sliceMetrics both 0 cut
       in Two
            (mkLeafWith mx (cloneByteArray both 0 cut))
            (mkLeafWith ((ml <> mr) `subMetrics` mx) (cloneByteArray both cut (total - cut)))
  where
    sl = sizeofByteArray bl
    sr = sizeofByteArray br
    total = sl + sr
    both = concat2 bl br
mergeEq l@(Inner h _ csl sl) r@(Inner _ _ csr sr)
  | nl >= minChildren && nr >= minChildren = Two l r
  | nl + nr <= maxChildren = One (innerOf h (concatKids kl kr))
  | half <= nl =
      -- Hand the last children of the left node over to the right one.
      Two
        (innerOf h (assemble [] kl 0 half []))
        (innerOf h (assemble [indexSmallArray csl c | c <- [half .. nl - 1]] kr 0 nr []))
  | otherwise =
      Two
        (innerOf h (assemble [] kl 0 nl [indexSmallArray csr c | c <- [0 .. half - nl - 1]]))
        (innerOf h (assemble [] kr (half - nl) (nr - (half - nl)) []))
  where
    nl = sizeofSmallArray csl
    nr = sizeofSmallArray csr
    half = (nl + nr + 1) `quot` 2
    kl = Kids csl sl
    kr = Kids csr sr
mergeEq _ _ = error "Data.Text.NanoRope: mergeEq"
{-# INLINABLE mergeEq #-}
{-# SPECIALIZE mergeEq :: Node () -> Node () -> Result () #-}

-- | One node of height @h@ if the children fit, else two.
fromKids :: Monoid a => Int -> Kids a -> Result a
fromKids h kids@(Kids cs _)
  | n <= maxChildren = One (innerOf h kids)
  | otherwise =
      let half = (n + 1) `quot` 2
       in Two (innerOf h (assemble [] kids 0 half [])) (innerOf h (assemble [] kids half (n - half) []))
  where
    n = sizeofSmallArray cs
{-# INLINABLE fromKids #-}
{-# SPECIALIZE fromKids :: Int -> Kids () -> Result () #-}

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

-- | The child holding offset @k > 0@, and the offset relative to that child.
descend :: Unit -> Int -> Int -> PrimArray Int -> (Int -> Int -> r) -> r
descend u k n sums cont =
  let ui = fromEnum u
      i = findChildGE sums n ui k
      k' = if i == 0 then k else k - sumAt sums n ui (i - 1)
   in cont i k'
{-# INLINE descend #-}

takeNode :: Measure a => Unit -> Int -> Node a -> Node a
takeNode u k node = case node of
  Leaf m _ arr -> leafPrefix (leafOffset u k m arr) node
  Inner h _ cs sums -> descend u k (sizeofSmallArray cs) sums $ \i k' ->
    let kids = Kids cs sums
        cut = takeNode u k' (indexSmallArray cs i)
     in if i == 0
          then cut
          else
            if nodeIsEmpty cut
              then sliceNode h kids 0 i
              else -- Settle the cut with its sibling, then line up the rest once.
              case merge (indexSmallArray cs (i - 1)) cut of
                One x
                  | i == 1 -> x
                  | otherwise -> innerOf h (assemble [] kids 0 (i - 1) [x])
                Two x y -> innerOf h (assemble [] kids 0 (i - 1) [x, y])
{-# INLINABLE takeNode #-}
{-# SPECIALIZE takeNode :: Unit -> Int -> Node () -> Node () #-}

dropNode :: Measure a => Unit -> Int -> Node a -> Node a
dropNode u k node = case node of
  Leaf m _ arr -> leafSuffix (leafOffset u k m arr) node
  Inner h _ cs sums -> descend u k (sizeofSmallArray cs) sums $ \i k' ->
    let kids = Kids cs sums
        rest = sizeofSmallArray cs - i - 1
        cut = dropNode u k' (indexSmallArray cs i)
     in if rest == 0
          then cut
          else
            if nodeIsEmpty cut
              then sliceNode h kids (i + 1) rest
              else case merge cut (indexSmallArray cs (i + 1)) of
                One x
                  | rest == 1 -> x
                  | otherwise -> innerOf h (assemble [x] kids (i + 2) (rest - 1) [])
                Two x y -> innerOf h (assemble [x, y] kids (i + 2) (rest - 1) [])
{-# INLINABLE dropNode #-}
{-# SPECIALIZE dropNode :: Unit -> Int -> Node () -> Node () #-}

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
      Inner _ _ cs sums ->
        let n = sizeofSmallArray cs
            i = findChildGE sums n (fromEnum u) j
            before = prefixBefore sums n i
         in go (acc <> before) (j - count u before) (indexSmallArray cs i)

-- | Just the byte offset of 'metricsAtNode', which is all that editing needs
-- and spares measuring the prefix of a leaf.
byteOffsetAtNode :: Unit -> Int -> Node a -> Int
byteOffsetAtNode u k root
  | k <= 0 = 0
  | beyondEnd u k (nodeMetrics root) = nodeBytes root
  | otherwise = go 0 k root
  where
    ui = fromEnum u
    go !acc !j node = case node of
      Leaf m _ arr -> acc + leafOffset u j m arr
      Inner _ _ cs sums ->
        let n = sizeofSmallArray cs
            i = findChildGE sums n ui j
         in if i == 0
              then go acc j (indexSmallArray cs 0)
              else go (acc + sumAt sums n 0 (i - 1)) (j - sumAt sums n ui (i - 1)) (indexSmallArray cs i)

-- | The byte at offset @i@, for @0 <= i < size@.
indexByteNode :: Int -> Node a -> Word8
indexByteNode !i node = case node of
  Leaf _ _ arr -> byteAt arr i
  Inner _ _ cs sums ->
    let n = sizeofSmallArray cs
        c = findChildGT sums n 0 i
        before = if c == 0 then 0 else sumAt sums n 0 (c - 1)
     in indexByteNode (i - before) (indexSmallArray cs c)

-- | Bytes @i .. j-1@ as a 'Text', given boundaries @0 <= i <= j <= size@.
-- A range inside a single chunk is returned as a view of that chunk.
sliceToText :: Int -> Int -> Node a -> Text
sliceToText !i !j node
  | i >= j = T.empty
  | otherwise = case node of
      Leaf _ _ arr -> viewSlice arr i (j - i)
      Inner _ _ cs sums ->
        let n = sizeofSmallArray cs
            c = findChildGT sums n 0 i
            before = if c == 0 then 0 else sumAt sums n 0 (c - 1)
         in if j <= sumAt sums n 0 c
              then sliceToText (i - before) (j - before) (indexSmallArray cs c)
              else
                let !(ByteArray ba) = runByteArray $ do
                      out <- newByteArray (j - i)
                      copyRange out 0 i j node
                      pure out
                 in TI.Text (A.ByteArray ba) 0 (j - i)

-- | Copy bytes @i .. j-1@ of a node to offset @d@ of a buffer.
copyRange :: MutableByteArray s -> Int -> Int -> Int -> Node a -> ST s ()
copyRange out !d !i !j node = case node of
  Leaf _ _ arr -> copyByteArray out d arr i (j - i)
  Inner _ _ cs sums ->
    let n = sizeofSmallArray cs
        c0 = findChildGT sums n 0 i
        go !c !start = when (c < n && start < j) $ do
          let end = sumAt sums n 0 c
              lo = max i start
              hi = min j end
          when (lo < hi) $
            copyRange out (d + lo - i) (lo - start) (hi - start) (indexSmallArray cs c)
          go (c + 1) end
     in go c0 (if c0 == 0 then 0 else sumAt sums n 0 (c0 - 1))

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
      Inner _ _ cs sums
        | j <= 0 -> go acc j (indexSmallArray cs 0)
        | otherwise ->
            let n = sizeofSmallArray cs
                i = findChildGE sums n (fromEnum Lines) j
                before = prefixBefore sums n i
             in go (acc <> before) (j - newlines before) (indexSmallArray cs i)

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
      Inner _ _ cs _ ->
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

-- | Replace bytes @i .. j-1@ (code point boundaries) by a slice of a byte
-- array /if/ the edit is confined to one leaf which stays within its size
-- bounds. This is the fast path of typing and erasing: the leaf is copied
-- once and the path to it is patched, with no splitting or merging.
--
-- The first argument is the least size the leaf may shrink to: 'minChunk', or
-- nothing at all for a leaf that is the root. (It is not a 'Bool': with one,
-- GHC's constructor specialisation gets ahead of specialising the measure.)
spliceNode :: Measure a => Int -> Int -> Int -> ByteArray -> Int -> Int -> Node a -> Maybe (Node a)
spliceNode !least !i !j !src !soff !slen node = case node of
  Leaf m _ arr
    | size > maxChunk || size < least -> Nothing
    | otherwise ->
        let m' = (m `subMetrics` sliceMetrics arr i (j - i)) <> sliceMetrics src soff slen
         in Just (mkLeafWith m' (spliceArray arr i j src soff slen))
    where
      size = sizeofByteArray arr - (j - i) + slen
  Inner _ _ cs sums
    | j > sumAt sums n 0 c -> Nothing
    | otherwise ->
        case spliceNode least (i - before) (j - before) src soff slen (indexSmallArray cs c) of
          Nothing -> Nothing
          Just new -> Just (replaceChild node c new)
    where
      n = sizeofSmallArray cs
      c = findChildGT sums n 0 i
      before = if c == 0 then 0 else sumAt sums n 0 (c - 1)
{-# INLINABLE spliceNode #-}
{-# SPECIALIZE spliceNode :: Int -> Int -> Int -> ByteArray -> Int -> Int -> Node () -> Maybe (Node ()) #-}

spliceRoot :: Measure a => Int -> Int -> Text -> Node a -> Node a
spliceRoot i j t@(TI.Text (A.ByteArray ba) off len) root =
  case spliceNode (if nodeHeight root == 0 then 0 else minChunk) i j (ByteArray ba) off len root of
    Just root' -> root'
    Nothing -> takeRoot Bytes i root `appendNode` fromTextNode t `appendNode` dropRoot Bytes j root
{-# INLINABLE spliceRoot #-}
{-# SPECIALIZE spliceRoot :: Int -> Int -> Text -> Node () -> Node () #-}

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
      go (Inner _ a cs _) = rnf a `seq` F.foldl' (\() c -> go c) () cs

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
    go z (Inner _ _ cs _) = F.foldr (flip go) z cs
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
      Inner _ _ cs sums ->
        let n = sizeofSmallArray cs
            c = findChildGT sums n 0 i
         in go (if c == 0 then i else i - sumAt sums n 0 (c - 1)) (indexSmallArray cs c)

------------------------------------------------------------------------------
-- Queries

-- | /O(1)/.
null :: Rope a -> Bool
null (Rope root) = nodeIsEmpty root
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
metrics (Rope root) = nodeMetrics root
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
-- a code point boundary (see 'Unit'). The halves are computed independently
-- and only on demand.
--
-- >>> splitAt Lines 1 "fst\nsnd\n"
-- ("fst\n","snd\n")
splitAt :: Measure a => Unit -> Int -> Rope a -> (Rope a, Rope a)
splitAt u k r = (take u k r, drop u k r)
{-# INLINE splitAt #-}

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
insert :: Measure a => Unit -> Int -> Text -> Rope a -> Rope a
insert u i = replace u i i
{-# INLINE insert #-}

-- | /O(log n)/. @delete u i j@ removes the text from offset @i@ up to
-- offset @j@.
delete :: Measure a => Unit -> Int -> Int -> Rope a -> Rope a
delete u i j = replace u i j T.empty
{-# INLINE delete #-}

-- | /O(log n + length of the text)/. @replace u i j t@ replaces the text
-- from offset @i@ up to offset @j@ by @t@.
--
-- An edit confined to one chunk that neither overflows nor underflows, as
-- nearly all keystrokes are, copies that chunk and the path to it and
-- nothing else.
replace :: Measure a => Unit -> Int -> Int -> Text -> Rope a -> Rope a
replace u i j t r@(Rope root)
  | bi == bj && T.null t = r
  | otherwise = Rope (spliceRoot bi bj t root)
  where
    (bi, bj) = byteRange u i j root
{-# INLINABLE replace #-}

------------------------------------------------------------------------------
-- Lines

-- | /O(log n + length of the line)/. The content of a line by 0-based index,
-- without its terminating @\\n@ or @\\r\\n@; empty if there is no such line.
-- A line within a single chunk is returned as a zero-copy view.
getLine :: Int -> Rope a -> Text
getLine l (Rope root)
  | l < 0 = T.empty
  | otherwise = case localLine l root of
      Just (LocalLine _ _ arr from to) -> viewSlice arr from (to - from)
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
offsetToPosition from to k r = metricsToPosition to (metricsAt from k r) r
{-# INLINE offsetToPosition #-}

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
    go (Inner h _ cs sums) =
      let cs' = mapSmallArray' go cs
       in Inner h (foldAnn cs') cs' sums
{-# INLINABLE remeasure #-}

------------------------------------------------------------------------------
-- Debugging

-- | Violated invariants of the tree; empty for every rope you can build
-- through the public interface with a lawful 'Measure'.
invariants :: (Measure a, Eq a) => Rope a -> [String]
invariants (Rope root) = go True root
  where
    go isRoot node = case node of
      Leaf m a arr ->
        let size = sizeofByteArray arr
         in [ "leaf of " ++ show size ++ " bytes is too large" | size > maxChunk ]
              ++ [ "leaf of " ++ show size ++ " bytes is too small" | not isRoot, size < minChunk ]
              ++ [ "leaf starts inside a code point" | size > 0, isContByte (byteAt arr 0) ]
              ++ [ "leaf caches " ++ show m ++ " instead of " ++ show (naive arr) | m /= naive arr ]
              ++ [ "leaf caches a wrong annotation" | a /= measureChunk (chunkText arr) ]
      Inner h a cs sums ->
        let n = sizeofSmallArray cs
            kids = F.toList cs
            running = L.scanl1 (<>) (map nodeMetrics kids)
         in [ "inner node with " ++ show n ++ " children is too large" | n > maxChildren ]
              ++ [ "inner node with " ++ show n ++ " children is too small" | n < (if isRoot then 2 else minChildren) ]
              ++ [ "child of height " ++ show (nodeHeight c) ++ " below a node of height " ++ show h | c <- kids, nodeHeight c /= h - 1 ]
              ++ [ "wrong prefix sums" | sizeofPrimArray sums /= 4 * n || or [ sumAt sums n (fromEnum u) i /= count u m | (i, m) <- zip [0 ..] running, u <- [minBound .. maxBound] ] ]
              ++ [ "inner node caches a wrong annotation" | a /= mconcat (map nodeAnn kids) ]
              ++ concatMap (go False) kids
    naive arr =
      let bs = [ byteAt arr i | i <- [0 .. sizeofByteArray arr - 1] ]
          cs = L.length (filter (not . isContByte) bs)
       in Metrics (L.length bs) cs (cs + L.length (filter (>= 0xF0) bs)) (L.length (filter (== 0x0A) bs))
