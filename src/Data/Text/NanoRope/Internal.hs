{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedFFITypes #-}
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
-- carries the 'Metrics' of its own subtree unpacked next to its header (a
-- leaf packs them into one word), so seeking by any unit is a linear scan
-- over the heads of at most 'maxChildren' children per level.
--
-- Nothing about a child is kept in its parent but the pointer. An edit
-- therefore copies one small array of pointers per level: persistence is
-- paid for in allocation, and this is what keeps the bill short.
--
-- Nodes are unlifted: a node is never a thunk, and the compiler knows. What
-- seeking reads of a child is then a load and a look at the tag of the
-- pointer. Were they lifted, every child read out of an array would have to
-- be evaluated first, for all that it always is already, and an evaluation
-- inside a loop saves the state of the loop to the stack and fetches it
-- back: that was most of what a descent cost.
--
-- The bill is meant to be exactly that: an edit allocates the new leaf, a
-- node and an array of pointers per level, and the rope; looking something
-- up allocates nothing but the answer. What keeps it so is strictness that
-- the compiler can see, and none of it is visible in the types: offsets and
-- metrics travel unboxed only through arguments that are strict on every
-- path, results that are single-constructor records, and helpers that are
-- either inlined or not at all (see the notes at the seeks and the scans).
-- Check the allocation of the benchmarks after touching any of it.
--
-- = Scanning
--
-- Whatever reads through a chunk (measuring it, finding a line feed,
-- counting code points or UTF-16 code units up to some offset) is one of a
-- few scans. Slices of 32 bytes or more are scanned by C with SIMD
-- instructions, AVX2 or SSE2 as the CPU allows (see 'kernels'); shorter ones,
-- and all of them if the package is built with @-f -simd@, by Haskell
-- reading 8 bytes at a time.
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

-- | Maximum number of bytes in a leaf. No more than 65535 divided by
-- 'maxChildren': the metrics of a leaf are kept in 16 bits each, and those of
-- the leaves of a node are added up that way (see 'sumMetrics').
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

-- | Size of the buffer that 'hPutUtf8' pours the chunks through, in bytes: 32
-- kB. So many chunks, because a chunk has to fit whatever 'maxChunk' is, and
-- no less than the buffer of a handle, which is then bypassed rather than
-- copied into.
outputBuffer :: Int
#ifdef NANO_ROPE_SMALL
-- A few chunks, so that the test suite fills it over and over.
outputBuffer = 4 * maxChunk
#else
outputBuffer = 64 * maxChunk
#endif

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
      (Node a)
      !Unit
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | -- | A tree and a run of keystrokes at one spot of it which are yet to be
    -- inserted. Typing on appends to the run, copying neither a leaf nor the
    -- path to it; whoever wants to read the rope gets the first field, the
    -- one lazy thing in here, which inserts the whole run at once.
    --
    -- The others: the tree without the run, the unit and the offset at
    -- which the run is to go in, its text (at most 'maxPending' bytes), the
    -- metrics of that text packed like those of a leaf, and the room left
    -- for the run to grow.
    --
    -- Every keystroke makes one of these, so it is kept small: where a
    -- keystroke would continue the run and the metrics of everything are
    -- worked out ('typingNext', 'metrics') rather than kept.
    Typing
      (Lazy a)
      (Node a)
      !Unit
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !PackedMetrics
      {-# UNPACK #-} !Int

-- | A node in a box. A node is never a thunk, but the box can be one: this
-- is how the tree of a 'Typing' rope is put off.
data Lazy a = Lazy (Node a)

-- | The tree of a rope, with everything typed in it. Matching evaluates it,
-- as a node is unlifted: the keystrokes that were waiting go in.
pattern Rope :: Node a -> Rope a
pattern Rope root <- (rootOf -> root)
  where
    Rope root = Settled root Bytes (-1) 0

{-# COMPLETE Rope #-}

rootOf :: Rope a -> Node a
rootOf (Settled root _ _ _) = root
rootOf (Typing (Lazy root) _ _ _ _ _ _) = root
{-# INLINE rootOf #-}

-- | A node of the B-tree. It is unlifted and every field is strict, so there
-- is no such thing as a tree that is not fully built.
--
-- Both constructors start with the metrics of their subtree, which is all
-- that seeking reads of a node it does not descend into.
type Node :: Type -> UnliftedType
data Node a
  = -- | Metrics, annotation and UTF-8 payload, which is what the pattern
    -- v'Leaf' matches and builds. The payload occupies the whole array:
    -- there is no offset or length to chase.
    --
    -- None of the metrics of a leaf exceeds 'maxChunk', so the four of them
    -- share a word, 16 bits each (see 'PackedMetrics'). Spelled out they made
    -- the leaves of a document three words heavier each, some 4% of it.
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

-- | The 'Metrics' of at most 65535 bytes of text, 16 bits each: bytes lowest,
-- then code points, UTF-16 code units and line feeds. 64 bits whatever the
-- machine's word is.
type PackedMetrics = Word64

packMetrics :: Metrics -> PackedMetrics
packMetrics (Metrics b c u l) =
  fromIntegral b
    .|. (fromIntegral c `unsafeShiftL` 16)
    .|. (fromIntegral u `unsafeShiftL` 32)
    .|. (fromIntegral l `unsafeShiftL` 48)
{-# INLINE packMetrics #-}

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

-- | Not a constant: there are none of an unlifted type. It is four words to
-- whoever ends up with no text.
emptyNode :: Monoid a => Node a
emptyNode = PackedLeaf 0 mempty emptyByteArray
{-# INLINE emptyNode #-}

------------------------------------------------------------------------------
-- Arrays of nodes

-- | The children of an inner node: a small array whose elements are
-- unlifted, which those of a 'Data.Primitive.SmallArray.SmallArray' cannot
-- be. What follows is as much of its interface as the tree needs.
data Children a = Children (SmallArray# (Node a))

data MutableChildren s a = MutableChildren (SmallMutableArray# s (Node a))

-- | How many children there are.
sizeofChildren :: Children a -> Int
sizeofChildren (Children cs) = I# (sizeofSmallArray# cs)
{-# INLINE sizeofChildren #-}

-- | The child at an index, which is not checked. A load, and no more: what
-- comes out of the array is a node, not something to evaluate to one.
indexChildren :: Children a -> Int -> Node a
indexChildren (Children cs) (I# i) = case indexSmallArray# cs i of (# node #) -> node
{-# INLINE indexChildren #-}

-- | An array of so many children, all of them the given node.
newChildren :: Int -> Node a -> ST s (MutableChildren s a)
newChildren (I# n) node = ST $ \s -> case newSmallArray# n node s of
  (# s', m #) -> (# s', MutableChildren m #)
{-# INLINE newChildren #-}

writeChildren :: MutableChildren s a -> Int -> Node a -> ST s ()
writeChildren (MutableChildren m) (I# i) node = ST $ \s -> (# writeSmallArray# m i node s, () #)
{-# INLINE writeChildren #-}

-- | @copyChildren dst d src off cnt@ copies @cnt@ children of @src@ from
-- @off@ on to @dst@ from @d@ on.
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

------------------------------------------------------------------------------
-- Seeking

-- The seeks are not inlined, and they return strict records rather than
-- unboxed tuples: a record of unpacked fields comes back in registers, but
-- an 'Int' or a 'Metrics' inside an unboxed tuple is a heap object, one for
-- every level of every descent.

-- | A child, and the metrics of the children in front of it.
data Seek = Seek {-# UNPACK #-} !Int {-# UNPACK #-} !Metrics

-- | A child, and an offset relative to it.
data Sought = Sought {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | A child, an offset relative to it, and the bytes in front of it.
data SoughtBytes = SoughtBytes {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | The child holding offset @k@ of a unit: the first one at which the
-- running total reaches @k@, or the last one. Comes with the metrics of the
-- children in front of it.
--
-- Two loops, one to find the child and one to add up what is in front of
-- it. As one loop it had the four sums, the offset and the counters to
-- carry, more than there are registers to carry them in, and spent its time
-- moving them to the stack and back.
--
-- Like 'seekUnit' it is given what the node holds in all, and adds up the
-- children on the shorter side of the one it found.
seekChild :: Unit -> Int -> Metrics -> Children a -> Seek
seekChild !u !k !total !cs = case seekUnit u k (count u total) cs of
  Sought i _
    | 2 * i > n -> Seek i (total `subMetrics` sumMetrics cs i (n - i))
    | otherwise -> Seek i (sumMetrics cs 0 i)
  where
    n = sizeofChildren cs
{-# NOINLINE seekChild #-}

-- | 'seekChild' for when nothing but the unit sought matters: the child and
-- the offset relative to it.
--
-- It is given how much of the unit the node holds in all, which the node
-- knows, and looks for an offset in the second half of that from the last
-- child backwards: a child is a pointer to follow, and this way it follows a
-- quarter of them on average rather than half.
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
        -- The same child, from the other end: the last one with less than k
        -- in front of it, which is the total without the child and what is
        -- after it.
        backwards !i !after
          | i <= 0 = Sought 0 k
          | before < k = Sought i (k - before)
          | otherwise = backwards (i - 1) (total - before)
          where
            before = total - after - sel (nodeMetrics (indexChildren cs i))
    {-# INLINE scan #-}
{-# NOINLINE seekUnit #-}

-- | 'seekUnit' which also counts the bytes in front of the child.
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

-- | The child holding the byte at offset @i@: the first one whose running
-- total exceeds @i@, or the last one. Comes with the offset relative to it.
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

-- | The annotation of a node with these children. A right fold, which for
-- the measure @()@ never gets going, because its '<>' does not look.
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
-- The children of a node are all leaves or none of them is. The metrics of
-- leaves are added up as they are packed, a word a leaf: no field of theirs
-- overflows into the next one, as the leaves of one node hold no more than
-- 'maxChildren' times 'maxChunk' of anything, which is less than 65536.
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

-- | An inner node of known height and metrics, out of at least one child.
-- Most nodes are built from pieces of others, whose metrics add up without
-- another look at the children.
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
replaceAt arr i x = runChildren $ do
  m <- thawChildren arr 0 (sizeofChildren arr)
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
      Chars -> scanUnits False k arr 0 (sizeofByteArray arr)
      Utf16 -> scanUnits True k arr 0 (sizeofByteArray arr)
      Lines -> scanLines k arr

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

-- | Is this all ASCII? Then bytes, code points and UTF-16 code units are the
-- same thing and nothing needs to be scanned to convert between them.
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

-- | 'leafPrefixMetrics' for whoever knows the line feeds among those bytes
-- already, having counted them to get there: in a leaf that is all ASCII,
-- that is all there is to know, and nothing is scanned.
leafPrefixWithLines :: Metrics -> ByteArray -> Int -> Int -> Metrics
leafPrefixWithLines m arr b nls
  | b <= 0 = mempty
  | b >= bytes m = m
  | isAscii m = Metrics b b b nls
  | otherwise = leafCutMetrics m arr b
{-# INLINE leafPrefixWithLines #-}

-- | 'leafPrefixMetrics' of a cut inside the leaf. Apart from the above
-- because it never returns the metrics it is given, which then reach it
-- unboxed.
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

-- | Where a line of a chunk starts, and where the @\\n@ that ends it is, or
-- the size of the chunk.
data ChunkLine = ChunkLine {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | Line @k@ of a chunk, which starts just after its @k@-th @\\n@, or at 0
-- for the first. 'scanLines' and 'findNewline' in one go, which is one foreign
-- call rather than two for whoever is after a line.
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

-- | One implementation of the scans over chunks, as the functions above use
-- them. Exposed so that the test suite can hold them all to the same results.
data Kernels = Kernels
  { kernelsName :: String
  , kernelMetrics :: ByteArray -> Int -> Int -> Metrics
  -- ^ Like 'sliceMetrics'.
  , kernelNewlines :: ByteArray -> Int -> Int -> Int
  -- ^ Line feeds in a slice.
  , kernelFindNewline :: ByteArray -> Int -> Int
  -- ^ The first line feed at or after an offset, or the size.
  , kernelFindNewlineBack :: ByteArray -> Int -> Int
  -- ^ The last line feed before an offset, or -1.
  , kernelNthNewline :: Int -> ByteArray -> Int
  -- ^ Just after the @k@-th line feed, for @k >= 1@, or the size.
  , kernelScanUnits :: Bool -> Int -> ByteArray -> Int -> Int -> Int
  -- ^ @kernelScanUnits wide k arr from to@: from a code point boundary, the
  -- offset in front of the first code point that does not fit into @k@ code
  -- points (UTF-16 code units if @wide@), or @to@.
  , kernelLineSpan :: Int -> ByteArray -> (Int, Int)
  -- ^ Just after the @k@-th line feed, or 0 for @k <= 0@, and the first line
  -- feed at or after that, or the size.
  }

-- | The scans in Haskell, 8 bytes at a time.
swarKernels :: Kernels
swarKernels =
  Kernels "Haskell" swarMetrics swarNewlines swarFindNewline swarFindNewlineBack swarScanLines swarScanUnits $ \k arr ->
    case swarLineSpan k arr of ChunkLine from lf -> (from, lf)

-- | Every implementation this machine runs: the Haskell one, then those in C
-- by level of SIMD support. The last one is used on long slices.
kernels :: [Kernels]
kernels = swarKernels : map simdKernels [0 .. simdLevel]

-- | Slices at least this long are scanned in C: long enough to make up for a
-- foreign call, which costs a few nanoseconds.
--
-- The scans above choose between the C and the Haskell themselves, with calls
-- to known functions. Picked out of a t'Kernels' they were calls to unknown
-- ones wherever the choice did not inline away, with every argument and
-- result in a box.
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
    (cNthNewline level)
    (cScanUnits level)
    (\k arr -> case cLineSpan level k arr of ChunkLine from lf -> (from, lf))

-- The scans of a t'Kernels' in C, given the level of SIMD support.
cMetrics :: Int -> ByteArray -> Int -> Int -> Metrics
cNewlines :: Int -> ByteArray -> Int -> Int -> Int
cFindNewline :: Int -> ByteArray -> Int -> Int
cFindNewlineBack :: Int -> ByteArray -> Int -> Int
cNthNewline :: Int -> Int -> ByteArray -> Int
cScanUnits :: Int -> Bool -> Int -> ByteArray -> Int -> Int -> Int
cLineSpan :: Int -> Int -> ByteArray -> ChunkLine

#ifdef NANO_ROPE_SIMD
-- The scans in C (cbits/scan.c), with SSE2 or AVX2. Unsafe calls, which may
-- be handed the payload of an unpinned array: the garbage collector cannot
-- run during one.

#ifdef NANO_ROPE_SMALL
-- Everything, so that the tiny chunks of the test suite go through the C.
simdMin = 0
#else
simdMin = 32
#endif

-- Asked once: a pure foreign call would be inlined into every scan and ask
-- again each time.
simdLevel = unsafeDupablePerformIO c_simdLevel
{-# NOINLINE simdLevel #-}

-- The C counts continuation bytes, 4-byte leaders and line feeds in 21 bits
-- each; chunks are nowhere near that long.
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

-- | The byte at an offset of what 'spliceArray' makes of the same arguments,
-- without making it.
splicedByte :: ByteArray -> Int -> Int -> ByteArray -> Int -> Int -> Int -> Word8
splicedByte arr i j src soff slen at
  | at < i = byteAt arr at
  | at < i + slen = byteAt src (soff + at - i)
  | otherwise = byteAt arr (j + at - i - slen)
{-# INLINE splicedByte #-}

-- | Bytes @from .. to-1@ of what 'spliceArray' makes of the same arguments,
-- without making it: a leaf that overflows goes straight into its two halves.
splicedSlice :: ByteArray -> Int -> Int -> ByteArray -> Int -> Int -> Int -> Int -> ByteArray
splicedSlice !arr !i !j !src !soff !slen !from !to = runByteArray $ do
  out <- newByteArray (to - from)
  -- What is spliced is three pieces, each the bytes from some offset of an
  -- array; of each, what falls into the slice.
  let piece !start !end source !sourceOff =
        let lo = max from start
            hi = min to end
         in when (lo < hi) $ copyByteArray out (lo - from) source (sourceOff + lo - start) (hi - lo)
  piece 0 i arr 0
  piece i (i + slen) src soff
  piece (i + slen) (sizeofByteArray arr - (j - i) + slen) arr j
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

-- | A tree over more than 'maxChunk' bytes, cut into evenly sized leaves.
-- Aiming a little below 'maxChunk' leaves room for moving every cut back to a
-- code point boundary.
--
-- Built from the top, every node straight into the array of its parent: with
-- the leaves in a list and a list for every level above them, loading made
-- two thirds of the text's size in garbage.
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
        -- The children are spread evenly, so that no parent ends up below
        -- 'minChildren'.
        let !cq = children `quot` parents
            !cr = children `rem` parents
            !first = j * cq + min j cr
            !size = if j < cr then cq + 1 else cq
         in mkInner $ runChildren $ do
              -- At least one child, and the array starts out full of it.
              m <- newChildren size (node below first)
              let go !i
                    | i >= size = pure m
                    | otherwise = do
                        writeChildren m i (node below (first + i))
                        go (i + 1)
              go 1
      [_] -> let !from = cut j in mkLeaf (cloneByteArray arr from (cut (j + 1) - from))
      [] -> emptyNode
{-# INLINABLE treeFromSlice #-}
{-# SPECIALIZE treeFromSlice :: ByteArray -> Int -> Int -> Node () #-}

-- | How many nodes each level of a tree over so many leaves has, from the
-- root down to the leaves.
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

-- | Outcome of merging two trees: one or two nodes of the height of the
-- taller tree. Also the outcome of an edit within a leaf, which is 'None' if
-- the edit does not stay there.
--
-- An unboxed sum, so that handing a node up a level allocates nothing.
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
  | k <= 0 = (# emptyNode, root #)
  | beyondEnd u k (nodeMetrics root) = (# root, emptyNode #)
  | otherwise = splitNode u k root
{-# INLINABLE splitRoot #-}
{-# SPECIALIZE splitRoot :: Unit -> Int -> Node () -> (# Node (), Node () #) #-}

-- | Children @0 .. i-1@ of a node of height @h@, whose metrics are @before@,
-- followed by a lower tree, which may be empty or undersized: it is settled
-- with its sibling, and the rest is lined up once.
joinLeft :: Measure a => Int -> Children a -> Int -> Metrics -> Node a -> Node a
joinLeft !h !cs !i !before !l
  | i == 0 = l
  | nodeIsEmpty l = if i == 1 then indexChildren cs 0 else inner h before (cloneChildren cs 0 i)
  | otherwise = case merge (indexChildren cs (i - 1)) l of
      One x
        | i == 1 -> x
        | otherwise -> inner h m $ runChildren $ do
            out <- thawChildren cs 0 i
            writeChildren out (i - 1) x
            pure out
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
        | otherwise -> inner h m $ runChildren $ do
            out <- thawChildren cs (i + 1) rest
            writeChildren out 0 x
            pure out
      Two x y -> inner h m (cons2 x y cs (i + 2))
      None -> unreachable "joinRight"
  where
    rest = sizeofChildren cs - i - 1
    m = nodeMetrics r <> after
{-# INLINABLE joinRight #-}
{-# SPECIALIZE joinRight :: Int -> Children () -> Int -> Metrics -> Node () -> Node () #-}

takeNode :: Measure a => Unit -> Int -> Node a -> Node a
takeNode u k node = case node of
  Leaf m _ arr -> leafPrefix (leafOffset u k m arr) node
  Inner total h _ cs -> case seekChild u k total cs of
    Seek i before -> joinLeft h cs i before (takeNode u (k - count u before) (indexChildren cs i))
{-# INLINABLE takeNode #-}
{-# SPECIALIZE takeNode :: Unit -> Int -> Node () -> Node () #-}

dropNode :: Measure a => Unit -> Int -> Node a -> Node a
dropNode u k node = case node of
  Leaf m _ arr -> leafSuffix (leafOffset u k m arr) node
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
      Leaf m _ arr
        -- Just after the j-th line feed of this leaf, which it has: there
        -- are j of them before that.
        | u == Lines -> acc <> leafPrefixWithLines m arr (scanLines j arr) j
        | otherwise -> acc <> leafPrefixMetrics m arr (leafOffset u j m arr)
      Inner total _ _ cs -> case seekChild u j total cs of
        Seek i before -> go (acc <> before) (j - count u before) (indexChildren cs i)

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

-- | Copy bytes @i .. j-1@ of a node to offset @d@ of a buffer.
--
-- Only the children at the two ends of the range can be cut by it. The ones
-- in between go to 'copyNode', and they are all of them for 'toText'.
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

-- | Copy all of a node to offset @d@ of a buffer, and return the offset after
-- it. There is nothing to seek and nothing to clamp, which is worth an eighth
-- to a fifth of 'toText'. What is left is a @memcpy@ of the document.
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

-- | Locations of the start of line @l@ and of the end of its content, that
-- is before the terminating @\n@ or @\r\n@, or at the end of the rope.
data Span = Span {-# UNPACK #-} !Metrics {-# UNPACK #-} !Metrics

lineSpan :: Int -> Node a -> Span
lineSpan !l root = Span start (lineEnd start (metricsAtNode Lines (max 0 l + 1) root) root)
  where
    start = metricsAtNode Lines l root
{-# NOINLINE lineSpan #-}

-- | End of the content of the line starting at @start@, given the start of
-- the next line.
lineEnd :: Metrics -> Metrics -> Node a -> Metrics
lineEnd start next root
  | newlines next == newlines start = next
  | bytes lf > bytes start && indexByteNode (bytes lf - 1) root == 0x0D = lf `subMetrics` Metrics 1 1 1 0
  | otherwise = lf
  where
    lf = next `subMetrics` Metrics 1 1 1 1
{-# INLINE lineEnd #-}

-- | The end of the content of a line that starts at @from@ and is terminated
-- by the line feed at @lf@ of the same chunk.
contentEnd :: ByteArray -> Int -> Int -> Int
contentEnd arr from lf
  | lf > from && byteAt arr (lf - 1) == 0x0D = lf - 1
  | otherwise = lf
{-# INLINE contentEnd #-}

-- | The text of line @l >= 0@. A line that starts and is terminated within
-- one leaf, as most lines are, is found in a single descent and returned as
-- a view of that leaf.
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

-- | The position of an offset. Found in a single descent if the line it is
-- on starts within the same leaf (as most lines do) or with the rope.
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

metricsAtPositionNode :: Unit -> Position -> Node a -> Metrics
metricsAtPositionNode u pos root = case linePositionNode False u pos root of
  Span _ at -> at
{-# INLINE metricsAtPositionNode #-}

-- | Where the line of a position starts, and where the position is. The
-- former only if asked for: it is not always free.
linePositionNode :: Bool -> Unit -> Position -> Node a -> Span
linePositionNode !wanted !u (Position l0 c) root
  | l > newlines (nodeMetrics root) = general
  | otherwise = go mempty l root
  where
    l = max 0 l0
    -- A line that starts and is terminated within one leaf, as most lines
    -- are, is found in a single descent.
    go !acc !j node = case node of
      Leaf m _ arr -> case chunkLine j arr of
        ChunkLine from lf ->
          if lf >= sizeofByteArray arr
            then general
            else
              let !to = contentEnd arr from lf
                  !b = column m arr from to
                  -- The line starts after the j-th line feed of the leaf,
                  -- and there is none between there and the column. The
                  -- start of the line is the way back over the column,
                  -- which is short, rather than another prefix to count.
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
      | otherwise = case u of
          Bytes -> roundDownFrom arr (from + c)
          Utf16 -> scanUnits True c arr from to
          _ -> scanUnits False c arr from to
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
editNode :: Measure a => Int -> Unit -> Int -> Int -> ByteArray -> Int -> Int -> Node a -> (# Result a, Int# #)
editNode !least u !k !d !src !soff !slen node = case node of
  Leaf m _ arr
    | d > 0 && kj > count u m -> (# None, 0# #)
    | slen <= 0 && bj <= bi -> (# None, 0# #)
    | size' < least || size' > 2 * maxChunk - 8 -> (# None, 0# #)
    | size' <= maxChunk -> roomy (One (mkLeafWith m' (spliceArray arr bi bj src soff slen))) (maxChunk - size')
    | otherwise ->
        -- Room for moving the cut back to a code point boundary is what the
        -- 8 bytes above are for.
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

-- | Replace the text from offset @i@ up to offset @j@. Comes with the room
-- of 'editNode', or none if it does not know.
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
    -- Both offsets as bytes. Offsets of the original rope rather than of some
    -- intermediate result, so that they round the same way as everywhere
    -- else.
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

-- | A rope with a run of keystrokes, of the given metrics, to be inserted at
-- an offset.
typing :: Measure a => Node a -> Unit -> Int -> ByteArray -> Metrics -> Int -> Rope a
typing base u start run typed room =
  -- The box is the thunk: the run goes in when someone opens it.
  let root = Lazy (edited u start start (chunkText run) base)
   in Typing root base u start run (packMetrics typed) room
{-# INLINE typing #-}

-- | The offset at which a keystroke would continue a run.
typingNext :: Unit -> Int -> PackedMetrics -> Int
typingNext u start typed = start + count u (unpackMetrics typed)
{-# INLINE typingNext #-}

-- | Insert text at an offset. The first insertion at some place goes into the
-- tree and leaves a note of where it ended. One that starts there is taken
-- for typing and begins a run, which the ones after it add to.
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

-- | Delete the text from offset @i@ up to offset @j > i@. Erasing the end of
-- what has just been typed shortens the run, given that the run is where its
-- offsets say, which is known of code points that are not beyond the end.
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

-- | /O(n)/. The text is copied into chunks, except that a text of at most
-- 'maxChunk' bytes which owns its whole buffer is shared.
fromText :: Measure a => Text -> Rope a
fromText t = Rope (fromTextNode t)
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
-- The rope is behind a lambda, here and in foldlChunks': a fold is inlined
-- once it has the arguments left of the equals sign, and one written
-- "foldrChunks f z" would otherwise stay a call of an unknown f per chunk,
-- four times slower.
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

-- | Strict left fold over the chunks of 'toChunks'. It is a walk of the tree
-- and allocates nothing of its own, where the list of 'toChunks' costs a
-- hundred bytes or so a chunk: the fold for whoever consumes a whole rope,
-- to hash it or to hand it to a parser or a socket.
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

-- | Write the text to a handle as UTF-8, which is what the chunks hold
-- already: they are poured through one small buffer, and no 'Text' of the
-- whole document is made on the way as it would be by way of 'toText'.
--
-- Like 'hPutBuf' this writes bytes. The encoding and the newline mode of the
-- handle have no say, so a @\\r\\n@ in the rope is a @\\r\\n@ in the file on
-- every platform. That is what a file, a pipe or a socket wants; a console
-- may not, and text for one is better off as 'toLazyText'.
hPutUtf8 :: Handle -> Rope a -> IO ()
hPutUtf8 h (Rope root) = do
  buf <- newPinnedByteArray outputBuffer
  withMutableByteArrayContents buf $ \ptr -> do
    I# used <- pourNode h buf ptr 0 root
    flushBuffer h ptr used

-- | Copy a node into a buffer of 'outputBuffer' bytes, @used@ of which are
-- taken, writing the buffer out whenever the next chunk would not fit.
-- Returns how much of it is taken then.
--
-- At the top level for the sake of that number: as a loop local to
-- 'hPutUtf8' it was a box for every chunk.
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

-- | Write out what is in the buffer.
--
-- 'hPutBuf' wants the number in a box. It is made here, out of sight: a
-- function that hands on a box it was given is given one by its callers in
-- turn, and for 'pourNode' that was a box for every chunk.
flushBuffer :: Handle -> Ptr Word8 -> Int# -> IO ()
flushBuffer h ptr used# = when (used > 0) $ hPutBuf h ptr used
  where
    used = I# used#
{-# NOINLINE flushBuffer #-}

-- | Write the text to a file as UTF-8 with 'hPutUtf8', replacing what was
-- there.
--
-- The rope is evaluated first, keystrokes that were waiting and their
-- measure included: should that fail, the file is as it was.
writeFileUtf8 :: FilePath -> Rope a -> IO ()
writeFileUtf8 path rope@(Rope _) = withBinaryFile path WriteMode (`hPutUtf8` rope)

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
        Sought c i' -> go i' (indexChildren cs c)

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
metrics (Typing _ base _ _ _ typed _) = nodeMetrics base <> unpackMetrics typed
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

-- | /O(log n)/. @slice u i j@ is the text from offset @i@ up to offset @j@.
--
-- Nothing above the lowest node that holds all of the range is looked at
-- twice or rebuilt, and a range within a single chunk is one descent and a
-- copy of those bytes.
slice :: Measure a => Unit -> Int -> Int -> Rope a -> Rope a
slice u i j (Rope root)
  | j <= i || j <= 0 = empty
  | beyondEnd u j (nodeMetrics root) = Rope (dropRoot u i root)
  | otherwise = Rope (sliceNode u (max 0 i) j root)
{-# INLINABLE slice #-}

-- | The text from offset @i@ up to offset @j@ of a node, for @0 <= i < j@
-- and @j@ not beyond its end.
--
-- Both offsets are followed down as long as they lead into the same child.
-- Where they part they become bytes. They are offsets of the original rope
-- all along rather than of some intermediate result, so that they round the
-- same way as everywhere else.
sliceNode :: Measure a => Unit -> Int -> Int -> Node a -> Node a
sliceNode !u !i !j node = case node of
  Leaf m _ arr ->
    let !bi = leafOffset u i m arr
        !bj = leafOffset u j m arr
     in if bi <= 0 && bj >= sizeofByteArray arr
          then node
          else
            if bj <= bi
              then emptyNode
              else mkLeafWith (leafSliceMetrics m arr bi (bj - bi)) (cloneByteArray arr bi (bj - bi))
  Inner total _ _ cs -> case seekUnit u i (count u total) cs of
    Sought c i'
      | j' <= count u (nodeMetrics child) -> sliceNode u i' j' child
      | otherwise -> dropRoot Bytes (byteOffsetAtNode u i node) (takeRoot Bytes (byteOffsetAtNode u j node) node)
      where
        child = indexChildren cs c
        j' = j - (i - i')
{-# INLINABLE sliceNode #-}
{-# SPECIALIZE sliceNode :: Unit -> Int -> Int -> Node () -> Node () #-}

-- | /O(log n + length of the result)/. Like 'slice', but straight to 'Text'
-- without building a rope in between. A range within a single chunk is
-- found in one descent and returned as a zero-copy view of that chunk.
sliceText :: Unit -> Int -> Int -> Rope a -> Text
sliceText u i j (Rope root)
  | j <= i || j <= 0 = T.empty
  | beyondEnd u j (nodeMetrics root) = sliceToText (byteOffsetAtNode u i root) (nodeBytes root) root
  | otherwise = sliceTextNode u (max 0 i) j root

-- | 'sliceNode' as a 'Text'.
sliceTextNode :: Unit -> Int -> Int -> Node a -> Text
sliceTextNode !u !i !j node = case node of
  Leaf m _ arr ->
    let !bi = leafOffset u i m arr
        !bj = leafOffset u j m arr
     in viewSlice arr bi (bj - bi)
  Inner total _ _ cs -> case seekUnit u i (count u total) cs of
    Sought c i'
      | j' <= count u (nodeMetrics child) -> sliceTextNode u i' j' child
      | otherwise -> sliceToText (byteOffsetAtNode u i node) (byteOffsetAtNode u j node) node
      where
        child = indexChildren cs c
        j' = j - (i - i')

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
  | otherwise = lineText l root

-- | /O(n)/. The lines of the rope without their terminators, lazily. Like
-- 'Data.Text.lines', a trailing @\\n@ does not start another line; unlike
-- it, @\\r\\n@ is stripped too. Lines within a single chunk are zero-copy
-- views.
lines :: Rope a -> [Text]
lines (Rope root) = go [] (foldrNode (:) [] root)
  where
    -- The pieces of an unfinished line, last one first. Never just empty
    -- pieces: a piece is only carried over if it is the non-empty rest of a
    -- chunk.
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

-- | /O(log n)/. Where the line of a position starts, and 'metricsAtPosition':
-- @('metricsAt' 'Lines' line, 'metricsAtPosition' u position)@, out of one
-- descent where those are two.
--
-- The difference of the two is the column that was reached, in every unit
-- at once. That converts a column from one unit to another, and tells a
-- column that was clamped to the end of its line, or rounded down to the
-- start of a code point, from one that is where it was asked for:
--
-- >>> let (line, at) = metricsAtLineAndPosition Utf16 (Position 1 3) "a😀\nb😀c"
-- >>> (utf16Units at - utf16Units line, chars at - chars line, bytes at)
-- (3,2,11)
metricsAtLineAndPosition :: Unit -> Position -> Rope a -> (Metrics, Metrics)
metricsAtLineAndPosition u pos (Rope root) = case linePositionNode True u pos root of
  Span line at -> (line, at)
{-# INLINE metricsAtLineAndPosition #-}

-- | /O(log n)/. The position, with its column in the given unit, of a
-- location obtained from 'metricsAt', 'metricsAtPosition' or 'metricsWhere'.
metricsToPosition :: Unit -> Metrics -> Rope a -> Position
metricsToPosition u m (Rope root) = positionOfMetrics u m root

-- | /O(log n)/. @offsetToPosition from to@ turns an offset in unit @from@
-- into a position with its column in unit @to@.
--
-- >>> offsetToPosition Bytes Utf16 11 "a😀\nb😀c"
-- Position {posLine = 1, posColumn = 3}
offsetToPosition :: Unit -> Unit -> Int -> Rope a -> Position
offsetToPosition from to k (Rope root) = positionAtNode from to k root

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
remeasure :: forall a b. Measure b => Rope a -> Rope b
remeasure (Rope root) = Rope (go root)
  where
    go :: Node a -> Node b
    go (Leaf m _ arr) = mkLeafWith m arr
    go (Inner m h _ cs) = inner h m $ runChildren $ do
      let n = sizeofChildren cs
      out <- newChildren n (go (indexChildren cs 0))
      let fill !i
            | i >= n = pure out
            | otherwise = writeChildren out i (go (indexChildren cs i)) >> fill (i + 1)
      fill 1
{-# INLINABLE remeasure #-}

------------------------------------------------------------------------------
-- Debugging

-- | Violated invariants of the tree; empty for every rope you can build
-- through the public interface with a lawful 'Measure'.
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
            -- In boxes: there is no list of what is unlifted.
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
