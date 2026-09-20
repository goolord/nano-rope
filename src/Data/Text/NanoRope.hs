-- |
-- Module      : Data.Text.NanoRope
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- A persistent UTF-8 text rope for editors, language servers, and parsers.
--
-- * __Multi-unit indexing.__ Cached metrics support /O(log n)/ lookups and
--   conversions in bytes, code points, UTF-16 code units, and lines.
-- * __Small edits.__ A B-tree of chunks up to 512 bytes shares unchanged
--   subtrees between versions. Consecutive keystrokes can be buffered; see
--   'insert'.
-- * __Bounded chunks.__ Chunk sizes depend on bytes, not line lengths.
-- * __Chunk-based I/O.__ Read chunk views or stream UTF-8 without flattening
--   the document.
-- * __Custom summaries.__ "Data.Text.NanoRope.Measured" adds cached monoidal
--   measures and searches over them.
--
-- Import this module qualified:
--
-- > import Data.Text.NanoRope (Rope, Unit (..), Position (..))
-- > import qualified Data.Text.NanoRope as Rope
--
-- Offsets are zero-based, clamped to the document, and rounded down to code
-- point boundaries. Ranges are half-open: the start is included and the end
-- is excluded. 'Chars' counts code points, not grapheme clusters or display
-- columns. Only @\\n@ starts a new line.
--
-- Complexity bounds use /n/ for the document's byte length. They assume the
-- tree is evaluated: a read after buffered typing may first apply a pending
-- insertion. 'null', 'length', 'lineCount', and 'metrics' do not force it.
--
-- = Example: a language server
--
-- Convert a client's UTF-16 range to byte offsets before replacing it.
-- The returned offsets refer to the original document.
--
-- > import Data.Text (Text)
-- >
-- > applyChange :: Position -> Position -> Text -> Rope -> (Rope, (Int, Int))
-- > applyChange from to new rope = (Rope.replace Bytes i j new rope, (i, j))
-- >   where
-- >     i = Rope.positionToOffset Utf16 Bytes from rope
-- >     j = Rope.positionToOffset Utf16 Bytes to rope
module Data.Text.NanoRope
  ( -- * Ropes
    Rope

    -- * Units and metrics
  , Unit (..)
  , Metrics (..)
  , count

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
    -- $conversions
  , metricsAt
  , convert

    -- * Positions
  , Position (..)
  , splitAtPosition
  , metricsAtPosition
  , metricsAtLineAndPosition
  , metricsToPosition
  , offsetToPosition
  , positionToOffset

    -- * Searching by metrics
  , splitWhere
  , metricsWhere

    -- * Custom measures
  , measured
  , unmeasured
  ) where

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.NanoRope.Internal (Measure, Metrics (..), Position (..), Unit (..), count)
import qualified Data.Text.NanoRope.Internal as M
import System.IO (Handle)
import Prelude hiding (drop, getLine, length, lines, null, splitAt, take)

-- | A rope with only the built-in metrics. This is the measured rope
-- specialised to @()@, with the same 'Eq', 'Ord', 'Show', 'Semigroup',
-- 'Monoid', 'Data.String.IsString', and 'Control.DeepSeq.NFData' instances.
type Rope = M.Rope ()

-- $conversions
-- Prefix t'Metrics' describe a location in all four units. Obtain them with
-- 'metricsAt', 'metricsAtPosition', or 'metricsWhere', then use 'count' for
-- absolute offsets. Reusing the metrics avoids repeating the lookup for
-- each unit. 'metricsToPosition' also looks up the line start to calculate
-- a column; use metrics from the same rope.

-- | The empty rope.
empty :: Rope
empty = M.empty

-- | A rope of one character.
singleton :: Char -> Rope
singleton = M.singleton

-- | /O(n)/. Build a rope from strict text. Copies the text into chunks,
-- unless it is at most 512 bytes and occupies its entire backing buffer.
fromText :: Text -> Rope
fromText = M.fromText

-- | Build a rope by appending the chunks of a lazy 'TL.Text'.
fromLazyText :: TL.Text -> Rope
fromLazyText = M.fromLazyText

-- | /O(n)/. Flatten the rope to strict text. A single chunk is shared
-- without copying; multiple chunks are copied into one buffer.
toText :: Rope -> Text
toText = M.toText

-- | /O(n)/. Convert to lazy text, sharing the chunk buffers.
toLazyText :: Rope -> TL.Text
toLazyText = M.toLazyText

-- | /O(n)/. Decode the rope to a 'String'.
toString :: Rope -> String
toString = M.toString

-- | The chunks of the rope as zero-copy views, in order. They are non-empty,
-- at most 512 bytes long and produced lazily.
toChunks :: Rope -> [Text]
toChunks = M.toChunks

-- | Lazy right fold over non-empty chunks in document order, without
-- building the list returned by 'toChunks'.
foldrChunks :: (Text -> b -> b) -> b -> Rope -> b
foldrChunks = M.foldrChunks

-- | Strict left fold over non-empty chunks in document order. Walks the
-- tree directly, sharing text buffers and avoiding an intermediate list.
-- Useful for consumers such as hashes and parsers.
foldlChunks' :: (b -> Text -> b) -> b -> Rope -> b
foldlChunks' = M.foldlChunks'

-- | /O(log n)/. Zero-copy view of the rest of the chunk containing the given
-- offset. Returns empty text when the clamped offset is at the end.
--
-- For a parser read callback, request a byte offset, consume the returned
-- text, then advance by its byte length. Offsets are clamped and rounded
-- as described at 'Unit'.
chunkAt :: Unit -> Int -> Rope -> Text
chunkAt = M.chunkAt

-- | /O(n)/. Write UTF-8 to a handle through a 32 KiB buffer, without
-- constructing a 'Text' for the whole document.
--
-- Like 'System.IO.hPutBuf', this bypasses the handle's encoding and newline
-- translation, preserving the rope's bytes on every platform. To use the
-- handle's text encoding instead, pass 'toLazyText' to text I/O.
hPutUtf8 :: Handle -> Rope -> IO ()
hPutUtf8 = M.hPutUtf8

-- | Write UTF-8 to a file with 'hPutUtf8', replacing its contents.
--
-- Evaluates the tree, including pending input, before opening the file.
-- An evaluation failure leaves an existing file untouched. The write itself
-- is not atomic.
writeFileUtf8 :: FilePath -> Rope -> IO ()
writeFileUtf8 = M.writeFileUtf8

-- | /O(1)/. Whether the rope is empty, including pending input.
null :: Rope -> Bool
null = M.null

-- | /O(1)/. Length in any unit; for 'Lines' this is the number of @\\n@.
--
-- >>> map (`length` "a😀\nb") [Bytes, Chars, Utf16, Lines]
-- [7,4,5,1]
length :: Unit -> Rope -> Int
length = M.length

-- | /O(1)/. Number of @\\n@ characters plus one. An empty rope has one line;
-- a trailing @\\n@ adds an empty final line. Valid indices range from zero
-- to @lineCount rope - 1@. See 'lines' for a list that omits that final empty line.
lineCount :: Rope -> Int
lineCount = M.lineCount

-- | /O(1)/. All built-in measurements, including pending input.
metrics :: Rope -> Metrics
metrics = M.metrics

-- | /O(log n)/. Concatenate two ropes, sharing unaffected subtrees.
-- Equivalent to '<>'. The traversal follows the difference in tree heights.
append :: Rope -> Rope -> Rope
append = M.append

-- | /O(log n)/. Split at an offset, clamped to the rope and rounded down to
-- a code point boundary (see 'Unit'). Finds both halves in one descent.
-- Use 'take' or 'drop' if you need only one half.
--
-- >>> splitAt Lines 1 "fst\nsnd\n"
-- ("fst\n","snd\n")
splitAt :: Unit -> Int -> Rope -> (Rope, Rope)
splitAt = M.splitAt

-- | /O(log n)/. The prefix before an offset, clamped and rounded as in 'splitAt'.
take :: Unit -> Int -> Rope -> Rope
take = M.take

-- | /O(log n)/. The suffix from an offset, clamped and rounded as in 'splitAt'.
drop :: Unit -> Int -> Rope -> Rope
drop = M.drop

-- | /O(log n)/. Extract the half-open range @[i, j)@. Both offsets are
-- clamped and rounded in the original rope. Returns empty when @j <= i@.
slice :: Unit -> Int -> Int -> Rope -> Rope
slice = M.slice

-- | /O(log n + result bytes)/. Like 'slice', but returns 'Text' directly.
-- A range within one chunk is a zero-copy view; a range spanning chunks
-- is copied into one buffer.
sliceText :: Unit -> Int -> Int -> Rope -> Text
sliceText = M.sliceText

-- | /O(log n + inserted bytes)/. Insert text at a clamped, code-point-aligned
-- offset. Empty input leaves the rope unchanged.
--
-- Small insertions copy the affected chunk and its path through the tree.
-- An overflowing chunk can split in two.
--
-- Consecutive insertions in the same unit ('Bytes', 'Chars', or 'Utf16')
-- can use a buffer of up to 128 bytes, limited by the target chunk's free
-- space. Updating that bounded buffer is /O(1)/ in document size. A tree
-- read, an edit elsewhere, or an insertion that exceeds the buffer's capacity
-- forces the pending insertion.
-- 'length' and 'metrics' include pending input without forcing it.
-- Evaluating a rope to weak head normal form may leave this insertion deferred.
insert :: Unit -> Int -> Text -> Rope -> Rope
insert = M.insert

-- | /O(log n)/. Remove the half-open range @[i, j)@, clamping and rounding
-- both offsets in the original rope. Does nothing when @j <= i@.
-- Deleting a suffix of buffered 'Chars' input can take /O(1)/; see 'insert'.
delete :: Unit -> Int -> Int -> Rope -> Rope
delete = M.delete

-- | /O(log n + inserted bytes)/. Replace the half-open range @[i, j)@ with
-- text, clamping and rounding both offsets in the original rope. When
-- @j <= i@, insert at @i@ instead.
--
-- An edit that stays within one chunk and keeps it within its size bounds
-- copies only that chunk and the path to it.
replace :: Unit -> Int -> Int -> Text -> Rope -> Rope
replace = M.replace

-- | /O(log n + length of the line)/. The content of a line by 0-based index,
-- without its terminating @\\n@ or @\\r\\n@; empty if there is no such line.
-- A line within a single chunk is returned as a zero-copy view.
getLine :: Int -> Rope -> Text
getLine = M.getLine

-- | /O(n)/. Lines without their @\\n@ or @\\r\\n@ terminators, produced
-- lazily. Returns @[]@ for an empty rope and omits the empty line after a
-- trailing @\\n@. A lone @\\r@ is preserved. Lines within one chunk share
-- its buffer.
lines :: Rope -> [Text]
lines = M.lines

-- | /O(log n)/. Measure the prefix ending at an offset to express that
-- location in all four units. The offset is clamped and rounded as in 'splitAt'.
--
-- >>> metricsAt Chars 3 "a😀\nb"
-- Metrics {bytes = 6, chars = 3, utf16Units = 4, newlines = 1}
metricsAt :: Unit -> Int -> Rope -> Metrics
metricsAt = M.metricsAt

-- | /O(log n)/. @convert from to@ re-expresses an offset in another unit.
-- Converting to 'Lines' gives the index of the line containing the offset,
-- converting from 'Lines' the offset of the start of a line.
--
-- >>> convert Bytes Utf16 5 "a😀\nb"
-- 3
convert :: Unit -> Unit -> Int -> Rope -> Int
convert = M.convert

-- | /O(log n)/. Split at a zero-based line and column, with the column in
-- the given unit. Negative coordinates clamp to zero. A column beyond the
-- line's content clamps to before its @\\n@ or @\\r\\n@; a line beyond the
-- document clamps to its end. Offsets inside code points round down.
-- For 'Lines' columns, zero means the line start and any positive value
-- means the end of its content.
splitAtPosition :: Unit -> Position -> Rope -> (Rope, Rope)
splitAtPosition = M.splitAtPosition

-- | /O(log n)/. The location of a position in every unit, clamped like
-- 'splitAtPosition'.
metricsAtPosition :: Unit -> Position -> Rope -> Metrics
metricsAtPosition = M.metricsAtPosition

-- | /O(log n)/. Return prefix metrics for the line start and the position,
-- sharing their lookup. Clamps coordinates as in 'metricsAtPosition'.
-- Subtract corresponding counts to get the reached column in any unit.
-- Comparing it with the requested column detects clamping or rounding:
--
-- >>> let (line, at) = metricsAtLineAndPosition Utf16 (Position 1 3) "a😀\nb😀c"
-- >>> (utf16Units at - utf16Units line, chars at - chars line, bytes at)
-- (3,2,11)
metricsAtLineAndPosition :: Unit -> Position -> Rope -> (Metrics, Metrics)
metricsAtLineAndPosition = M.metricsAtLineAndPosition
{-# INLINE metricsAtLineAndPosition #-}

-- | /O(log n)/. The position, with its column in the given unit, of a
-- location obtained from 'metricsAt', 'metricsAtPosition', or 'metricsWhere'
-- on the same rope. Does not clamp or validate the supplied metrics.
metricsToPosition :: Unit -> Metrics -> Rope -> Position
metricsToPosition = M.metricsToPosition

-- | /O(log n)/. @offsetToPosition from to@ turns an offset in unit @from@
-- into a position with its column in unit @to@.
-- An offset inside a line terminator remains there; converting the result
-- back with 'positionToOffset' clamps it to the end of the line's content.
--
-- >>> offsetToPosition Bytes Utf16 11 "a😀\nb😀c"
-- Position {posLine = 1, posColumn = 3}
offsetToPosition :: Unit -> Unit -> Int -> Rope -> Position
offsetToPosition = M.offsetToPosition

-- | /O(log n)/. @positionToOffset from to@ turns a position with its column
-- in unit @from@ into an offset in unit @to@.
-- Coordinates are clamped as in 'splitAtPosition'.
--
-- >>> positionToOffset Utf16 Bytes (Position 1 3) "a😀\nb😀c"
-- 11
positionToOffset :: Unit -> Unit -> Position -> Rope -> Int
positionToOffset = M.positionToOffset

-- | /O(log n)/ for a constant-time predicate. Split after the longest
-- code-point-aligned prefix for which the predicate is false. The predicate
-- must be monotone: once true, it must stay true as the prefix grows.
-- If true for the empty prefix, split at the start; if never true, split
-- at the end.
splitWhere :: (Metrics -> Bool) -> Rope -> (Rope, Rope)
splitWhere p = M.splitWhere (\m _ -> p m)

-- | /O(log n)/ for a constant-time predicate. Prefix metrics at the split
-- point chosen by 'splitWhere', without constructing either half.
metricsWhere :: (Metrics -> Bool) -> Rope -> Metrics
metricsWhere p = M.metricsWhere (\m _ -> p m)

-- | /O(n)/. Annotate the text with a custom measure for use with
-- "Data.Text.NanoRope.Measured". The text itself is shared, not copied.
measured :: Measure a => Rope -> M.Rope a
measured = M.remeasure

-- | /O(n)/. Forget a custom measure. The text itself is shared, not copied.
unmeasured :: M.Rope a -> Rope
unmeasured = M.remeasure
