-- |
-- Module      : Data.Text.NanoRope
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- A text rope for editors, language servers and parsers.
--
-- * __Flat chunks.__ Leaves are unpinned byte arrays of at most 512 bytes of
--   UTF-8, referenced straight from the tree.
-- * __A B-tree.__ Up to 16 children per node. Every node carries the sizes of
--   its subtree, so that seeking reads the heads of a node's children and an
--   edit copies one small array of pointers per level.
-- * __Cheap keystrokes.__ An edit within a chunk is one descent and one copy
--   of that chunk, and typing on from where the last insertion ended does
--   not even touch the tree, see 'insert'.
-- * __Every unit at once.__ Bytes, code points, UTF-16 code units and lines
--   are tracked at every node. Any of them addresses the rope in /O(log n)/,
--   and any converts to any other in /O(log n)/, see 'Unit' and 'convert'.
-- * __Long lines are not special.__ Chunks are cut by size, never by line.
-- * __Custom measures.__ "Data.Text.NanoRope.Measured" caches a monoid of
--   your choice at every node and searches by it.
--
-- This module is meant to be imported qualified:
--
-- > import Data.Text.NanoRope (Rope, Unit (..), Position (..))
-- > import qualified Data.Text.NanoRope as Rope
--
-- = Example: a language server
--
-- A client sends a range of UTF-16 positions to replace. The parser
-- downstream wants to hear about it in bytes.
--
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
import Prelude hiding (drop, getLine, length, lines, null, splitAt, take)

-- | A rope of text. This is "Data.Text.NanoRope.Measured"'s rope without a
-- custom measure, so the two interfaces mix freely and all instances ('Eq',
-- 'Ord', 'Show', 'Semigroup', 'Monoid', 'Data.String.IsString',
-- 'Control.DeepSeq.NFData') are shared.
type Rope = M.Rope ()

-- $conversions
-- The 'Metrics' of the prefix of a rope up to some location describe that
-- location in every unit at once. All conversions go through this hub:
-- 'metricsAt', 'metricsAtPosition' and 'metricsWhere' lead into it, 'count'
-- and 'metricsToPosition' lead out of it. When you need more than one unit of
-- the same location (tree-sitter wants a byte offset /and/ a row and byte
-- column), take them from the same 'Metrics' and pay for one descent.

-- | The empty rope.
empty :: Rope
empty = M.empty

-- | A rope of one character.
singleton :: Char -> Rope
singleton = M.singleton

-- | /O(n)/. The text is copied into chunks, except that a text of at most a
-- chunk which owns its whole buffer is shared.
fromText :: Text -> Rope
fromText = M.fromText

-- | /O(n)/.
fromLazyText :: TL.Text -> Rope
fromLazyText = M.fromLazyText

-- | /O(n)/. A rope of a single chunk is converted without copying.
toText :: Rope -> Text
toText = M.toText

-- | /O(n)/, without copying any text: the lazy text shares the chunks.
toLazyText :: Rope -> TL.Text
toLazyText = M.toLazyText

-- | /O(n)/.
toString :: Rope -> String
toString = M.toString

-- | The chunks of the rope as zero-copy views, in order. They are non-empty,
-- at most 512 bytes long and produced lazily.
toChunks :: Rope -> [Text]
toChunks = M.toChunks

-- | Lazy right fold over the chunks of 'toChunks'.
foldrChunks :: (Text -> b -> b) -> b -> Rope -> b
foldrChunks = M.foldrChunks

-- | Strict left fold over the chunks of 'toChunks'. It is a walk of the tree
-- and allocates nothing of its own, where the list of 'toChunks' costs a
-- hundred bytes or so a chunk: the fold for whoever consumes a whole rope,
-- to hash it or to hand it to a parser or a socket.
foldlChunks' :: (b -> Text -> b) -> b -> Rope -> b
foldlChunks' = M.foldlChunks'

-- | /O(log n)/. Zero-copy view of the rest of the chunk containing the given
-- offset; empty exactly when the offset is at or beyond the end.
--
-- This is the shape of a parser's read callback (such as tree-sitter's
-- @TSInput@): ask for the text at a byte offset, consume it, ask again at
-- the following offset.
chunkAt :: Unit -> Int -> Rope -> Text
chunkAt = M.chunkAt

-- | /O(1)/.
null :: Rope -> Bool
null = M.null

-- | /O(1)/. Length in any unit; for 'Lines' this is the number of @\\n@.
--
-- >>> map (`length` "a😀\nb") [Bytes, Chars, Utf16, Lines]
-- [7,4,5,1]
length :: Unit -> Rope -> Int
length = M.length

-- | /O(1)/. Number of lines: one more than the number of @\\n@, so that the
-- valid line indices are @[0 .. lineCount - 1]@. The last line may be empty.
lineCount :: Rope -> Int
lineCount = M.lineCount

-- | /O(1)/. All measurements of the rope.
metrics :: Rope -> Metrics
metrics = M.metrics

-- | /O(log n)/, more precisely proportional to the difference in height.
-- Same as '<>'.
append :: Rope -> Rope -> Rope
append = M.append

-- | /O(log n)/. Split at an offset, clamped to the rope and rounded down to
-- a code point boundary (see 'Unit'). Both halves come out of one descent;
-- 'take' and 'drop' are cheaper if you are after just one of them.
--
-- >>> splitAt Lines 1 "fst\nsnd\n"
-- ("fst\n","snd\n")
splitAt :: Unit -> Int -> Rope -> (Rope, Rope)
splitAt = M.splitAt

-- | /O(log n)/. The prefix up to an offset.
take :: Unit -> Int -> Rope -> Rope
take = M.take

-- | /O(log n)/. The suffix from an offset.
drop :: Unit -> Int -> Rope -> Rope
drop = M.drop

-- | /O(log n)/. @slice u i j@ is the text from offset @i@ up to offset @j@.
slice :: Unit -> Int -> Int -> Rope -> Rope
slice = M.slice

-- | /O(log n + length of the result)/. Like 'slice', but straight to 'Text'
-- without building a rope in between. A range within a single chunk is
-- returned as a zero-copy view of that chunk.
sliceText :: Unit -> Int -> Int -> Rope -> Text
sliceText = M.sliceText

-- | /O(log n + length of the text)/. Insert text at an offset.
--
-- An insertion confined to one chunk, as nearly all are, copies that chunk
-- and the path to it and nothing else; a chunk that overflows splits in two.
--
-- Typing is cheaper still. An insertion that starts where the one before it
-- ended (in the same unit, which is not 'Lines') is held back: up to 128
-- bytes of such keystrokes wait next to the tree and go into it at once, when
-- the rope is read or edited elsewhere. A keystroke then costs a copy of what
-- is waiting, /O(1)/, and 'length' and 'metrics' answer without looking at
-- the tree. This is the one lazy spot of a rope: evaluating it to weak head
-- normal form leaves up to one such insertion undone.
insert :: Unit -> Int -> Text -> Rope -> Rope
insert = M.insert

-- | /O(log n)/. @delete u i j@ removes the text from offset @i@ up to
-- offset @j@. Erasing the end of what was just typed (see 'insert') by code
-- points is /O(1)/ as well.
delete :: Unit -> Int -> Int -> Rope -> Rope
delete = M.delete

-- | /O(log n + length of the text)/. @replace u i j t@ replaces the text
-- from offset @i@ up to offset @j@ by @t@.
--
-- An edit confined to one chunk that neither overflows nor underflows, as
-- nearly all keystrokes are, copies that chunk and the path to it and
-- nothing else.
replace :: Unit -> Int -> Int -> Text -> Rope -> Rope
replace = M.replace

-- | /O(log n + length of the line)/. The content of a line by 0-based index,
-- without its terminating @\\n@ or @\\r\\n@; empty if there is no such line.
-- A line within a single chunk is returned as a zero-copy view.
getLine :: Int -> Rope -> Text
getLine = M.getLine

-- | The lines of the rope without their terminators, lazily. Like
-- 'Data.Text.lines', a trailing @\\n@ does not start another line; unlike
-- it, @\\r\\n@ is stripped too.
lines :: Rope -> [Text]
lines = M.lines

-- | /O(log n)/. The 'Metrics' of the prefix ending at an offset, in other
-- words the same location expressed in every unit at once. The offset is
-- clamped and rounded as described at 'Unit'.
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

-- | /O(log n)/. Split at a line and column, the column counted in the given
-- unit. A column beyond the end of the line is clamped to the end of its
-- content (before the @\\n@ or @\\r\\n@) and a line beyond the last one to the
-- end of the rope, as the Language Server Protocol asks for.
splitAtPosition :: Unit -> Position -> Rope -> (Rope, Rope)
splitAtPosition = M.splitAtPosition

-- | /O(log n)/. The location of a position in every unit, clamped like
-- 'splitAtPosition'.
metricsAtPosition :: Unit -> Position -> Rope -> Metrics
metricsAtPosition = M.metricsAtPosition

-- | /O(log n)/. The position, with its column in the given unit, of a
-- location obtained from 'metricsAt', 'metricsAtPosition' or 'metricsWhere'.
metricsToPosition :: Unit -> Metrics -> Rope -> Position
metricsToPosition = M.metricsToPosition

-- | /O(log n)/. @offsetToPosition from to@ turns an offset in unit @from@
-- into a position with its column in unit @to@.
--
-- >>> offsetToPosition Bytes Utf16 11 "a😀\nb😀c"
-- Position {posLine = 1, posColumn = 3}
offsetToPosition :: Unit -> Unit -> Int -> Rope -> Position
offsetToPosition = M.offsetToPosition

-- | /O(log n)/. @positionToOffset from to@ turns a position with its column
-- in unit @from@ into an offset in unit @to@.
--
-- >>> positionToOffset Utf16 Bytes (Position 1 3) "a😀\nb😀c"
-- 11
positionToOffset :: Unit -> Unit -> Position -> Rope -> Int
positionToOffset = M.positionToOffset

-- | /O(log n)/. Split where a predicate on the metrics of the prefix turns
-- true: the first half is the longest prefix (of whole code points) for which
-- the predicate is false. The predicate has to be monotone, that is stay true
-- once it is true.
splitWhere :: (Metrics -> Bool) -> Rope -> (Rope, Rope)
splitWhere p = M.splitWhere (\m _ -> p m)

-- | /O(log n)/. The location where 'splitWhere' splits.
metricsWhere :: (Metrics -> Bool) -> Rope -> Metrics
metricsWhere p = M.metricsWhere (\m _ -> p m)

-- | /O(n)/. Annotate the text with a custom measure for use with
-- "Data.Text.NanoRope.Measured". The text itself is shared, not copied.
measured :: Measure a => Rope -> M.Rope a
measured = M.remeasure

-- | /O(n)/. Forget a custom measure. The text itself is shared, not copied.
unmeasured :: M.Rope a -> Rope
unmeasured = M.remeasure
