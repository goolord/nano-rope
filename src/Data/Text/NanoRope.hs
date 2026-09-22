-- |
-- Module      : Data.Text.NanoRope
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- A persistent UTF-8 text rope for editors, language servers, and parsers.
--
-- * /O(log n)/ indexing and conversion in bytes, code points, UTF-16 and lines.
-- * Small edits: a B-tree of chunks of at most 512 bytes, whatever the line
--   lengths, shares unchanged subtrees; keystrokes are buffered ('insert').
-- * Chunk views and streamed UTF-8 output without flattening the document.
-- * Custom cached measures in "Data.Text.NanoRope.Measured".
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
-- Prefix t'Metrics' (from 'metricsAt', 'metricsAtPosition' or 'metricsWhere')
-- locate a point in all four units at once; read any of them with 'count'.

-- | See 'Data.Text.NanoRope.Measured.empty'.
empty :: Rope
empty = M.empty

-- | See 'Data.Text.NanoRope.Measured.singleton'.
singleton :: Char -> Rope
singleton = M.singleton

-- | See 'Data.Text.NanoRope.Measured.fromText'.
fromText :: Text -> Rope
fromText = M.fromText

-- | See 'Data.Text.NanoRope.Measured.fromLazyText'.
fromLazyText :: TL.Text -> Rope
fromLazyText = M.fromLazyText

-- | See 'Data.Text.NanoRope.Measured.toText'.
toText :: Rope -> Text
toText = M.toText

-- | See 'Data.Text.NanoRope.Measured.toLazyText'.
toLazyText :: Rope -> TL.Text
toLazyText = M.toLazyText

-- | See 'Data.Text.NanoRope.Measured.toString'.
toString :: Rope -> String
toString = M.toString

-- | See 'Data.Text.NanoRope.Measured.toChunks'.
toChunks :: Rope -> [Text]
toChunks = M.toChunks

-- | See 'Data.Text.NanoRope.Measured.foldrChunks'.
foldrChunks :: (Text -> b -> b) -> b -> Rope -> b
foldrChunks = M.foldrChunks

-- | See 'Data.Text.NanoRope.Measured.foldlChunks''.
foldlChunks' :: (b -> Text -> b) -> b -> Rope -> b
foldlChunks' = M.foldlChunks'

-- | See 'Data.Text.NanoRope.Measured.chunkAt'.
chunkAt :: Unit -> Int -> Rope -> Text
chunkAt = M.chunkAt

-- | See 'Data.Text.NanoRope.Measured.hPutUtf8'.
hPutUtf8 :: Handle -> Rope -> IO ()
hPutUtf8 = M.hPutUtf8

-- | See 'Data.Text.NanoRope.Measured.writeFileUtf8'.
writeFileUtf8 :: FilePath -> Rope -> IO ()
writeFileUtf8 = M.writeFileUtf8

-- | See 'Data.Text.NanoRope.Measured.null'.
null :: Rope -> Bool
null = M.null

-- | See 'Data.Text.NanoRope.Measured.length'.
length :: Unit -> Rope -> Int
length = M.length

-- | See 'Data.Text.NanoRope.Measured.lineCount'.
lineCount :: Rope -> Int
lineCount = M.lineCount

-- | See 'Data.Text.NanoRope.Measured.metrics'.
metrics :: Rope -> Metrics
metrics = M.metrics

-- | See 'Data.Text.NanoRope.Measured.append'.
append :: Rope -> Rope -> Rope
append = M.append

-- | See 'Data.Text.NanoRope.Measured.splitAt'.
splitAt :: Unit -> Int -> Rope -> (Rope, Rope)
splitAt = M.splitAt

-- | See 'Data.Text.NanoRope.Measured.take'.
take :: Unit -> Int -> Rope -> Rope
take = M.take

-- | See 'Data.Text.NanoRope.Measured.drop'.
drop :: Unit -> Int -> Rope -> Rope
drop = M.drop

-- | See 'Data.Text.NanoRope.Measured.slice'.
slice :: Unit -> Int -> Int -> Rope -> Rope
slice = M.slice

-- | See 'Data.Text.NanoRope.Measured.sliceText'.
sliceText :: Unit -> Int -> Int -> Rope -> Text
sliceText = M.sliceText

-- | See 'Data.Text.NanoRope.Measured.insert'.
insert :: Unit -> Int -> Text -> Rope -> Rope
insert = M.insert

-- | See 'Data.Text.NanoRope.Measured.delete'.
delete :: Unit -> Int -> Int -> Rope -> Rope
delete = M.delete

-- | See 'Data.Text.NanoRope.Measured.replace'.
replace :: Unit -> Int -> Int -> Text -> Rope -> Rope
replace = M.replace

-- | See 'Data.Text.NanoRope.Measured.getLine'.
getLine :: Int -> Rope -> Text
getLine = M.getLine

-- | See 'Data.Text.NanoRope.Measured.lines'.
lines :: Rope -> [Text]
lines = M.lines

-- | See 'Data.Text.NanoRope.Measured.metricsAt'.
metricsAt :: Unit -> Int -> Rope -> Metrics
metricsAt = M.metricsAt

-- | See 'Data.Text.NanoRope.Measured.convert'.
convert :: Unit -> Unit -> Int -> Rope -> Int
convert = M.convert

-- | See 'Data.Text.NanoRope.Measured.splitAtPosition'.
splitAtPosition :: Unit -> Position -> Rope -> (Rope, Rope)
splitAtPosition = M.splitAtPosition

-- | See 'Data.Text.NanoRope.Measured.metricsAtPosition'.
metricsAtPosition :: Unit -> Position -> Rope -> Metrics
metricsAtPosition = M.metricsAtPosition

-- | See 'Data.Text.NanoRope.Measured.metricsAtLineAndPosition'.
metricsAtLineAndPosition :: Unit -> Position -> Rope -> (Metrics, Metrics)
metricsAtLineAndPosition = M.metricsAtLineAndPosition
{-# INLINE metricsAtLineAndPosition #-}

-- | See 'Data.Text.NanoRope.Measured.metricsToPosition'.
metricsToPosition :: Unit -> Metrics -> Rope -> Position
metricsToPosition = M.metricsToPosition

-- | See 'Data.Text.NanoRope.Measured.offsetToPosition'.
offsetToPosition :: Unit -> Unit -> Int -> Rope -> Position
offsetToPosition = M.offsetToPosition

-- | See 'Data.Text.NanoRope.Measured.positionToOffset'.
positionToOffset :: Unit -> Unit -> Position -> Rope -> Int
positionToOffset = M.positionToOffset

-- | 'Data.Text.NanoRope.Measured.splitWhere' on the built-in metrics.
splitWhere :: (Metrics -> Bool) -> Rope -> (Rope, Rope)
splitWhere p = M.splitWhere (\m _ -> p m)

-- | 'Data.Text.NanoRope.Measured.metricsWhere' on the built-in metrics.
metricsWhere :: (Metrics -> Bool) -> Rope -> Metrics
metricsWhere p = M.metricsWhere (\m _ -> p m)

-- | /O(n)/. Add a custom measure, sharing the text.
measured :: Measure a => Rope -> M.Rope a
measured = M.remeasure

-- | /O(n)/. Forget a custom measure, sharing the text.
unmeasured :: M.Rope a -> Rope
unmeasured = M.remeasure
