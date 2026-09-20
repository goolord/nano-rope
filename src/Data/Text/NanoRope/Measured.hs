-- |
-- Module      : Data.Text.NanoRope.Measured
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- A persistent text rope with a custom monoidal 'Measure' cached alongside
-- the built-in t'Metrics'. Use "Data.Text.NanoRope" when the built-in byte,
-- code point, UTF-16, and newline counts are enough.
--
-- Import this module qualified:
--
-- > import Data.Text.NanoRope.Measured (Rope, Measure (..), Unit (..))
-- > import qualified Data.Text.NanoRope.Measured as Rope
--
-- Offsets are zero-based, clamped to the document, and rounded down to code
-- point boundaries. Ranges are half-open. Only @\\n@ starts a new line;
-- 'Chars' counts code points, not grapheme clusters or display columns.
--
-- Complexity bounds use /n/ for the document's byte length. They assume
-- constant-time measure combination, linear-time chunk measurement, and an
-- evaluated tree. Reads, including 'measure', may first apply buffered
-- input; 'null', 'length', 'lineCount', and 'metrics' do not force it.
--
-- = Example: counting tabs
--
-- A tab count is independent of chunk boundaries and grows monotonically,
-- so it supports prefix searches. Enable @OverloadedStrings@ for this example.
--
-- > import qualified Data.Text as T
-- >
-- > newtype Tabs = Tabs Int deriving (Eq, Ord, Show)
-- >
-- > instance Semigroup Tabs where Tabs a <> Tabs b = Tabs (a + b)
-- > instance Monoid Tabs where mempty = Tabs 0
-- >
-- > instance Measure Tabs where
-- >   measureChunk = Tabs . T.count "\t"
-- >
-- > document :: Rope Tabs
-- > document = Rope.fromText "a\tb\tc\td"
-- >
-- > tabCount = Rope.measure document  -- Tabs 3
-- > beforeThirdTab = Rope.toText (fst (Rope.splitWhere (\_ n -> n >= Tabs 3) document))
-- > -- "a\tb\tc"
--
-- See 'Measure' for the laws every annotation must satisfy.
module Data.Text.NanoRope.Measured
  ( -- * Ropes
    Rope
  , Measure (..)

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

    -- * Searching by measure
  , splitWhere
  , metricsWhere
  , remeasure
  ) where

import Data.Text.NanoRope.Internal
import Prelude ()

-- $conversions
-- Prefix t'Metrics' describe a location in all four units. Obtain them with
-- 'metricsAt', 'metricsAtPosition', or 'metricsWhere', then use 'count' for
-- absolute offsets. Reusing the metrics avoids repeating the lookup for
-- each unit. 'metricsToPosition' also looks up the line start to calculate
-- a column; use metrics from the same rope.
