-- |
-- Module      : Data.Text.NanoRope.Measured
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- Ropes annotated with a custom monoidal 'Measure', cached at every node of
-- the tree next to the built-in 'Metrics'. If bytes, code points, UTF-16
-- code units and lines are all you need, "Data.Text.NanoRope" offers the same
-- interface without the type parameter.
--
-- This module is meant to be imported qualified:
--
-- > import Data.Text.NanoRope.Measured (Rope, Measure (..), Unit (..))
-- > import qualified Data.Text.NanoRope.Measured as Rope
--
-- = Example: display width
--
-- > newtype Width = Width Int deriving (Eq, Ord, Show)
-- >
-- > instance Semigroup Width where Width a <> Width b = Width (a + b)
-- > instance Monoid Width where mempty = Width 0
-- >
-- > instance Measure Width where
-- >   measureChunk = Width . T.foldl' (\n c -> n + charWidth c) 0
-- >
-- > -- O(1): the width of everything
-- > Rope.measure rope :: Width
-- >
-- > -- O(log n): what fits into 80 columns
-- > fst (Rope.splitWhere (\_ w -> w > Width 80) rope)
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
-- The 'Metrics' of the prefix of a rope up to some location describe that
-- location in every unit at once. All conversions go through this hub:
-- 'metricsAt', 'metricsAtPosition' and 'metricsWhere' lead into it, 'count'
-- and 'metricsToPosition' lead out of it. When you need more than one unit of
-- the same location (tree-sitter wants a byte offset /and/ a row and byte
-- column), take them from the same 'Metrics' and pay for one descent.
