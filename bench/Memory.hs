-- Prevent full laziness from sharing an input across measurements, which
-- would undercount the memory retained by later runs.
{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Measure retained heap with RTS statistics. Requires @+RTS -T@.
module Memory
  ( fresh
  , footprint
  ) where

import Control.Exception (evaluate)
import Foreign.StablePtr (freeStablePtr, newStablePtr)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import System.Mem (performMajorGC)

-- | Construct and evaluate a fresh input for one measurement.
fresh :: (n -> a) -> n -> IO a
fresh make n = evaluate (make n)
{-# NOINLINE fresh #-}

-- | Live bytes after a major collection.
liveBytes :: IO Double
liveBytes = do
  performMajorGC
  fromIntegral . gcdetails_live_bytes . gc <$> getRTSStats

-- | Measure additional live bytes while retaining the built value but no
-- separate reference to its input. Input buffers shared by the result
-- remain live and are included in the measurement.
footprint :: (n -> i) -> n -> (i -> a) -> (a -> ()) -> IO Double
footprint make n build deep = do
  before <- liveBytes
  input <- fresh make n
  let a = build input
  _ <- evaluate (deep a)
  -- Keep the builder alive across both measurements so collecting its
  -- captured inputs cannot reduce the apparent size of the result.
  keep <- newStablePtr (a, build)
  after <- liveBytes
  freeStablePtr keep
  pure (after - before)
{-# NOINLINE footprint #-}
