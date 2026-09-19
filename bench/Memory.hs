-- Full laziness would float a document out of the action that makes it and
-- keep it for the next one, which would then find it already paid for.
{-# OPTIONS_GHC -fno-full-laziness #-}

-- | How much of the heap a structure holds on to. Needs @+RTS -T@.
module Memory
  ( fresh
  , footprint
  ) where

import Control.Exception (evaluate)
import Foreign.StablePtr (freeStablePtr, newStablePtr)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import System.Mem (performMajorGC)

-- | A value made anew, which nothing else holds on to.
fresh :: (n -> a) -> n -> IO a
fresh make n = evaluate (make n)
{-# NOINLINE fresh #-}

-- | Live bytes after a major collection.
liveBytes :: IO Double
liveBytes = do
  performMajorGC
  fromIntegral . gcdetails_live_bytes . gc <$> getRTSStats

-- | How many more bytes are live while a structure built from a freshly made
-- input is, and the input is not. A structure that shares the input's
-- buffers keeps them alive and pays for them here.
footprint :: (n -> i) -> n -> (i -> a) -> (a -> ()) -> IO Double
footprint make n build deep = do
  before <- liveBytes
  input <- fresh make n
  let a = build input
  _ <- evaluate (deep a)
  keep <- newStablePtr a
  after <- liveBytes
  freeStablePtr keep
  pure (after - before)
{-# NOINLINE footprint #-}
