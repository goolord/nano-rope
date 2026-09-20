-- | Shared deterministic pseudo-random generator for benchmark documents
-- and workloads. Seeds keep each input reproducible.
module Rand (rands) where

import Data.Bits (shiftR)
import Data.Word (Word64)

-- | Deterministic pseudo-random numbers.
rands :: Word64 -> [Int]
rands = map (\x -> fromIntegral (x `shiftR` 33)) . drop 1 . iterate step
  where
    step x = x * 6364136223846793005 + 1442695040888963407
