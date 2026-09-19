{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks of the workloads the rope is meant for. Build with
-- @-f compare-text-rope@ to run the same workloads on @text-rope@.
module Main (main) where

import Control.DeepSeq (NFData (..))
import Data.Bits (shiftR)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.NanoRope (Position (..), Unit (..))
import qualified Data.Text.NanoRope as Nano
import Data.Word (Word64)
import Test.Tasty.Bench

#ifdef COMPARE_TEXT_ROPE
import qualified Data.Text.Rope as TR
import qualified Data.Text.Utf16.Rope as TR16
#endif

------------------------------------------------------------------------------
-- Data

-- | Deterministic pseudo-random numbers.
rands :: Word64 -> [Int]
rands = map (\x -> fromIntegral (x `shiftR` 33)) . drop 1 . iterate step
  where
    step x = x * 6364136223846793005 + 1442695040888963407

-- | Something like source code: short lines, mostly ASCII, the odd accent,
-- CJK and emoji.
sourceText :: Int -> Text
sourceText n = T.concat (zipWith line [0 :: Int ..] (L.take n (rands 1)))
  where
    line i r =
      T.concat
        [ T.replicate (r `mod` 5) "  "
        , "let value"
        , T.pack (show i)
        , " = compute (arg"
        , T.pack (show (r `mod` 1000))
        , ")"
        , case r `mod` 16 of
            0 -> " -- caf\233 \20013\25991 \128512"
            1 -> " -- na\239ve r\233sum\233"
            _ -> ""
        , "\n"
        ]

-- | The same text as one enormous line, like minified output.
minified :: Text -> Text
minified = T.filter (/= '\n')

data Env = Env
  { envText :: !Text
  , envOneLine :: !Text
  , envNano :: !Nano.Rope
  , envNanoOneLine :: !Nano.Rope
  , envNanoEdited :: !Nano.Rope
  -- ^ After 10k random inserts: the shape a rope has in the middle of a session.
  , envChars :: ![Int]
  -- ^ Random character offsets.
  , envBursts :: ![(Int, Int)]
  -- ^ A hundred of them, each with the line it is on.
  , envPositions :: ![Position]
  -- ^ Random positions with their column in UTF-16 code units.
  , envByteOffsets :: ![Int]
#ifdef COMPARE_TEXT_ROPE
  , envTR :: !TR.Rope
  , envTROneLine :: !TR.Rope
  , envTR16 :: !TR16.Rope
  , envTREdited :: !TR.Rope
#endif
  }

-- | The ropes are strict all the way down, and the lists are forced here.
instance NFData Env where
  rnf e = rnf (envChars e) `seq` rnf (envBursts e) `seq` rnf (envPositions e) `seq` rnf (envByteOffsets e)

mkEnv :: Int -> Int -> Env
mkEnv nLines nOps =
  Env
    { envText = text
    , envOneLine = oneLine
    , envNano = nano
    , envNanoOneLine = Nano.fromText oneLine
    , envNanoEdited = L.foldl' (\rope i -> Nano.insert Chars i "x" rope) nano offsets
    , envChars = offsets
    , envBursts = [(i, Nano.convert Chars Lines i nano) | i <- L.take 100 offsets]
    , envPositions = [Position (r `mod` nLines) (r `mod` 40) | r <- L.take nOps (rands 3)]
    , envByteOffsets = [r `mod` (Nano.length Bytes nano + 1) | r <- L.take nOps (rands 4)]
#ifdef COMPARE_TEXT_ROPE
    , envTR = TR.fromText text
    , envTROneLine = TR.fromText oneLine
    , envTR16 = TR16.fromText text
    , envTREdited = L.foldl' (\rope i -> let (a, b) = TR.splitAt (fromIntegral i) rope in a <> "x" <> b) (TR.fromText text) offsets
#endif
    }
  where
    text = sourceText nLines
    oneLine = minified text
    nano = Nano.fromText text
    chars = T.length text
    offsets = [r `mod` (chars + 1) | r <- L.take nOps (rands 2)]

------------------------------------------------------------------------------
-- Workloads

-- | The length of a rope that has been read, so that the keystrokes typed
-- last are in the tree like everything else and not left waiting.
built :: Nano.Rope -> Int
built r = if T.null (Nano.chunkAt Bytes 0 r) then 0 else Nano.length Chars r

-- | Insert a character at each of the given offsets.
nanoInserts :: [Int] -> Nano.Rope -> Int
nanoInserts offsets r0 = built (L.foldl' (\r i -> Nano.insert Chars i "x" r) r0 offsets)

-- | Type a run of characters starting at each of the given offsets.
nanoTyping :: Int -> [Int] -> Nano.Rope -> Int
nanoTyping n offsets r0 = built (L.foldl' burst r0 offsets)
  where
    burst r i = L.foldl' (\acc k -> Nano.insert Chars (i + k) "x" acc) r [0 .. n - 1]

-- | The same, looking at the line after every keystroke like an editor that
-- redraws it.
nanoTypingRead :: [(Int, Int)] -> Nano.Rope -> Int
nanoTypingRead bursts r0 = snd (L.foldl' burst (r0, 0) bursts)
  where
    burst acc (i, l) = L.foldl' (key i l) acc [0 .. 99]
    key i l (r, n) k =
      let r' = Nano.insert Chars (i + k) "x" r
          !n' = n + T.length (Nano.getLine l r')
       in (r', n')

-- | Delete a character at each of the given offsets.
nanoDeletes :: [Int] -> Nano.Rope -> Int
nanoDeletes offsets r0 = built (L.foldl' (\r i -> Nano.delete Chars i (i + 1) r) r0 offsets)

nanoSplits :: [Int] -> Nano.Rope -> Int
nanoSplits offsets r = L.foldl' (\n i -> let (a, b) = Nano.splitAt Chars i r in n + Nano.length Lines a + Nano.length Lines b) 0 offsets

-- | What a language server does with an incoming change: find a UTF-16
-- position and edit there.
nanoLspEdits :: [Position] -> Nano.Rope -> Int
nanoLspEdits positions r0 = built (L.foldl' edit r0 positions)
  where
    edit r pos =
      let i = Nano.positionToOffset Utf16 Bytes pos r
       in Nano.insert Bytes i "x" r

-- | What a language server does with the result of a byte-based tool: turn
-- byte offsets into UTF-16 positions.
nanoByteToPosition :: [Int] -> Nano.Rope -> Int
nanoByteToPosition offsets r = L.foldl' (\n i -> n + posColumn (Nano.offsetToPosition Bytes Utf16 i r)) 0 offsets

nanoGetLines :: [Position] -> Nano.Rope -> Int
nanoGetLines positions r = L.foldl' (\n (Position l _) -> n + T.length (Nano.getLine l r)) 0 positions

#ifdef COMPARE_TEXT_ROPE
trInserts :: [Int] -> TR.Rope -> Int
trInserts offsets r0 = fromIntegral (TR.length (L.foldl' ins r0 offsets))
  where
    ins r i = let (a, b) = TR.splitAt (fromIntegral i) r in a <> "x" <> b

trTyping :: Int -> [Int] -> TR.Rope -> Int
trTyping n offsets r0 = fromIntegral (TR.length (L.foldl' burst r0 offsets))
  where
    burst r i = L.foldl' (\acc k -> let (a, b) = TR.splitAt (fromIntegral (i + k)) acc in a <> "x" <> b) r [0 .. n - 1]

trTypingRead :: [(Int, Int)] -> TR.Rope -> Int
trTypingRead bursts r0 = snd (L.foldl' burst (r0, 0) bursts)
  where
    burst acc (i, l) = L.foldl' (key i l) acc [0 .. 99 :: Int]
    key i l (r, n) k =
      let (a, b) = TR.splitAt (fromIntegral (i + k)) r
          r' = a <> "x" <> b
          !n' = n + T.length (TR.toText (TR.getLine (fromIntegral l) r'))
       in (r', n')

trDeletes :: [Int] -> TR.Rope -> Int
trDeletes offsets r0 = fromIntegral (TR.length (L.foldl' del r0 offsets))
  where
    del r i =
      let (a, b) = TR.splitAt (fromIntegral i) r
          (_, c) = TR.splitAt 1 b
       in a <> c

trSplits :: [Int] -> TR.Rope -> Int
trSplits offsets r = L.foldl' (\n i -> let (a, b) = TR.splitAt (fromIntegral i) r in n + lineBreaks a + lineBreaks b) 0 offsets
  where
    lineBreaks = fromIntegral . TR.posLine . TR.lengthAsPosition

trLspEdits :: [Position] -> TR16.Rope -> Int
trLspEdits positions r0 = fromIntegral (TR16.length (L.foldl' edit r0 positions))
  where
    edit r (Position l c) =
      case TR16.splitAtPosition (TR16.Position (fromIntegral l) (fromIntegral c)) r of
        Just (a, b) -> a <> "x" <> b
        Nothing -> r

trGetLines :: [Position] -> TR.Rope -> Int
trGetLines positions r = L.foldl' (\n (Position l _) -> n + T.length (TR.toText (TR.getLine (fromIntegral l) r))) 0 positions
#endif

------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain
    [ env (pure (mkEnv 100000 10000)) $ \e ->
        bgroup
          "100k lines"
          [ bgroup
              "fromText"
              [ bench "nano-rope" $ whnf Nano.fromText (envText e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf TR.fromText (envText e)
#endif
              ]
          , bgroup
              "toText"
              [ bench "nano-rope" $ whnf Nano.toText (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf TR.toText (envTR e)
#endif
              ]
          , bgroup
              "10k random inserts"
              [ bench "nano-rope" $ whnf (nanoInserts (envChars e)) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trInserts (envChars e)) (envTR e)
#endif
              ]
          , bgroup
              "100 bursts of 100 keystrokes"
              [ bench "nano-rope" $ whnf (nanoTyping 100 (L.take 100 (envChars e))) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trTyping 100 (L.take 100 (envChars e))) (envTR e)
#endif
              ]
          , bgroup
              "100 bursts of 100 keystrokes, reading the line after each"
              [ bench "nano-rope" $ whnf (nanoTypingRead (envBursts e)) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trTypingRead (envBursts e)) (envTR e)
#endif
              ]
          , bgroup
              "10k keystrokes in one spot"
              [ bench "nano-rope" $ whnf (nanoTyping 10000 (L.take 1 (envChars e))) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trTyping 10000 (L.take 1 (envChars e))) (envTR e)
#endif
              ]
          , bgroup
              "10k random deletes"
              [ bench "nano-rope" $ whnf (nanoDeletes (envChars e)) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trDeletes (envChars e)) (envTR e)
#endif
              ]
          , bgroup
              "10k random splits"
              [ bench "nano-rope" $ whnf (nanoSplits (envChars e)) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trSplits (envChars e)) (envTR e)
#endif
              ]
          , bgroup
              "10k edits at UTF-16 positions"
              [ bench "nano-rope" $ whnf (nanoLspEdits (envPositions e)) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trLspEdits (envPositions e)) (envTR16 e)
#endif
              ]
          , bgroup
              "10k byte offsets to UTF-16 positions"
              [ bench "nano-rope" $ whnf (nanoByteToPosition (envByteOffsets e)) (envNano e)
              ]
          , bgroup
              "10k getLine"
              [ bench "nano-rope" $ whnf (nanoGetLines (envPositions e)) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trGetLines (envPositions e)) (envTR e)
#endif
              ]
          , bgroup
              "one long line, 10k random inserts"
              [ bench "nano-rope" $ whnf (nanoInserts (envChars e)) (envNanoOneLine e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trInserts (envChars e)) (envTROneLine e)
#endif
              ]
            -- A freshly loaded text-rope is a single chunk, which the read-only
            -- workloads above keep hitting. These run on ropes that have been
            -- through 10k edits and are in the shape they have mid-session.
          , bgroup
              "after 10k edits, 10k random splits"
              [ bench "nano-rope" $ whnf (nanoSplits (envChars e)) (envNanoEdited e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trSplits (envChars e)) (envTREdited e)
#endif
              ]
          , bgroup
              "after 10k edits, 10k getLine"
              [ bench "nano-rope" $ whnf (nanoGetLines (envPositions e)) (envNanoEdited e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trGetLines (envPositions e)) (envTREdited e)
#endif
              ]
          , bgroup
              "after 10k edits, 10k random inserts"
              [ bench "nano-rope" $ whnf (nanoInserts (envChars e)) (envNanoEdited e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trInserts (envChars e)) (envTREdited e)
#endif
              ]
          , bgroup
              "after 10k edits, 100 bursts of 100 keystrokes"
              [ bench "nano-rope" $ whnf (nanoTyping 100 (L.take 100 (envChars e))) (envNanoEdited e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trTyping 100 (L.take 100 (envChars e))) (envTREdited e)
#endif
              ]
          , bgroup
              "after 10k edits, 10k keystrokes in one spot"
              [ bench "nano-rope" $ whnf (nanoTyping 10000 (L.take 1 (envChars e))) (envNanoEdited e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trTyping 10000 (L.take 1 (envChars e))) (envTREdited e)
#endif
              ]
          , bgroup
              "after 10k edits, toText"
              [ bench "nano-rope" $ whnf Nano.toText (envNanoEdited e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf TR.toText (envTREdited e)
#endif
              ]
          ]
    ]
