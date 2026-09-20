{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks of the workloads the rope is meant for. Build with
-- @-f compare-text-rope@, @-f compare-yi-rope@ or @-f compare-core-text@ to
-- run the same workloads on those packages, as far as they have the means:
-- @yi-rope@ knows nothing of UTF-16, and @core-text@ nothing of lines either.
--
-- With @--chart FILE.svg@ the results are drawn into a chart as well, next to
-- the heap each library needs to hold the document.
module Main (main) where

import Chart (Chart (..), Footprint (..), Group (..), Row (..), Sample (..), readSamples, render, showBytes)
import Control.DeepSeq (NFData (..))
import Control.Exception (throwIO, try)
import Control.Monad (forM_, unless)
import Data.Bits (shiftR)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.NanoRope (Position (..), Unit (..))
import qualified Data.Text.NanoRope as Nano
import Data.Text.NanoRope.Internal (kernels, kernelsName)
import Data.Version (showVersion)
import Data.Word (Word64)
import GHC.Stats (getRTSStatsEnabled)
import Lsp (lspBenchmarks, mkLspEnv)
import Memory (footprint, fresh)
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode (..))
import System.IO (IOMode (..), hGetContents', hPutStr, hSetEncoding, utf8, withFile)
import System.Info (fullCompilerVersion)
import Test.Tasty.Bench
import Text.Printf (printf)

#ifdef COMPARE_TEXT_ROPE
import qualified Data.Text.Rope as TR
import qualified Data.Text.Utf16.Rope as TR16
#endif

#ifdef COMPARE_YI_ROPE
import qualified Yi.Rope as Yi
#endif

#ifdef COMPARE_CORE_TEXT
import qualified Core.Text.Rope as CT
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
#ifdef COMPARE_YI_ROPE
  , envYi :: !Yi.YiString
  , envYiOneLine :: !Yi.YiString
  , envYiEdited :: !Yi.YiString
#endif
#ifdef COMPARE_CORE_TEXT
  , envCT :: !CT.Rope
  , envCTOneLine :: !CT.Rope
  , envCTEdited :: !CT.Rope
#endif
  }

-- | The ropes of @nano-rope@ and @text-rope@ are strict all the way down.
-- The finger trees of the other two and the lists are forced here.
instance NFData Env where
  rnf e =
    rnf (envChars e)
      `seq` rnf (envBursts e)
      `seq` rnf (envPositions e)
      `seq` rnf (envByteOffsets e)
#ifdef COMPARE_YI_ROPE
      `seq` forceYi (envYi e)
      `seq` forceYi (envYiOneLine e)
      `seq` forceYi (envYiEdited e)
#endif
#ifdef COMPARE_CORE_TEXT
      `seq` rnf (envCT e)
      `seq` rnf (envCTOneLine e)
      `seq` rnf (envCTEdited e)
#endif

mkEnv :: Int -> Int -> Env
mkEnv nLines nOps =
  Env
    { envText = text
    , envOneLine = oneLine
    , envNano = nano
    , envNanoOneLine = Nano.fromText oneLine
    , envNanoEdited = nanoEdited offsets nano
    , envChars = offsets
    , envBursts = [(i, Nano.convert Chars Lines i nano) | i <- L.take 100 offsets]
    , envPositions = [Position (r `mod` nLines) (r `mod` 40) | r <- L.take nOps (rands 3)]
    , envByteOffsets = [r `mod` (Nano.length Bytes nano + 1) | r <- L.take nOps (rands 4)]
#ifdef COMPARE_TEXT_ROPE
    , envTR = TR.fromText text
    , envTROneLine = TR.fromText oneLine
    , envTR16 = TR16.fromText text
    , envTREdited = trEdited offsets (TR.fromText text)
#endif
#ifdef COMPARE_YI_ROPE
    , envYi = Yi.fromText text
    , envYiOneLine = Yi.fromText oneLine
    , envYiEdited = yiEdited offsets (Yi.fromText text)
#endif
#ifdef COMPARE_CORE_TEXT
    , envCT = ctFromText text
    , envCTOneLine = ctFromText oneLine
    , envCTEdited = ctEdited offsets (ctFromText text)
#endif
    }
  where
    text = sourceText nLines
    oneLine = minified text
    nano = Nano.fromText text
    offsets = editOffsets nOps text

-- | Where the random edits go.
editOffsets :: Int -> Text -> [Int]
editOffsets nOps text = [r `mod` (chars + 1) | r <- L.take nOps (rands 2)]
  where
    chars = T.length text

------------------------------------------------------------------------------
-- Workloads

-- | The length of a rope that has been read, so that the keystrokes typed
-- last are in the tree like everything else and not left waiting.
built :: Nano.Rope -> Int
built r = if T.null (Nano.chunkAt Bytes 0 r) then 0 else Nano.length Chars r

-- | Insert a character at each of the given offsets.
nanoEdited :: [Int] -> Nano.Rope -> Nano.Rope
nanoEdited offsets r0 = L.foldl' (\r i -> Nano.insert Chars i "x" r) r0 offsets

nanoInserts :: [Int] -> Nano.Rope -> Int
nanoInserts offsets r0 = built (nanoEdited offsets r0)

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
trEdited :: [Int] -> TR.Rope -> TR.Rope
trEdited offsets r0 = L.foldl' ins r0 offsets
  where
    ins r i = let (a, b) = TR.splitAt (fromIntegral i) r in a <> "x" <> b

trInserts :: [Int] -> TR.Rope -> Int
trInserts offsets r0 = fromIntegral (TR.length (trEdited offsets r0))

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

#ifdef COMPARE_YI_ROPE
-- | A finger tree is lazy in its spine, and @yi-rope@ in its counts of lines
-- as well: a rope that has been used has both.
forceYi :: Yi.YiString -> ()
forceYi r = Yi.countNewLines r `seq` rnf r

-- | Loaded and ready for a question about lines, like the others.
yiFromText :: Text -> Yi.YiString
yiFromText t = let r = Yi.fromText t in Yi.countNewLines r `seq` r

-- | There is no insertion as such: split and append, which is what Yi does.
yiInsert :: Int -> Yi.YiString -> Yi.YiString
yiInsert i r = let (a, b) = Yi.splitAt i r in a <> "x" <> b

yiEdited :: [Int] -> Yi.YiString -> Yi.YiString
yiEdited offsets r0 = L.foldl' (flip yiInsert) r0 offsets

yiInserts :: [Int] -> Yi.YiString -> Int
yiInserts offsets r0 = Yi.length (yiEdited offsets r0)

yiTyping :: Int -> [Int] -> Yi.YiString -> Int
yiTyping n offsets r0 = Yi.length (L.foldl' burst r0 offsets)
  where
    burst r i = L.foldl' (\acc k -> yiInsert (i + k) acc) r [0 .. n - 1]

-- | A line without its line feed, like 'Nano.getLine'.
yiGetLine :: Int -> Yi.YiString -> Text
yiGetLine l = Yi.toText . Yi.takeWhile (/= '\n') . snd . Yi.splitAtLine l

yiTypingRead :: [(Int, Int)] -> Yi.YiString -> Int
yiTypingRead bursts r0 = snd (L.foldl' burst (r0, 0) bursts)
  where
    burst acc (i, l) = L.foldl' (key i l) acc [0 .. 99 :: Int]
    key i l (r, n) k =
      let r' = yiInsert (i + k) r
          !n' = n + T.length (yiGetLine l r')
       in (r', n')

yiDeletes :: [Int] -> Yi.YiString -> Int
yiDeletes offsets r0 = Yi.length (L.foldl' del r0 offsets)
  where
    del r i = let (a, b) = Yi.splitAt i r in a <> Yi.drop 1 b

yiSplits :: [Int] -> Yi.YiString -> Int
yiSplits offsets r = L.foldl' (\n i -> let (a, b) = Yi.splitAt i r in n + Yi.countNewLines a + Yi.countNewLines b) 0 offsets

yiGetLines :: [Position] -> Yi.YiString -> Int
yiGetLines positions r = L.foldl' (\n (Position l _) -> n + T.length (yiGetLine l r)) 0 positions
#endif

#ifdef COMPARE_CORE_TEXT
-- | One piece which shares the text, in a tree that is lazy: benchmark it to
-- normal form.
ctFromText :: Text -> CT.Rope
ctFromText = CT.intoRope

ctToText :: CT.Rope -> Text
ctToText = CT.fromRope

ctEdited :: [Int] -> CT.Rope -> CT.Rope
ctEdited offsets r0 = L.foldl' (\r i -> CT.insertRope i "x" r) r0 offsets

ctInserts :: [Int] -> CT.Rope -> Int
ctInserts offsets r0 = CT.widthRope (ctEdited offsets r0)

ctTyping :: Int -> [Int] -> CT.Rope -> Int
ctTyping n offsets r0 = CT.widthRope (L.foldl' burst r0 offsets)
  where
    burst r i = L.foldl' (\acc k -> CT.insertRope (i + k) "x" acc) r [0 .. n - 1]

ctDeletes :: [Int] -> CT.Rope -> Int
ctDeletes offsets r0 = CT.widthRope (L.foldl' del r0 offsets)
  where
    del r i =
      let (a, b) = CT.splitRope i r
          (_, c) = CT.splitRope 1 b
       in a <> c

-- | There are no lines to count: the halves are forced by their width.
ctSplits :: [Int] -> CT.Rope -> Int
ctSplits offsets r = L.foldl' (\n i -> let (a, b) = CT.splitRope i r in n + CT.widthRope a + CT.widthRope b) 0 offsets
#endif

------------------------------------------------------------------------------

-- | The size of the document, and the number of operations in a workload.
documentLines, workloadOps :: Int
documentLines = 100000
workloadOps = 10000

-- | The size of the module of the language server workloads: some 300 kB.
lspLines :: Int
lspLines = 8000

benchmarks :: [Benchmark]
benchmarks =
    [ env (pure (mkEnv documentLines workloadOps)) $ \e ->
        bgroup
          "100k lines"
          [ bgroup
              "fromText"
              [ bench "nano-rope" $ whnf Nano.fromText (envText e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf TR.fromText (envText e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf yiFromText (envText e)
#endif
#ifdef COMPARE_CORE_TEXT
              , bench "core-text" $ nf ctFromText (envText e)
#endif
              ]
          , bgroup
              "toText"
              [ bench "nano-rope" $ whnf Nano.toText (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf TR.toText (envTR e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf Yi.toText (envYi e)
#endif
#ifdef COMPARE_CORE_TEXT
              , bench "core-text" $ whnf ctToText (envCT e)
#endif
              ]
          , bgroup
              "10k random inserts"
              [ bench "nano-rope" $ whnf (nanoInserts (envChars e)) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trInserts (envChars e)) (envTR e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiInserts (envChars e)) (envYi e)
#endif
#ifdef COMPARE_CORE_TEXT
              , bench "core-text" $ whnf (ctInserts (envChars e)) (envCT e)
#endif
              ]
          , bgroup
              "100 bursts of 100 keystrokes"
              [ bench "nano-rope" $ whnf (nanoTyping 100 (L.take 100 (envChars e))) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trTyping 100 (L.take 100 (envChars e))) (envTR e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiTyping 100 (L.take 100 (envChars e))) (envYi e)
#endif
#ifdef COMPARE_CORE_TEXT
              , bench "core-text" $ whnf (ctTyping 100 (L.take 100 (envChars e))) (envCT e)
#endif
              ]
          , bgroup
              "100 bursts of 100 keystrokes, reading the line after each"
              [ bench "nano-rope" $ whnf (nanoTypingRead (envBursts e)) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trTypingRead (envBursts e)) (envTR e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiTypingRead (envBursts e)) (envYi e)
#endif
              ]
          , bgroup
              "10k keystrokes in one spot"
              [ bench "nano-rope" $ whnf (nanoTyping 10000 (L.take 1 (envChars e))) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trTyping 10000 (L.take 1 (envChars e))) (envTR e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiTyping 10000 (L.take 1 (envChars e))) (envYi e)
#endif
              -- Not core-text: a freshly loaded rope is one piece, which it
              -- measures again at every keystroke next to it and every split
              -- of it. This and the splits below take 45 s a run.
              ]
          , bgroup
              "10k random deletes"
              [ bench "nano-rope" $ whnf (nanoDeletes (envChars e)) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trDeletes (envChars e)) (envTR e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiDeletes (envChars e)) (envYi e)
#endif
#ifdef COMPARE_CORE_TEXT
              , bench "core-text" $ whnf (ctDeletes (envChars e)) (envCT e)
#endif
              ]
          , bgroup
              "10k random splits"
              [ bench "nano-rope" $ whnf (nanoSplits (envChars e)) (envNano e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trSplits (envChars e)) (envTR e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiSplits (envChars e)) (envYi e)
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
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiGetLines (envPositions e)) (envYi e)
#endif
              ]
          , bgroup
              "one long line, 10k random inserts"
              [ bench "nano-rope" $ whnf (nanoInserts (envChars e)) (envNanoOneLine e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trInserts (envChars e)) (envTROneLine e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiInserts (envChars e)) (envYiOneLine e)
#endif
#ifdef COMPARE_CORE_TEXT
              , bench "core-text" $ whnf (ctInserts (envChars e)) (envCTOneLine e)
#endif
              ]
            -- A freshly loaded text-rope is a single chunk and a freshly loaded
            -- core-text a single piece, which the read-only workloads above
            -- keep hitting. These run on ropes that have been through 10k edits
            -- and are in the shape they have mid-session.
          , bgroup
              "after 10k edits, 10k random splits"
              [ bench "nano-rope" $ whnf (nanoSplits (envChars e)) (envNanoEdited e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trSplits (envChars e)) (envTREdited e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiSplits (envChars e)) (envYiEdited e)
#endif
#ifdef COMPARE_CORE_TEXT
              , bench "core-text" $ whnf (ctSplits (envChars e)) (envCTEdited e)
#endif
              ]
          , bgroup
              "after 10k edits, 10k getLine"
              [ bench "nano-rope" $ whnf (nanoGetLines (envPositions e)) (envNanoEdited e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trGetLines (envPositions e)) (envTREdited e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiGetLines (envPositions e)) (envYiEdited e)
#endif
              ]
          , bgroup
              "after 10k edits, 10k random inserts"
              [ bench "nano-rope" $ whnf (nanoInserts (envChars e)) (envNanoEdited e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trInserts (envChars e)) (envTREdited e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiInserts (envChars e)) (envYiEdited e)
#endif
#ifdef COMPARE_CORE_TEXT
              , bench "core-text" $ whnf (ctInserts (envChars e)) (envCTEdited e)
#endif
              ]
          , bgroup
              "after 10k edits, 100 bursts of 100 keystrokes"
              [ bench "nano-rope" $ whnf (nanoTyping 100 (L.take 100 (envChars e))) (envNanoEdited e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trTyping 100 (L.take 100 (envChars e))) (envTREdited e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiTyping 100 (L.take 100 (envChars e))) (envYiEdited e)
#endif
#ifdef COMPARE_CORE_TEXT
              , bench "core-text" $ whnf (ctTyping 100 (L.take 100 (envChars e))) (envCTEdited e)
#endif
              ]
          , bgroup
              "after 10k edits, 10k keystrokes in one spot"
              [ bench "nano-rope" $ whnf (nanoTyping 10000 (L.take 1 (envChars e))) (envNanoEdited e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf (trTyping 10000 (L.take 1 (envChars e))) (envTREdited e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf (yiTyping 10000 (L.take 1 (envChars e))) (envYiEdited e)
#endif
#ifdef COMPARE_CORE_TEXT
              , bench "core-text" $ whnf (ctTyping 10000 (L.take 1 (envChars e))) (envCTEdited e)
#endif
              ]
          , bgroup
              "after 10k edits, toText"
              [ bench "nano-rope" $ whnf Nano.toText (envNanoEdited e)
#ifdef COMPARE_TEXT_ROPE
              , bench "text-rope" $ whnf TR.toText (envTREdited e)
#endif
#ifdef COMPARE_YI_ROPE
              , bench "yi-rope" $ whnf Yi.toText (envYiEdited e)
#endif
#ifdef COMPARE_CORE_TEXT
              , bench "core-text" $ whnf ctToText (envCTEdited e)
#endif
              ]
          ]
    , -- A module the size of a large one of GHC's, and the rope's side of what
      -- lsp and haskell-language-server do with it: see "Lsp".
      env (pure (mkLspEnv lspLines)) $ \e ->
        bgroup "language server, 8k lines" (lspBenchmarks e)
    ]

------------------------------------------------------------------------------
-- The chart

main :: IO ()
main = do
  args <- getArgs
  case chartArgs args of
    Nothing -> defaultMain benchmarks
    Just (svg, csv, redraw, rest) -> do
      stats <- getRTSStatsEnabled
      unless stats $ putStrLn "No RTS statistics (+RTS -T): leaving memory out."
      (textHeap, fps) <- if stats then footprints else pure (Nothing, [])
      forM_ textHeap $ \t -> do
        putStrLn "Live heap holding the document"
        printf "  %-10s %s\n" ("Text" :: String) (showBytes t)
        forM_ fps $ \f ->
          printf "  %-10s %-8s %s\n" (footprintLibrary f) (footprintState f) (showBytes (footprintBytes f))
      unless redraw $ do
        done <- try (withArgs rest (defaultMain benchmarks))
        case done of
          Left ExitSuccess -> pure ()
          Left failure -> throwIO failure
          Right () -> pure ()
      -- The workloads of a language server are nano-rope's alone and are not
      -- drawn: the chart is of what the libraries can be compared by.
      samples <- filter ((`elem` libraries) . sampleLibrary) . readSamples <$> withFile csv ReadMode hGetContents'
      bytes <- Nano.length Bytes . Nano.fromText <$> fresh sourceText documentLines
      let ran = map sampleLibrary samples ++ map footprintLibrary fps
          others = filter (`elem` ran) (drop 1 libraries)
      withFile svg WriteMode $ \h -> do
        hSetEncoding h utf8
        hPutStr h . render $
          Chart
            { chartTitle = "nano-rope" ++ (if null others then "" else " against " ++ andList others)
            , chartSubtitle =
                printf
                  "A run is %s operations on %s lines of source code (%.1f MB), or one load or save of it. GHC %s, chunks scanned with %s."
                  (commas workloadOps)
                  (commas documentLines)
                  (fromIntegral bytes / 1e6 :: Double)
                  (showVersion fullCompilerVersion)
                  (kernelsName (last kernels))
            , chartNotes =
                [ "Fresh: a freshly loaded rope. Edited: the same rope after " ++ commas workloadOps
                    ++ " random inserts, in the shape it has mid-session. Rows with one run are on a fresh rope."
                , "A freshly loaded text-rope or core-text is a single chunk, and core-text is the Text it was loaded from, so loading and saving them is far faster than nano-rope; their first reads and splits walk the whole text."
                , "No mark: the library cannot do this (yi-rope has no UTF-16, core-text neither UTF-16 nor lines). A hollow mark at the right: left out, a run takes minutes."
                , "Live heap: what stays reachable after a major collection once the Text the document came from is dropped. A rope that shares that Text keeps it alive."
                ]
            , chartLibraries = libraries
            , chartGroups = chartRows
            , chartSamples = samples
            , chartSkipped = skipped
            , chartTextHeap = textHeap
            , chartFootprints = fps
            }
      putStrLn ("Chart: " ++ svg)
  where
    libraries = ["nano-rope", "text-rope", "yi-rope", "core-text"]
    andList [x] = x
    andList [x, y] = x ++ " and " ++ y
    andList (x : xs) = x ++ ", " ++ andList xs
    andList [] = ""
    skipped =
#ifdef COMPARE_CORE_TEXT
      [(w, "core-text") | w <- ["10k keystrokes in one spot", "10k random splits"]]
#else
      []
#endif

-- | The workloads by what they do, each on a fresh rope and, where there is
-- a workload for it, an edited one.
chartRows :: [Group]
chartRows =
  [ Group
      "Loading and saving"
      [ Row "fromText" [("", "fromText")]
      , both "toText" "toText"
      ]
  , Group
      "Editing"
      [ both "Random inserts" "10k random inserts"
      , Row "Random deletes" [("", "10k random deletes")]
      , Row "Random inserts, one long line" [("", "one long line, 10k random inserts")]
      , Row "Edits at UTF-16 positions" [("", "10k edits at UTF-16 positions")]
      ]
  , Group
      "Typing"
      [ both "100 bursts of 100 keystrokes" "100 bursts of 100 keystrokes"
      , Row "The same, reading the line" [("", "100 bursts of 100 keystrokes, reading the line after each")]
      , both "10,000 keystrokes in one spot" "10k keystrokes in one spot"
      ]
  , Group
      "Reading"
      [ both "Random splits" "10k random splits"
      , both "getLine" "10k getLine"
      , Row "Byte offsets to UTF-16 positions" [("", "10k byte offsets to UTF-16 positions")]
      ]
  ]
  where
    both label w = Row label [(freshState, w), (editedState, "after 10k edits, " ++ w)]

freshState, editedState :: String
freshState = "fresh"
editedState = "edited"

-- | @--chart FILE@, and the CSV file the numbers come back through: the one
-- @--csv@ names, or FILE with @.csv@ for its extension. With @--redraw@ the
-- benchmarks do not run again, and the chart is drawn from that file.
chartArgs :: [String] -> Maybe (FilePath, FilePath, Bool, [String])
chartArgs args = case break (== "--chart") (filter (/= "--redraw") args) of
  (before, _ : svg : after) ->
    let rest = before ++ after
     in Just $ case lookup "--csv" (zip rest (drop 1 rest)) of
          Just csv -> (svg, csv, redraw, rest)
          Nothing -> let csv = dropSvg svg ++ ".csv" in (svg, csv, redraw, rest ++ ["--csv", csv])
  _ -> Nothing
  where
    redraw = "--redraw" `elem` args
    dropSvg f = maybe f reverse (L.stripPrefix "gvs." (reverse f))

commas :: Int -> String
commas = reverse . L.intercalate "," . L.unfoldr (\s -> if null s then Nothing else Just (L.splitAt 3 s)) . reverse . show

------------------------------------------------------------------------------
-- Memory

-- | The live heap each library needs to hold the document, freshly loaded
-- and after the random edits, and that of the document as one 'Text'.
footprints :: IO (Maybe Double, [Footprint])
footprints = do
  offsets <- fresh (\n -> let o = editOffsets workloadOps (sourceText n) in rnf o `seq` o) documentLines
  text <- footprint sourceText documentLines id rnf
  fps <-
    sequence
      [ measure "nano-rope" Nano.fromText (nanoEdited offsets) rnf
#ifdef COMPARE_TEXT_ROPE
      , measure "text-rope" TR.fromText (trEdited offsets) rnf
#endif
#ifdef COMPARE_YI_ROPE
      , measure "yi-rope" yiFromText (yiEdited offsets) forceYi
#endif
#ifdef COMPARE_CORE_TEXT
      , measure "core-text" ctFromText (ctEdited offsets) rnf
#endif
      ]
  pure (Just text, concat fps)
  where
    measure :: String -> (Text -> a) -> (a -> a) -> (a -> ()) -> IO [Footprint]
    measure library load edit deep = do
      loaded <- footprint sourceText documentLines load deep
      edited <- footprint sourceText documentLines (edit . load) deep
      pure
        [ Footprint freshState library loaded
        , Footprint editedState library edited
        ]
