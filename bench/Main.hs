{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Editor and language-server workloads. Optional @compare-text-rope@,
-- @compare-yi-rope@, and @compare-core-text@ Cabal flags enable comparisons
-- for workloads supported by each benchmark adapter.
--
-- @--chart FILE.svg@ plots timings, allocation, and retained heap.
module Main (main) where

import Chart (Chart (..), Footprint (..), Group (..), Row (..), Sample (..), commas, readSamples, render, showBytes)
import Control.DeepSeq (NFData (..))
import Control.Exception (throwIO, try)
import Control.Monad (forM_, unless)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.NanoRope (Position (..), Unit (..))
import qualified Data.Text.NanoRope as Nano
import Data.Text.NanoRope.Internal (kernels, kernelsName)
import Data.Version (showVersion)
import GHC.Stats (getRTSStatsEnabled)
import Lsp (lspBenchmarks, mkLspEnv)
import Memory (footprint, fresh)
import Rand (rands)
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

-- | Generated source-like text: short, mostly ASCII lines with occasional
-- accented characters, CJK characters, and emoji.
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

-- | Remove line feeds to model a single-line document such as minified output.
minified :: Text -> Text
minified = T.filter (/= '\n')

-- | Three input states: freshly loaded, without line feeds, and after
-- 10,000 random inserts.
data Ropes r = Ropes {ropeFresh :: !r, ropeOneLine :: !r, ropeEdited :: !r}
  deriving (Foldable)

ropes :: (Text -> r) -> ([Int] -> r -> r) -> Text -> [Int] -> Ropes r
ropes load edit text offsets = Ropes r (load (minified text)) (edit offsets r)
  where
    r = load text

data Env = Env
  { envText :: !Text
  , envNano :: !(Ropes Nano.Rope)
  , envChars :: ![Int]
  -- ^ Random character offsets.
  , envBursts :: ![(Int, Int)]
  -- ^ One hundred typing locations, each paired with its line index.
  , envPositions :: ![Position]
  -- ^ Random positions with their column in UTF-16 code units.
  , envByteOffsets :: ![Int]
#ifdef COMPARE_TEXT_ROPE
  , envTR :: !(Ropes TR.Rope)
  , envTR16 :: !TR16.Rope
#endif
#ifdef COMPARE_YI_ROPE
  , envYi :: !(Ropes Yi.YiString)
#endif
#ifdef COMPARE_CORE_TEXT
  , envCT :: !(Ropes CT.Rope)
#endif
  }

-- | Force lazy lists and finger trees before timing. The strict record
-- fields already evaluate the nano-rope and text-rope input trees.
instance NFData Env where
  rnf e =
    rnf (envChars e)
      `seq` rnf (envBursts e)
      `seq` rnf (envPositions e)
      `seq` rnf (envByteOffsets e)
#ifdef COMPARE_YI_ROPE
      `seq` foldr (seq . forceYi) () (envYi e)
#endif
#ifdef COMPARE_CORE_TEXT
      `seq` foldr (seq . rnf) () (envCT e)
#endif

mkEnv :: Int -> Int -> Env
mkEnv nLines nOps =
  Env
    { envText = text
    , envNano = nano
    , envChars = offsets
    , envBursts = [(i, Nano.convert Chars Lines i (ropeFresh nano)) | i <- L.take 100 offsets]
    , envPositions = [Position (r `mod` nLines) (r `mod` 40) | r <- L.take nOps (rands 3)]
    , envByteOffsets = [r `mod` (Nano.length Bytes (ropeFresh nano) + 1) | r <- L.take nOps (rands 4)]
#ifdef COMPARE_TEXT_ROPE
    , envTR = ropes TR.fromText (edits trOps) text offsets
    , envTR16 = TR16.fromText text
#endif
#ifdef COMPARE_YI_ROPE
    , envYi = ropes Yi.fromText (edits yiOps) text offsets
#endif
#ifdef COMPARE_CORE_TEXT
    , envCT = ropes ctFromText (edits ctOps) text offsets
#endif
    }
  where
    text = sourceText nLines
    nano = ropes Nano.fromText (edits nanoOps) text offsets
    offsets = editOffsets nOps text

-- | Deterministic code point offsets within the original document.
editOffsets :: Int -> Text -> [Int]
editOffsets nOps text = [r `mod` (chars + 1) | r <- L.take nOps (rands 2)]
  where
    chars = T.length text

------------------------------------------------------------------------------
-- Workloads

-- | Named workloads supported by a library's benchmark adapter.
type Workloads = [(String, Benchmarkable)]

-- | Label a workload on an already-edited rope. Edits change chunk
-- boundaries, so fresh and edited trees are measured separately.
afterEdits :: String -> String
afterEdits w = "after 10k edits, " ++ w

-- | Operations used by the shared workloads. Inlining resolves each
-- adapter's fields to known functions in the benchmark loops.
data Ops r = Ops
  { opLoad :: Text -> Benchmarkable
  , opToText :: r -> Text
  , opInsert :: Int -> r -> r
  -- ^ Insert @x@ at a code point offset.
  , opDelete :: Int -> r -> r
  -- ^ Delete the code point at an offset.
  , opSize :: r -> Int
  -- ^ Read the length after forcing any pending tree update.
  , opSplit :: Int -> r -> Int
  -- ^ Split at a character offset, and look at both halves.
  , opGetLine :: Maybe (Int -> r -> Text)
  -- ^ Read a line without its terminator, if supported by the adapter.
  }

-- | Insert a character at each of the given offsets.
edits :: Ops r -> [Int] -> r -> r
edits ops offsets = \r0 -> L.foldl' (flip (opInsert ops)) r0 offsets
{-# INLINE edits #-}

-- | Type a run of characters starting at each of the given offsets.
typing :: Ops r -> Int -> [Int] -> r -> Int
typing ops n offsets = \r0 -> opSize ops (L.foldl' burst r0 offsets)
  where
    burst r i = L.foldl' (\acc k -> opInsert ops (i + k) acc) r [0 .. n - 1]
{-# INLINE typing #-}

-- | Type in bursts and read the line after each key, modelling an editor redraw.
typingRead :: Ops r -> (Int -> r -> Text) -> [(Int, Int)] -> r -> Int
typingRead ops lineOf bursts = \r0 -> snd (L.foldl' burst (r0, 0) bursts)
  where
    burst acc (i, l) = L.foldl' (key i l) acc [0 .. 99 :: Int]
    key i l (r, n) k =
      let r' = opInsert ops (i + k) r
          !n' = n + T.length (lineOf l r')
       in (r', n')
{-# INLINE typingRead #-}

-- | Build shared workloads, excluding unsupported line reads and runs
-- explicitly omitted because of run time.
workloads :: Ops r -> [String] -> Ropes r -> Env -> Workloads
workloads ops tooSlow rs e =
  filter ((`notElem` tooSlow) . fst) . concat $
    [ [("fromText", opLoad ops (envText e))]
    , both "toText" (whnf (opToText ops))
    , both "10k random inserts" (whnf (opSize ops . edits ops (envChars e)))
    , both "100 bursts of 100 keystrokes" (whnf (typing ops 100 (L.take 100 (envChars e))))
    , both "10k keystrokes in one spot" (whnf (typing ops 10000 (L.take 1 (envChars e))))
    , [("10k random deletes", whnf (\r0 -> opSize ops (L.foldl' (flip (opDelete ops)) r0 (envChars e))) (ropeFresh rs))]
    , both "10k random splits" (whnf (\r -> L.foldl' (\n i -> n + opSplit ops i r) 0 (envChars e)))
    , [("one long line, 10k random inserts", whnf (opSize ops . edits ops (envChars e)) (ropeOneLine rs))]
    , concat
        [ ("100 bursts of 100 keystrokes, reading the line after each", whnf (typingRead ops lineOf (envBursts e)) (ropeFresh rs))
            : both "10k getLine" (whnf (\r -> L.foldl' (\n (Position l _) -> n + T.length (lineOf l r)) 0 (envPositions e)))
        | Just lineOf <- [opGetLine ops]
        ]
    ]
  where
    -- On the fresh rope and on the edited one.
    both w run = [(w, run (ropeFresh rs)), (afterEdits w, run (ropeEdited rs))]
{-# INLINE workloads #-}

nanoOps :: Ops Nano.Rope
nanoOps =
  Ops
    { opLoad = whnf Nano.fromText
    , opToText = Nano.toText
    , opInsert = \i -> Nano.insert Chars i "x"
    , opDelete = \i -> Nano.delete Chars i (i + 1)
    , -- Include the final buffered insertion in the measured work.
      opSize = \r -> if T.null (Nano.chunkAt Bytes 0 r) then 0 else Nano.length Chars r
    , opSplit = \i r -> let (a, b) = Nano.splitAt Chars i r in Nano.length Lines a + Nano.length Lines b
    , opGetLine = Just Nano.getLine
    }

nanoWorkloads :: Env -> Workloads
nanoWorkloads e =
  workloads nanoOps [] (envNano e) e
    ++ [ ("10k edits at UTF-16 positions", whnf (opSize nanoOps . lspEdits) rope)
       , ("10k byte offsets to UTF-16 positions", whnf byteToPosition rope)
       ]
  where
    rope = ropeFresh (envNano e)
    -- Convert incoming UTF-16 positions to byte offsets and insert there.
    lspEdits r0 = L.foldl' (\r pos -> Nano.insert Bytes (Nano.positionToOffset Utf16 Bytes pos r) "x" r) r0 (envPositions e)
    -- Convert byte-based tool results to UTF-16 positions.
    byteToPosition r = L.foldl' (\n i -> n + posColumn (Nano.offsetToPosition Bytes Utf16 i r)) 0 (envByteOffsets e)

#ifdef COMPARE_TEXT_ROPE
trOps :: Ops TR.Rope
trOps =
  Ops
    { opLoad = whnf TR.fromText
    , opToText = TR.toText
    , opInsert = \i r -> let (a, b) = TR.splitAt (fromIntegral i) r in a <> "x" <> b
    , opDelete = \i r ->
        let (a, b) = TR.splitAt (fromIntegral i) r
            (_, c) = TR.splitAt 1 b
         in a <> c
    , opSize = fromIntegral . TR.length
    , opSplit = \i r -> let (a, b) = TR.splitAt (fromIntegral i) r in lineBreaks a + lineBreaks b
    , opGetLine = Just (\l -> TR.toText . TR.getLine (fromIntegral l))
    }
  where
    lineBreaks = fromIntegral . TR.posLine . TR.lengthAsPosition

trWorkloads :: Env -> Workloads
trWorkloads e = workloads trOps [] (envTR e) e ++ [("10k edits at UTF-16 positions", whnf lspEdits (envTR16 e))]
  where
    lspEdits r0 = fromIntegral (TR16.length (L.foldl' edit r0 (envPositions e))) :: Int
    edit r (Position l c) =
      case TR16.splitAtPosition (TR16.Position (fromIntegral l) (fromIntegral c)) r of
        Just (a, b) -> a <> "x" <> b
        Nothing -> r
#endif

#ifdef COMPARE_YI_ROPE
-- | Force both the finger tree and its cached newline count before timing.
forceYi :: Yi.YiString -> ()
forceYi r = Yi.countNewLines r `seq` rnf r

-- | Construct a rope and evaluate its newline count for line-based workloads.
yiFromText :: Text -> Yi.YiString
yiFromText t = let r = Yi.fromText t in Yi.countNewLines r `seq` r

-- | Code point and line operations for the yi-rope adapter.
yiOps :: Ops Yi.YiString
yiOps =
  Ops
    { opLoad = whnf yiFromText
    , opToText = Yi.toText
    , -- Implement insertion through splitting and concatenation.
      opInsert = \i r -> let (a, b) = Yi.splitAt i r in a <> "x" <> b
    , opDelete = \i r -> let (a, b) = Yi.splitAt i r in a <> Yi.drop 1 b
    , opSize = Yi.length
    , opSplit = \i r -> let (a, b) = Yi.splitAt i r in Yi.countNewLines a + Yi.countNewLines b
    , opGetLine = Just (\l -> Yi.toText . Yi.takeWhile (/= '\n') . snd . Yi.splitAtLine l)
    }
#endif

#ifdef COMPARE_CORE_TEXT
-- | Wrap the input text in a rope. Benchmark to normal form to account for
-- lazy tree construction.
ctFromText :: Text -> CT.Rope
ctFromText = CT.intoRope

-- | Code point operations for the core-text adapter. Force split results
-- through their widths; this adapter has no line lookup.
ctOps :: Ops CT.Rope
ctOps =
  Ops
    { opLoad = nf ctFromText
    , opToText = CT.fromRope
    , opInsert = \i -> CT.insertRope i "x"
    , opDelete = \i r ->
        let (a, b) = CT.splitRope i r
            (_, c) = CT.splitRope 1 b
         in a <> c
    , opSize = CT.widthRope
    , opSplit = \i r -> let (a, b) = CT.splitRope i r in CT.widthRope a + CT.widthRope b
    , opGetLine = Nothing
    }

-- | Omit fresh-rope workloads where repeated measurement of the initial
-- large piece made the recorded benchmark impractically slow.
ctTooSlow :: [String]
ctTooSlow = ["10k keystrokes in one spot", "10k random splits"]
#endif

------------------------------------------------------------------------------

-- | The size of the document, and the number of operations in a workload.
documentLines, workloadOps :: Int
documentLines = 100000
workloadOps = 10000

-- | The size of the module of the language server workloads: some 300 kB.
lspLines :: Int
lspLines = 8000

-- | A library's name, workloads, and retained-heap measurement action.
data Library = Library String (Env -> Workloads) ([Int] -> IO [Footprint])

libraries :: [Library]
libraries =
  [ Library "nano-rope" nanoWorkloads (measure "nano-rope" Nano.fromText (edits nanoOps) rnf)
#ifdef COMPARE_TEXT_ROPE
  , Library "text-rope" trWorkloads (measure "text-rope" TR.fromText (edits trOps) rnf)
#endif
#ifdef COMPARE_YI_ROPE
  , Library "yi-rope" (\e -> workloads yiOps [] (envYi e) e) (measure "yi-rope" yiFromText (edits yiOps) forceYi)
#endif
#ifdef COMPARE_CORE_TEXT
  , Library "core-text" (\e -> workloads ctOps ctTooSlow (envCT e) e) (measure "core-text" ctFromText (edits ctOps) rnf)
#endif
  ]

benchmarks :: [Benchmark]
benchmarks =
    [ env (pure (mkEnv documentLines workloadOps)) $ \e ->
        bgroup
          "100k lines"
          -- Use nano-rope's workload names and include each available adapter.
          [ bgroup w [bench name run | Library name have _ <- libraries, Just run <- [lookup w (have e)]]
          | (w, _) <- nanoWorkloads e
          ]
    , -- Model language-server operations on a large module; see "Lsp".
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
      -- The comparison chart excludes nano-rope-only language-server runs.
      -- Their results remain in the CSV.
      samples <- filter ((`elem` libraryNames) . sampleLibrary) . readSamples <$> withFile csv ReadMode hGetContents'
      bytes <- Nano.length Bytes . Nano.fromText <$> fresh sourceText documentLines
      let ran = map sampleLibrary samples ++ map footprintLibrary fps
          others = filter (`elem` ran) (drop 1 libraryNames)
      withFile svg WriteMode $ \h -> do
        hSetEncoding h utf8
        hPutStr h . render $
          Chart
            { chartTitle = "nano-rope benchmarks" ++ (if null others then "" else ": " ++ andList others ++ " comparisons")
            , chartSubtitle =
                printf
                  "%s operations on %s generated lines (%.1f MB), or one Text conversion. GHC %s; %s scans."
                  (commas workloadOps)
                  (commas documentLines)
                  (fromIntegral bytes / 1e6 :: Double)
                  (showVersion fullCompilerVersion)
                  (kernelsName (last kernels))
            , chartNotes =
                [ "Fresh: built from Text. Edited: after " ++ commas workloadOps
                    ++ " random inserts. Rows with one run use a fresh rope. Timings depend on hardware, compiler, and document shape."
                , "Text conversions measure in-memory construction and flattening, not file I/O. Sharing the original buffer can make fresh-rope conversions much cheaper than edited-rope conversions."
                , "No mark: not implemented by this benchmark adapter. Hollow mark: omitted because of run time. Adapters cover different indexing and line APIs; see bench/Main.hs."
                , "Live heap: retained bytes after a major collection, with no separate reference to the source Text. Shared input buffers are included. Earlier rope versions are not retained."
                ]
            , chartLibraries = libraryNames
            , chartGroups = chartRows
            , chartSamples = samples
            , chartSkipped = skipped
            , chartTextHeap = textHeap
            , chartFootprints = fps
            }
      putStrLn ("Chart: " ++ svg)
  where
    libraryNames = ["nano-rope", "text-rope", "yi-rope", "core-text"]
    andList [x] = x
    andList [x, y] = x ++ " and " ++ y
    andList (x : xs) = x ++ ", " ++ andList xs
    andList [] = ""
    skipped =
#ifdef COMPARE_CORE_TEXT
      [(w, "core-text") | w <- ctTooSlow]
#else
      []
#endif

-- | Group workloads by operation, pairing fresh and edited runs where available.
chartRows :: [Group]
chartRows =
  [ Group
      "Text conversion"
      [ Row "fromText" [("", "fromText")]
      , inBoth "toText" "toText"
      ]
  , Group
      "Editing"
      [ inBoth "Random inserts" "10k random inserts"
      , Row "Random deletes" [("", "10k random deletes")]
      , Row "Random inserts, one long line" [("", "one long line, 10k random inserts")]
      , Row "Edits at UTF-16 positions" [("", "10k edits at UTF-16 positions")]
      ]
  , Group
      "Typing"
      [ inBoth "100 bursts of 100 keystrokes" "100 bursts of 100 keystrokes"
      , Row "The same, reading the line" [("", "100 bursts of 100 keystrokes, reading the line after each")]
      , inBoth "10,000 keystrokes in one spot" "10k keystrokes in one spot"
      ]
  , Group
      "Reading"
      [ inBoth "Random splits" "10k random splits"
      , inBoth "getLine" "10k getLine"
      , Row "Byte offsets to UTF-16 positions" [("", "10k byte offsets to UTF-16 positions")]
      ]
  ]
  where
    inBoth label w = Row label [(freshState, w), (editedState, afterEdits w)]

freshState, editedState :: String
freshState = "fresh"
editedState = "edited"

-- | Parse the chart path, CSV path, and redraw flag. The CSV defaults to
-- the chart path with a @.csv@ extension. @--redraw@ reuses saved timings
-- and allocation results; live-heap measurements still run.
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

------------------------------------------------------------------------------
-- Memory

-- | Measure retained heap for fresh and edited ropes and for the source 'Text'.
footprints :: IO (Maybe Double, [Footprint])
footprints = do
  offsets <- fresh (\n -> let o = editOffsets workloadOps (sourceText n) in rnf o `seq` o) documentLines
  text <- footprint sourceText documentLines id rnf
  fps <- sequence [heap offsets | Library _ _ heap <- libraries]
  pure (Just text, concat fps)

-- | The heap of a library holding the document, fresh and edited.
measure :: String -> (Text -> a) -> ([Int] -> a -> a) -> (a -> ()) -> [Int] -> IO [Footprint]
measure library load edit deep offsets = do
  loaded <- footprint sourceText documentLines load deep
  edited <- footprint sourceText documentLines (edit offsets . load) deep
  pure
    [ Footprint freshState library loaded
    , Footprint editedState library edited
    ]
