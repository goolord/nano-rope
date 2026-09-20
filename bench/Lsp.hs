{-# LANGUAGE OverloadedStrings #-}

-- | What a language server does with the rope, spelled the way @lsp@ and
-- @haskell-language-server@ spell it: every function here is the rope's side
-- of one of theirs, call for call, so that a descent saved or added there
-- shows here.
--
-- * @applyChange@ of @Language.LSP.VFS@: two UTF-16 positions to byte
--   offsets, each checked for landing inside a code point, and a 'replace'.
-- * @getCompletionPrefix@ of ghcide: the line under the cursor, after every
--   keystroke.
-- * The tokenizer of the semantic tokens: where every token of the module
--   starts and ends, its text, and its columns in UTF-16.
-- * @positionToCodePointPosition@ and back, as the positions of GHC and
--   those of the client are told apart.
-- * @rangeLinesFromVfs@ and @takeLineRange@: a few lines around somewhere,
--   as code actions read them.
--
-- The document is source code that is nearly all ASCII, as source code is.
--
-- Most of these ask where a position is and where its line starts. That is
-- one descent with 'Rope.metricsAtLineAndPosition' and was two or three
-- before there was such a thing, which is how @lsp@ and HLS asked at first:
-- those are kept, as the workloads "in two descents", to tell what the rope
-- gained from what its callers did.
module Lsp
  ( LspEnv (..)
  , mkLspEnv
  , lspBenchmarks
  ) where

import Control.DeepSeq (NFData (..))
import Data.Char (isAlpha, isAlphaNum, ord)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.NanoRope (Metrics (..), Position (..), Rope, Unit (..))
import qualified Data.Text.NanoRope as Rope
import Rand (rands)
import Test.Tasty.Bench

------------------------------------------------------------------------------
-- The document and the session

-- | A module: short lines, an accent or an emoji in a comment now and then.
moduleText :: Int -> Text
moduleText n = T.concat (zipWith line [0 :: Int ..] (L.take n (rands 5)))
  where
    line i r =
      T.concat
        [ T.replicate (r `mod` 4) "  "
        , case r `mod` 7 of
            0 -> "import qualified Data.Map.Strict as Map"
            1 -> "  where go acc (x : xs) = go (acc <> render x) xs"
            2 -> ""
            _ -> T.concat ["let value", T.pack (show i), " = compute (arg", T.pack (show (r `mod` 1000)), ") Map.empty"]
        , case r `mod` 97 of
            0 -> " -- caf\233 \20013\25991 \128512"
            1 -> " -- na\239ve r\233sum\233"
            _ -> ""
        , "\n"
        ]

-- | A change as a client sends it: a range of UTF-16 positions and what
-- replaces it.
data Change = Change !Position !Position !Text

-- | A change, and where it leaves the cursor.
data Step = Step !Change !Position

instance NFData Change where
  rnf (Change _ _ t) = rnf t

instance NFData Step where
  rnf (Step c _) = rnf c

snippets :: [Text]
snippets =
  [ "\n  where\n    go acc (x : xs) = go (acc <> render x) xs\n    go acc [] = acc"
  , " -- TODO: na\239ve, see the r\233sum\233 \128512"
  , "\nimport qualified Data.Map.Strict as Map"
  , "\n    , fieldName :: !(Maybe Text)"
  , "\n\nhelper :: Monad m => Int -> m [Int]\nhelper n = traverse (pure . (+ 1)) [0 .. n]"
  , " <> mempty"
  , "\n  let result = fromMaybe defaultValue (Map.lookup key table)\n  pure result"
  ]

width16 :: Char -> Int
width16 c = if ord c > 0xFFFF then 2 else 1

utf16Length :: Text -> Int
utf16Length = T.foldl' (\n c -> n + width16 c) 0

-- | Typing: go to the end of some line and type a snippet there, a keystroke
-- at a time, mistyping and erasing a letter now and then.
-- Comes with the document the steps leave behind, which the replay of the
-- session builds on the way and nobody need fold again.
typing :: Int -> Rope -> ([Step], Rope)
typing bursts rope0 = go bursts (rands 7) rope0
  where
    go :: Int -> [Int] -> Rope -> ([Step], Rope)
    go n (r1 : r2 : rs) rope
      | n > 0 =
          let line = r1 `mod` Rope.lineCount rope
              col = utf16Length (Rope.getLine line rope)
              steps = keys (T.unpack (snippets !! (r2 `mod` L.length snippets))) (1 :: Int) line col
              rope' = L.foldl' (\r (Step c _) -> applyChange TwoDescents r c) rope steps
              (rest, final) = go (n - 1) rs rope'
           in (steps ++ rest, final)
    go _ _ rope = ([], rope)

    keys [] _ _ _ = []
    keys (c : cs) i line col
      | i `mod` 17 == 0 =
          Step (Change (Position line col) (Position line col) "x") (Position line (col + 1))
            : Step (Change (Position line col) (Position line (col + 1)) "") (Position line col)
            : keys (c : cs) (i + 1) line col
      | c == '\n' = Step (Change (Position line col) (Position line col) "\n") (Position (line + 1) 0) : keys cs (i + 1) (line + 1) 0
      | otherwise =
          let col' = col + width16 c
           in Step (Change (Position line col) (Position line col) (T.singleton c)) (Position line col') : keys cs (i + 1) line col'

-- | The edits of renaming something used all over, in one notification, last
-- one first.
renaming :: Int -> Text -> [Change]
renaming edits text =
  [ Change (Position l 2) (Position l 6) "renamedIdentifier"
  | l <- L.reverse (L.nub (L.sort [candidates !! (r `mod` L.length candidates) | r <- L.take edits (rands 11)]))
  ]
  where
    candidates = [l | (l, line) <- zip [0 ..] (T.lines text), T.length line >= 8, T.all (< '\x80') line]

-- | A word of the document: its line, its columns in code points as GHC
-- counts them, and its start in UTF-16 code units as a client does.
data Token = Token !Int !Int !Int !Int

instance NFData Token where
  rnf !_ = ()

tokens :: Text -> [Token]
tokens text = concat (zipWith (\l -> go l 0 0) [0 ..] (T.lines text))
  where
    go l !col !col16 t =
      let (skipped, rest) = T.break (\c -> isAlpha c || c == '_') t
          (word, rest') = T.span (\c -> isAlphaNum c || c == '_' || c == '\'') rest
          from = col + T.length skipped
          from16 = col16 + utf16Length skipped
          to = from + T.length word
       in if T.null word
            then []
            else Token l from to from16 : go l to (from16 + utf16Length word) rest'

------------------------------------------------------------------------------
-- Language.LSP.VFS

-- | How a position and the start of its line are asked for.
data Asking
  = -- | 'Rope.metricsAtLineAndPosition'.
    OneDescent
  | -- | 'Rope.metricsAtPosition' and 'Rope.metricsAt', each on its own.
    TwoDescents

lineAndPosition :: Asking -> Unit -> Position -> Rope -> (Metrics, Metrics)
lineAndPosition OneDescent u pos rope = Rope.metricsAtLineAndPosition u pos rope
lineAndPosition TwoDescents u pos rope = (Rope.metricsAt Lines (posLine pos) rope, Rope.metricsAtPosition u pos rope)
{-# INLINE lineAndPosition #-}

-- | The byte offset of a position in UTF-16 code units, or 'Nothing' if it
-- lies within a code point.
utf16PositionToBytes :: Asking -> Position -> Rope -> Maybe Int
utf16PositionToBytes asking pos@(Position l c) str
  | reached line loc == c = Just (bytes loc)
  -- Short of the column: clamped to the end of the line, or rounded down to
  -- the start of a surrogate pair, the end of which is then one further.
  | uncurry reached (lineAndPosition asking Utf16 (Position l (c + 1)) str) == c + 1 = Nothing
  | otherwise = Just (bytes loc)
  where
    (line, loc) = lineAndPosition asking Utf16 pos str
    reached from m = utf16Units m - utf16Units from
{-# INLINE utf16PositionToBytes #-}

applyChange :: Asking -> Rope -> Change -> Rope
applyChange asking str (Change start finish new) = case asking of
  -- An insertion starts where it finishes, and is asked for once.
  OneDescent | start == finish -> case utf16PositionToBytes asking start str of
    Nothing -> str
    Just i -> Rope.replace Bytes i i new str
  _ -> case utf16PositionToBytes asking finish str of
    Nothing -> str
    Just j -> case utf16PositionToBytes asking start str of
      Nothing -> str
      Just i -> Rope.replace Bytes (min i j) j new str
{-# INLINE applyChange #-}

lineBounds :: Rope -> Int -> Maybe (Metrics, Metrics)
lineBounds rope l
  | l < Rope.lineCount rope = Just (Rope.metricsAt Lines l rope, Rope.metricsAt Lines (l + 1) rope)
  | otherwise = Nothing

-- | @codePointPositionToPosition@ and @positionToCodePointPosition@: a
-- position with its column in one unit as one with it in another, or
-- 'Nothing' if there is no such column, or it lies within a code point. A
-- column on the line is there in one descent; one in its terminator, or
-- beyond, takes the bounds of the line to tell.
convertPosition :: Unit -> Unit -> Asking -> Rope -> Position -> Maybe Position
convertPosition from to OneDescent text pos@(Position l c)
  | (line, loc) <- Rope.metricsAtLineAndPosition from pos text
  , newlines line == l
  , Rope.count from loc - Rope.count from line == c =
      Just (Position l (Rope.count to loc - Rope.count to line))
convertPosition from to _ text (Position l c) = do
  (lineStart, lineEnd) <- lineBounds text l
  let target = Rope.count from lineStart + c
      loc = Rope.metricsAt from target text
  if target <= Rope.count from lineEnd && Rope.count from loc == target
    then Just (Position l (Rope.count to loc - Rope.count to lineStart))
    else Nothing
{-# INLINE convertPosition #-}

rangeLines :: Rope -> Int -> Int -> Text
rangeLines rope lf lt = Rope.sliceText Lines lf lt rope

------------------------------------------------------------------------------
-- ghcide and the plugins

takeLineRange :: Int -> Int -> Rope -> [Text]
takeLineRange from to rope
  | to < from = []
  | otherwise = Rope.lines (Rope.slice Lines from (to + 1) rope)

-- | A line, if there is one: there is none after a final line terminator.
lineAt :: Asking -> Int -> Rope -> Maybe Text
lineAt OneDescent line rope
  | line < lastLine || line == lastLine && not (T.null text) = Just text
  | otherwise = Nothing
  where
    lastLine = Rope.lineCount rope - 1
    text = Rope.getLine line rope
lineAt TwoDescents line rope
  | Rope.convert Lines Bytes line rope < Rope.length Bytes rope = Just (Rope.getLine line rope)
  | otherwise = Nothing
{-# INLINE lineAt #-}

-- | What the completions read after a keystroke.
completionPrefix :: Asking -> Rope -> Position -> Int
completionPrefix asking rope (Position l c) = case lineAt asking l rope of
  Nothing -> 0
  Just curLine -> T.length (T.takeWhileEnd (\x -> isAlphaNum x || x == '.' || x == '_' || x == '\'') (T.take c curLine))

-- | The tokenizer of the semantic tokens: where a code point position is,
-- and its column in UTF-16, given that there is such a line and that it is
-- that long.
locate :: Asking -> Position -> Rope -> Maybe (Metrics, Int)
locate asking pos@(Position l c) rpe =
  let (lineStart, at) = lineAndPosition asking Chars pos rpe
   in if newlines lineStart == l && chars at - chars lineStart == c
        then Just (at, utf16Units at - utf16Units lineStart)
        else Nothing
{-# INLINE locate #-}

focusToken :: Asking -> Rope -> Token -> Int
focusToken asking rope (Token l from to _) =
  case (locate asking (Position l from) rope, locate asking (Position l to) rope) of
    (Just (tokenStart, ncs), Just (tokenEnd, nce)) ->
      let token = Rope.sliceText Bytes (bytes tokenStart) (bytes tokenEnd) rope
       in ncs + nce + T.length token
    _ -> -1

------------------------------------------------------------------------------
-- Workloads

-- | Replay a session, reading the document after every change.
replay :: Asking -> (Rope -> Position -> Int) -> Rope -> [Step] -> Int
replay asking observe = go 0
  where
    go !acc !rope [] = acc + Rope.length Bytes rope
    go !acc !rope (Step c cursor : steps) =
      let rope' = applyChange asking rope c
       in go (acc + observe rope' cursor) rope' steps
{-# INLINE replay #-}

observeNothing :: Rope -> Position -> Int
observeNothing rope _ = rope `seq` 1

semanticTokens :: Asking -> Rope -> [Token] -> Int
semanticTokens asking rope = L.foldl' (\n t -> n + focusToken asking rope t) 0
{-# INLINE semanticTokens #-}

convertPositions :: Asking -> Rope -> [Position] -> Int
convertPositions asking rope = L.foldl' step 0
  where
    step n p = case convertPosition Utf16 Chars asking rope p of
      Just cp | Just (Position l c) <- convertPosition Chars Utf16 asking rope cp -> n + l + c
      _ -> n - 1
{-# INLINE convertPositions #-}

readLines :: Rope -> [Position] -> Int
readLines rope = L.foldl' step 0
  where
    step n (Position l _) =
      n + T.length (rangeLines rope l (l + 3)) + sum (map T.length (takeLineRange l (l + 2) rope))

rename :: Asking -> Rope -> [Change] -> Int
rename asking rope changes = T.length (Rope.toText (L.foldl' (applyChange asking) rope changes))
{-# INLINE rename #-}

data LspEnv = LspEnv
  { lspOpened :: !Rope
  , lspEdited :: !Rope
  -- ^ After the typing.
  , lspTyping :: ![Step]
  , lspRenaming :: ![Change]
  , lspTokens :: ![Token]
  -- ^ Of the edited document.
  , lspPositions :: ![Position]
  -- ^ Where every fourth of those tokens starts.
  }

instance NFData LspEnv where
  rnf e = rnf (lspTyping e) `seq` rnf (lspRenaming e) `seq` rnf (lspTokens e) `seq` rnf (lspPositions e)

mkLspEnv :: Int -> LspEnv
mkLspEnv nLines =
  LspEnv
    { lspOpened = opened
    , lspEdited = edited
    , lspTyping = steps
    , lspRenaming = renaming 200 text
    , lspTokens = toks
    , lspPositions = every 4 [Position l c | Token l _ _ c <- toks]
    }
  where
    text = moduleText nLines
    opened = Rope.fromText text
    (steps, edited) = typing 100 opened
    toks = tokens (Rope.toText edited)
    every n xs = case xs of
      [] -> []
      x : _ -> x : every n (L.drop n xs)

lspBenchmarks :: LspEnv -> [Benchmark]
lspBenchmarks e =
  asked OneDescent
    ++ [ bench "reading lines" $ whnf (readLines (lspEdited e)) (lspPositions e)
       , bgroup "in two descents" (asked TwoDescents)
       ]
  where
    -- Inlined, so that each workload is compiled for the one way of asking.
    asked asking =
      [ bench "typing, the edits alone" $ whnf (replay asking observeNothing (lspOpened e)) (lspTyping e)
      , bench "typing, completion prefix after each key" $ whnf (replay asking (completionPrefix asking) (lspOpened e)) (lspTyping e)
      , bench "rename, 200 edits at once" $ whnf (rename asking (lspOpened e)) (lspRenaming e)
      , bench "semantic tokens" $ whnf (semanticTokens asking (lspEdited e)) (lspTokens e)
      , bench "position conversions" $ whnf (convertPositions asking (lspEdited e)) (lspPositions e)
      ]
    {-# INLINE asked #-}
