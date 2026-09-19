{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Properties of the rope against a model of plain 'Text' with naive,
-- obviously correct implementations of every unit. After every operation the
-- structural invariants of the tree are checked as well, which includes all
-- cached metrics and annotations. The instances are held to the laws of
-- their classes on top of that.
--
-- This file is compiled twice: against the library as it ships, and against
-- a build with tiny chunks and nodes (see the cabal file).
module Main (main) where

import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Primitive.ByteArray (ByteArray (..), cloneByteArray, indexByteArray, sizeofByteArray)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import qualified Data.Text.Lazy as TL
import qualified Data.Text.NanoRope as Plain
import Data.Text.NanoRope.Internal (Kernels (..), height, invariants, kernels, maxChunk)
import Data.Text.NanoRope.Measured (Measure (..), Metrics (..), Position (..), Rope, Unit (..))
import qualified Data.Text.NanoRope.Measured as Rope
import Data.Text.Unsafe (dropWord8, takeWord8)
import Data.Word (Word8)
import Test.QuickCheck.Classes.Base (Laws (..), commutativeMonoidLaws, eqLaws, monoidLaws, ordLaws, semigroupLaws, semigroupMonoidLaws, showLaws)
import Test.Tasty (TestTree, adjustOption, defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup
      ("nano-rope, chunks of " ++ show maxChunk ++ " bytes")
      [ testGroup
          "conversions"
          [ testProperty "fromText / toText" prop_roundtrip
          , testProperty "lazy text" prop_lazy
          , testProperty "toChunks" prop_chunks
          , testProperty "chunkAt" prop_chunkAt
          , testProperty "remeasure" prop_remeasure
          ]
      , testGroup
          "combining and breaking"
          [ testProperty "append" prop_append
          , testProperty "mconcat of many pieces" prop_mconcat
          , testProperty "splitAt" prop_splitAt
          , testProperty "slice / sliceText" prop_slice
          ]
      , testGroup
          "editing"
          [ testProperty "sequences of operations" prop_ops
          , testProperty "typing and erasing at a cursor" prop_typing
          , testProperty "keystrokes that continue each other" prop_run
          , testProperty "typing on from the same rope twice" prop_branching
          ]
      , testGroup
          "units"
          [ testProperty "metricsAt" prop_metricsAt
          , testProperty "convert" prop_convert
          , testProperty "metricsAtPosition / splitAtPosition" prop_position
          , testProperty "metricsToPosition" prop_toPosition
          , testProperty "position round trip" prop_positionRoundtrip
          ]
      , testGroup
          "lines"
          [ testProperty "lines / lineCount" prop_lines
          , testProperty "getLine" prop_getLine
          ]
      , testGroup
          "measures"
          [ testProperty "splitWhere by width" prop_splitWhereWidth
          , testProperty "splitWhere by line breaks" prop_splitWhereBreaks
          , testProperty "splitWhere by metrics" prop_splitWhereMetrics
          ]
      , testGroup
          "instances"
          [ testProperty "Eq ignores the shape of the tree" prop_eq
          , testProperty "Ord agrees with Text" prop_ord
          , testProperty "Show agrees with Text" prop_show
          ]
      , testGroup
          "laws"
          [ testProperty "the ropes they are tried on" prop_lawRopes
          , -- Enough for three ropes picked independently to be equal now and then.
            adjustOption (\(QuickCheckTests n) -> QuickCheckTests (max 500 n)) $
              lawsOf
                "Rope"
                [ eqLaws ropes
                , ordLaws ropes
                , eqOrdLaws ropes
                , semigroupLaws ropes
                , monoidLaws ropes
                , semigroupMonoidLaws ropes
                , homomorphismLaws
                , showLaws ropes
                , isStringLaws
                ]
          , lawsOf
              "Metrics"
              [ semigroupLaws metrics
              , monoidLaws metrics
              , commutativeMonoidLaws metrics
              , semigroupMonoidLaws metrics
              ]
          , testGroup
              "Measure"
              [ lawsOf "()" [measureLaws (Proxy :: Proxy ())]
              , lawsOf "pairs" [measureLaws (Proxy :: Proxy (Breaks, Width))]
              , lawsOf "triples" [measureLaws (Proxy :: Proxy ((), Width, Breaks))]
              ]
          , -- The measures of this file, which the rest of it relies on.
            testGroup
              "test measures"
              [ lawsOf "Breaks" [semigroupLaws breaks, monoidLaws breaks, semigroupMonoidLaws breaks, measureLaws breaks]
              , lawsOf "Width" [semigroupLaws widths, monoidLaws widths, semigroupMonoidLaws widths, measureLaws widths]
              ]
          ]
      , testGroup
          "plain interface"
          [ testProperty "agrees with the measured one" prop_plain
          ]
      , testGroup
          "big"
          [ localOption (QuickCheckTests 5) (testProperty "a tall tree" prop_big)
          ]
      , testGroup
          ("chunk scans: " ++ L.intercalate ", " (map kernelsName kernels))
          [ testProperty "metrics" prop_scanMetrics
          , testProperty "line feeds" prop_scanNewlines
          , testProperty "the next line feed" prop_scanNext
          , testProperty "the previous line feed" prop_scanPrevious
          , testProperty "the k-th line feed" prop_scanNth
          , testProperty "code points and UTF-16 code units" prop_scanUnits
          ]
      ]

------------------------------------------------------------------------------
-- A measure with some bite

-- | Line breaks where @\\r\\n@, @\\r@ and @\\n@ each count once. Needs to
-- remember its edges to be a homomorphism: gluing @"a\\r"@ to @"\\nb"@ makes
-- one break out of two.
data Breaks
  = NoText
  | Breaks !Int !Bool !Bool
  deriving (Eq, Show)

breakCount :: Breaks -> Int
breakCount NoText = 0
breakCount (Breaks n _ _) = n

instance Semigroup Breaks where
  NoText <> b = b
  a <> NoText = a
  Breaks n1 lf1 cr1 <> Breaks n2 lf2 cr2 =
    Breaks (n1 + n2 - (if cr1 && lf2 then 1 else 0)) lf1 cr2

instance Monoid Breaks where
  mempty = NoText

instance Measure Breaks where
  measureChunk t
    | T.null t = NoText
    | otherwise =
        Breaks
          (T.count "\r" t + T.count "\n" t - T.count "\r\n" t)
          (T.head t == '\n')
          (T.last t == '\r')

-- | Display width, with everything beyond Latin taking two columns.
newtype Width = Width Int
  deriving (Eq, Ord, Show)

instance Semigroup Width where
  Width a <> Width b = Width (a + b)

instance Monoid Width where
  mempty = Width 0

charWidth :: Char -> Int
charWidth c = if c >= '\x1100' then 2 else 1

instance Measure Width where
  measureChunk = Width . T.foldl' (\n c -> n + charWidth c) 0

type R = Rope (Breaks, Width)

------------------------------------------------------------------------------
-- The model

utf8Len, utf16Len :: Char -> Int
utf8Len c
  | c < '\x80' = 1
  | c < '\x800' = 2
  | c < '\x10000' = 3
  | otherwise = 4
utf16Len c = if c < '\x10000' then 1 else 2

naiveMetrics :: Text -> Metrics
naiveMetrics t =
  Metrics
    { bytes = sum (map utf8Len s)
    , chars = L.length s
    , utf16Units = sum (map utf16Len s)
    , newlines = L.length (filter (== '\n') s)
    }
  where
    s = T.unpack t

-- | The number of characters before the location of an offset.
charsAt :: Unit -> Int -> Text -> Int
charsAt u k t
  | k <= 0 = 0
  | otherwise = case u of
      Chars -> min k (T.length t)
      Bytes -> fitting utf8Len
      Utf16 -> fitting utf16Len
      Lines -> case L.drop (k - 1) [i + 1 | (i, '\n') <- zip [0 ..] s] of
        p : _ -> p
        [] -> L.length s
  where
    s = T.unpack t
    fitting w = L.length (takeWhile (<= k) (drop 1 (scanl (+) 0 (map w s))))

-- | The characters before a range of offsets and before its end.
charRange :: Unit -> Int -> Int -> Text -> (Int, Int)
charRange u i j t = (ni, if j <= i then ni else charsAt u j t)
  where
    ni = charsAt u i t

-- | Start and content of every line.
lineTable :: Text -> [(Int, Text)]
lineTable = go 0 . T.splitOn "\n"
  where
    go _ [] = []
    go s [final] = [(s, final)]
    go s (l : ls) = (s, fromMaybe l (T.stripSuffix "\r" l)) : go (s + T.length l + 1) ls

naiveLines :: Text -> [Text]
naiveLines t
  | T.null t || T.last t == '\n' = L.init table
  | otherwise = table
  where
    table = map snd (lineTable t)

-- | The number of characters before a position.
charsAtPosition :: Unit -> Position -> Text -> Int
charsAtPosition u (Position l c) t = case L.drop (max 0 l) (lineTable t) of
  [] -> T.length t
  (s, content) : _ -> s + charsAt u c content

naivePosition :: Unit -> Int -> Text -> Position
naivePosition u n t = Position (T.count "\n" before) (Rope.count u (naiveMetrics column))
  where
    before = T.take n t
    column = T.takeWhileEnd (/= '\n') before

------------------------------------------------------------------------------
-- Generators

-- | 1 for the small build, 64 for the real one: sizes are scaled so that both
-- grow trees of a few levels.
sizeFactor :: Int
sizeFactor = maxChunk `quot` 16

genPiece :: Gen String
genPiece =
  frequency
    [ (12, pure <$> elements "abcxyz ")
    , (3, pure "\n")
    , (1, pure "\r")
    , (1, pure "\r\n")
    , (2, pure <$> elements "\233\223\241") -- two bytes
    , (2, pure <$> elements "\8364\20013\12354") -- three bytes
    , (2, pure <$> elements "\128512\119070\127881") -- four bytes, surrogate pairs
    ]

genText :: Int -> Gen Text
genText n = do
  oneLine <- frequency [(4, pure False), (1, pure True)]
  t <- T.pack . concat <$> vectorOf n genPiece
  pure (if oneLine then T.filter (/= '\n') t else t)

-- | Every candidate is strictly shorter, or shrinking would never end.
shrinkText :: Text -> [Text]
shrinkText t =
  [half | h > 0, half <- [T.take h t, T.drop h t]]
    ++ [T.take i t <> T.drop (i + 1) t | i <- [0 .. min 30 (n - 1)]]
  where
    n = T.length t
    h = n `quot` 2

-- | A document.
newtype Doc = Doc Text
  deriving (Show)

instance Arbitrary Doc where
  arbitrary = do
    n <- frequency [(1, pure 0), (2, choose (0, 8)), (8, choose (0, 200 * sizeFactor))]
    Doc <$> genText n
  shrink (Doc t) = Doc <$> shrinkText t

-- | Something to insert: usually a keystroke, sometimes a paste.
newtype Snippet = Snippet Text
  deriving (Show)

instance Arbitrary Snippet where
  arbitrary = do
    n <- frequency [(4, choose (0, 3)), (2, choose (0, 40)), (1, choose (0, 80 * sizeFactor))]
    Snippet <$> genText n
  shrink (Snippet t) = Snippet <$> shrinkText t

instance Arbitrary Unit where
  arbitrary = elements [minBound .. maxBound]

-- | An offset relative to the length of whatever it is applied to, reaching
-- a little beyond both ends.
newtype Offset = Offset Int
  deriving (Show)

instance Arbitrary Offset where
  arbitrary = Offset <$> choose (0, 1000)
  shrink (Offset k) = Offset <$> shrink k

resolve :: Unit -> Offset -> Text -> Int
resolve u (Offset k) t = k * (Rope.count u (naiveMetrics t) + 5) `quot` 1000 - 2

data Op
  = Insert Unit Offset Snippet
  | Delete Unit Offset Offset
  | Replace Unit Offset Offset Snippet
  | Append Snippet
  | Prepend Snippet
  | Take Unit Offset
  | Drop Unit Offset
  | Slice Unit Offset Offset
  | Rejoin Unit Offset
  | Typed Unit Offset [Snippet] Int
  deriving (Show)

instance Arbitrary Op where
  arbitrary =
    frequency
      [ (6, Insert <$> arbitrary <*> arbitrary <*> arbitrary)
      , (6, Delete <$> arbitrary <*> arbitrary <*> nearby)
      , (4, Replace <$> arbitrary <*> arbitrary <*> nearby <*> arbitrary)
      , (2, Append <$> arbitrary)
      , (2, Prepend <$> arbitrary)
      , (1, Take <$> arbitrary <*> arbitrary)
      , (1, Drop <$> arbitrary <*> arbitrary)
      , (1, Slice <$> arbitrary <*> arbitrary <*> arbitrary)
      , (3, Rejoin <$> arbitrary <*> arbitrary)
      , (4, Typed <$> arbitrary <*> arbitrary <*> resize 6 (listOf arbitrary) <*> choose (0, 4))
      ]
    where
      -- Ends of ranges, as lengths: mostly short, so that the text survives.
      nearby = Offset <$> frequency [(5, choose (0, 20)), (1, choose (0, 1000))]
  shrink op = case op of
    Insert u i s -> Insert u i <$> shrink s
    Replace u i j s -> Delete u i j : (Replace u i j <$> shrink s)
    Append s -> Append <$> shrink s
    Prepend s -> Prepend <$> shrink s
    Typed u i ss erased -> [Typed u i ss' erased | ss' <- shrink ss] ++ [Typed u i ss 0 | erased > 0]
    _ -> []

-- | Ranges are generated as a start and a length.
range :: Unit -> Offset -> Offset -> Text -> (Int, Int)
range u i (Offset len) t = (start, start + len * (Rope.count u (naiveMetrics t) + 5) `quot` 1000)
  where
    start = resolve u i t

apply :: Op -> (R, Text) -> (R, Text)
apply op (r, t) = case op of
  Insert u i (Snippet s) ->
    let k = resolve u i t
        n = charsAt u k t
     in (Rope.insert u k s r, T.take n t <> s <> T.drop n t)
  Delete u i j ->
    let (a, b) = range u i j t
        (na, nb) = charRange u a b t
     in (Rope.delete u a b r, T.take na t <> T.drop nb t)
  Replace u i j (Snippet s) ->
    let (a, b) = range u i j t
        (na, nb) = charRange u a b t
     in (Rope.replace u a b s r, T.take na t <> s <> T.drop nb t)
  Append (Snippet s) -> (r <> Rope.fromText s, t <> s)
  Prepend (Snippet s) -> (Rope.fromText s <> r, s <> t)
  Take u i ->
    let k = resolve u i t
     in (Rope.take u k r, T.take (charsAt u k t) t)
  Drop u i ->
    let k = resolve u i t
     in (Rope.drop u k r, T.drop (charsAt u k t) t)
  Slice u i j ->
    let (a, b) = range u i j t
        (na, nb) = charRange u a b t
     in (Rope.slice u a b r, T.take (nb - na) (T.drop na t))
  Rejoin u i ->
    let (a, b) = Rope.splitAt u (resolve u i t) r
     in (a <> b, t)
  Typed u i snippets erased ->
    let (r', t', cursor) = L.foldl' (keystroke u) (r, t, resolve u i t) snippets
        (na, nb) = charRange u (cursor - erased) cursor t'
     in (Rope.delete u (cursor - erased) cursor r', T.take na t' <> T.drop nb t')

-- | Insert at a cursor and move it to where the next keystroke goes: by what
-- the text measures, from the offset asked for or the start of the rope.
keystroke :: Unit -> (R, Text, Int) -> Snippet -> (R, Text, Int)
keystroke u (r, t, cursor) (Snippet s) =
  (Rope.insert u cursor s r, T.take n t <> s <> T.drop n t, max 0 cursor + Rope.count u (naiveMetrics s))
  where
    n = charsAt u cursor t

-- | A rope with some history, and the text it should hold.
data Edited = Edited R Text

instance Show Edited where
  show (Edited r t) = show t ++ " as a tree of height " ++ show (height r)

instance Arbitrary Edited where
  arbitrary = do
    Doc t <- arbitrary
    ops <- resize 8 (listOf arbitrary)
    pure (uncurry Edited (L.foldl' (flip apply) (Rope.fromText t, t) ops))
  shrink (Edited _ t) = [Edited (Rope.fromText t') t' | t' <- shrinkText t]

------------------------------------------------------------------------------
-- Properties

-- | The rope is a valid tree holding exactly this text.
holds :: R -> Text -> Property
holds r t =
  conjoin
    [ counterexample "invariants" (invariants r === [])
    , counterexample "text" (Rope.toText r === t)
    , counterexample "metrics" (Rope.metrics r === naiveMetrics t)
    , counterexample "measure" (Rope.measure r === (measureChunk t, measureChunk t))
    ]

prop_roundtrip :: Doc -> Property
prop_roundtrip (Doc t) = holds (Rope.fromText t) t

prop_lazy :: [Doc] -> Property
prop_lazy docs =
  holds (Rope.fromLazyText lazy) (TL.toStrict lazy)
    .&&. Rope.toLazyText (Rope.fromLazyText lazy :: R) === lazy
    .&&. Rope.toString (Rope.fromLazyText lazy :: R) === TL.unpack lazy
  where
    lazy = TL.fromChunks [t | Doc t <- docs]

prop_chunks :: Edited -> Property
prop_chunks (Edited r t) =
  T.concat chunks === t
    .&&. counterexample "empty chunk" (not (any T.null chunks))
    .&&. counterexample "oversized chunk" (all ((<= maxChunk) . bytes . naiveMetrics) chunks)
    .&&. Rope.foldrChunks (\c n -> T.length c + n) 0 r === T.length t
  where
    chunks = Rope.toChunks r

prop_chunkAt :: Edited -> Unit -> Offset -> Property
prop_chunkAt (Edited r t) u i =
  counterexample (show chunk) $
    (chunk `T.isPrefixOf` rest) .&&. (T.null chunk === T.null rest)
  where
    k = resolve u i t
    chunk = Rope.chunkAt u k r
    rest = T.drop (charsAt u k t) t

prop_remeasure :: Edited -> Property
prop_remeasure (Edited r t) =
  invariants plain === []
    .&&. Rope.toText plain === t
    .&&. holds (Rope.remeasure plain) t
  where
    plain = Rope.remeasure r :: Rope ()

prop_append :: Edited -> Edited -> Property
prop_append (Edited a ta) (Edited b tb) = holds (a <> b) (ta <> tb)

prop_mconcat :: [Snippet] -> Property
prop_mconcat snippets =
  holds (mconcat (map Rope.fromText ts)) (T.concat ts)
    .&&. holds (L.foldl' (\acc t -> Rope.fromText t <> acc) mempty ts) (T.concat (reverse ts))
  where
    ts = [t | Snippet t <- snippets]

prop_splitAt :: Edited -> Unit -> Offset -> Property
prop_splitAt (Edited r t) u i =
  counterexample (show (u, k)) $
    holds a (T.take n t) .&&. holds b (T.drop n t)
  where
    k = resolve u i t
    n = charsAt u k t
    (a, b) = Rope.splitAt u k r

prop_slice :: Edited -> Unit -> Offset -> Offset -> Property
prop_slice (Edited r t) u i j =
  counterexample (show (u, a, b)) $
    holds (Rope.slice u a b r) expected .&&. Rope.sliceText u a b r === expected
  where
    (a, b) = range u i j t
    (na, nb) = charRange u a b t
    expected = T.take (nb - na) (T.drop na t)

prop_ops :: Doc -> [Op] -> Property
prop_ops (Doc t0) = go (1 :: Int) (Rope.fromText t0, t0)
  where
    go _ (r, t) [] = holds r t
    go i (r, t) (op : ops) =
      holds r t .&&. counterexample ("operation " ++ show i) (go (i + 1) (apply op (r, t)) ops)

-- | A burst of keystrokes at the cursor.
data Burst
  = Typing Text Int
  | Erasing Int
  deriving (Show)

instance Arbitrary Burst where
  arbitrary =
    oneof
      [ Typing <$> (choose (1, 3) >>= genText) <*> choose (1, 8 * sizeFactor)
      , Erasing <$> choose (1, 16 * sizeFactor)
      ]

-- | Lots of small edits at a cursor, one keystroke at a time: fills chunks
-- until they split and drains them until they merge.
prop_typing :: Doc -> Offset -> Property
prop_typing (Doc t0) start = forAll (resize 10 (listOf arbitrary)) $ \bursts ->
  let cursor = max 0 (min (T.length t0) (resolve Chars start t0))
   in go (Rope.fromText t0, t0, cursor) bursts
  where
    go (r, t, _) [] = holds r t
    go st@(r, t, _) (b : bs) = holds r t .&&. counterexample (show b) (go (burst b st) bs)

    burst (Typing s n) st = L.foldl' (\acc _ -> typeOne s acc) st [1 .. n]
    burst (Erasing n) st = L.foldl' (\acc _ -> eraseOne acc) st [1 .. n]

    typeOne s (r, t, c) = (Rope.insert Chars c s r, T.take c t <> s <> T.drop c t, c + T.length s)
    eraseOne st@(r, t, c)
      | c <= 0 = st
      | otherwise = (Rope.delete Chars (c - 1) c r, T.take (c - 1) t <> T.drop c t, c - 1)

-- | Keystrokes that continue each other, in any unit and from any offset,
-- also one that is clamped or falls inside a code point. Every rope on the
-- way is right, whether or not it has been looked at before typing on.
prop_run :: Edited -> Unit -> Offset -> Property
prop_run (Edited r0 t0) u i = forAll (resize 12 (listOf arbitrary)) $ \snippets ->
  let steps = L.scanl (keystroke u) (r0, t0, resolve u i t0) snippets
      (unread, final, _) = L.foldl' (keystroke u) (r0, t0, resolve u i t0) snippets
   in counterexample (show (u, resolve u i t0)) $
        holds unread final .&&. conjoin [holds r t | (r, t, _) <- steps]

-- | A rope in the middle of typing is a value like any other: typing on from
-- it twice gives two ropes and leaves the first alone.
prop_branching :: Edited -> Offset -> Snippet -> Snippet -> Snippet -> Property
prop_branching (Edited r0 t0) i s1 s2 s3 =
  conjoin
    [ counterexample "one way" (holds ra ta)
    , counterexample "the other way" (holds rb tb)
    , counterexample "erased" (holds rc tc)
    , counterexample "the rope they came from" (holds r t)
    ]
  where
    start = max 0 (min (T.length t0) (resolve Chars i t0))
    (r, t, cursor) = L.foldl' (keystroke Chars) (r0, t0, start) [Snippet "ab", s1]
    (ra, ta, _) = keystroke Chars (r, t, cursor) s2
    (rb, tb, _) = keystroke Chars (r, t, cursor) s3
    rc = Rope.delete Chars (cursor - 1) cursor r
    tc = T.take (cursor - 1) t <> T.drop cursor t

prop_metricsAt :: Edited -> Unit -> Offset -> Property
prop_metricsAt (Edited r t) u i =
  counterexample (show (u, k)) $
    Rope.metricsAt u k r === naiveMetrics (T.take (charsAt u k t) t)
  where
    k = resolve u i t

prop_convert :: Edited -> Unit -> Unit -> Offset -> Property
prop_convert (Edited r t) from to i =
  counterexample (show (from, to, k)) $
    Rope.convert from to k r === Rope.count to (naiveMetrics (T.take (charsAt from k t) t))
  where
    k = resolve from i t

genPosition :: Text -> Gen Position
genPosition t =
  Position
    <$> choose (-1, T.count "\n" t + 1)
    <*> frequency [(6, choose (-1, 12)), (2, choose (0, 40 * sizeFactor)), (1, pure maxBound)]

prop_position :: Edited -> Unit -> Property
prop_position (Edited r t) u = forAll (genPosition t) $ \pos ->
  let n = charsAtPosition u pos t
      (a, b) = Rope.splitAtPosition u pos r
   in Rope.metricsAtPosition u pos r === naiveMetrics (T.take n t)
        .&&. Rope.toText a === T.take n t
        .&&. Rope.toText b === T.drop n t
        .&&. Rope.positionToOffset u Chars pos r === n

prop_toPosition :: Edited -> Unit -> Unit -> Offset -> Property
prop_toPosition (Edited r t) from to i =
  counterexample (show (from, to, k)) $
    Rope.offsetToPosition from to k r === naivePosition to (charsAt from k t) t
  where
    k = resolve from i t

-- | Positions of actual locations survive the trip through offsets, unless
-- they sit inside a line terminator, which no position can address.
prop_positionRoundtrip :: Edited -> Unit -> Unit -> Offset -> Property
prop_positionRoundtrip (Edited r t) u via i =
  via /= Lines && not insideTerminator ==>
    Rope.offsetToPosition via u (Rope.positionToOffset u via pos r) r === pos
  where
    n = charsAt Chars (resolve Chars i t) t
    pos = naivePosition u n t
    insideTerminator = T.take 1 (T.drop n t) == "\n" && T.takeEnd 1 (T.take n t) == "\r"

prop_lines :: Edited -> Property
prop_lines (Edited r t) =
  Rope.lines r === naiveLines t
    .&&. Rope.lineCount r === T.count "\n" t + 1
    .&&. Rope.length Lines r === T.count "\n" t

prop_getLine :: Edited -> Property
prop_getLine (Edited r t) = forAll (choose (-1, L.length table + 1)) $ \l ->
  Rope.getLine l r === (if l < 0 then "" else maybe "" snd (L.lookup l (zip [0 ..] table)))
  where
    table = lineTable t

-- | The longest prefix (in characters) on which a predicate fails.
longestPrefix :: (Text -> Bool) -> Text -> Int
longestPrefix p t = L.length (takeWhile (not . p) (drop 1 (T.inits t)))

prop_splitWhereWidth :: Edited -> Offset -> Property
prop_splitWhereWidth (Edited r t) (Offset k) =
  counterexample (show limit) $
    holds a (T.take n t) .&&. holds b (T.drop n t)
  where
    Width whole = measureChunk t
    limit = k * (whole + 5) `quot` 1000 - 2
    (a, b) = Rope.splitWhere (\_ (_, Width w) -> w > limit) r
    n = longestPrefix (\p -> measureChunk p > Width limit) t

prop_splitWhereBreaks :: Edited -> Offset -> Property
prop_splitWhereBreaks (Edited r t) (Offset k) =
  counterexample (show limit) $
    Rope.metricsWhere (\_ (bs, _) -> breakCount bs >= limit) r === naiveMetrics (T.take n t)
  where
    limit = k * (breakCount (measureChunk t) + 3) `quot` 1000
    n = longestPrefix (\p -> breakCount (measureChunk p) >= limit) t

prop_splitWhereMetrics :: Edited -> Unit -> Offset -> Property
prop_splitWhereMetrics (Edited r t) u i =
  u /= Lines ==>
    Rope.metricsWhere (\m _ -> Rope.count u m > k) r === Rope.metricsAt u k r
  where
    k = max 0 (resolve u i t)

prop_eq :: Edited -> Property
prop_eq (Edited r t) =
  r === Rope.fromText t
    .&&. compare r (Rope.fromText t) === EQ
    .&&. (r /= Rope.fromText (t <> "!")) === True
    .&&. (r == Rope.fromText swapped) === (t == swapped)
  where
    -- Same metrics, so that equality has to look at the text.
    swapped = T.map (\c -> case c of 'a' -> 'b'; 'b' -> 'a'; _ -> c) t

prop_ord :: Edited -> Edited -> Offset -> Property
prop_ord (Edited a ta) (Edited b tb) i =
  compare a b === compare ta tb
    .&&. (a == b) === (ta == tb)
    .&&. compare a (Rope.fromText grafted) === compare ta grafted
  where
    -- Shares a prefix with the first text.
    grafted = T.take (resolve Chars i ta) ta <> tb

prop_show :: Edited -> Property
prop_show (Edited r t) = show r === show t

------------------------------------------------------------------------------
-- Laws

-- | The laws of some classes as a group of tests.
lawsOf :: String -> [Laws] -> TestTree
lawsOf name sets =
  testGroup name [testGroup cls [testProperty law p | (law, p) <- properties] | Laws cls properties <- sets]

ropes :: Proxy R
ropes = Proxy

metrics :: Proxy Metrics
metrics = Proxy

breaks :: Proxy Breaks
breaks = Proxy

widths :: Proxy Width
widths = Proxy

-- | The laws speak of ropes that are equal, or that differ late, and two
-- documents picked at random are neither. These are a few texts over a
-- common stem, some of them alike in every metric, of several chunks and of
-- two heights in either build.
lawTexts :: [(Int, Text)]
lawTexts =
  [ (1, "")
  , (3, stem <> "xyz")
  , (2, stem <> "xzy")
  , (2, stem <> "xyz\128512")
  , (2, T.replicate 8 stem <> "xyz")
  , (1, T.replicate 8 stem <> "xzy")
  ]
  where
    stem = T.take (2 * maxChunk) (T.replicate maxChunk "ab\nc\233 \8364\r\n\128512xyz")

-- | A rope of a text by one of several routes, which leave it in different
-- chunks: all at once, out of pieces, with its end typed and maybe still
-- waiting, with a gap filled in, or cut out of something longer.
ropeOf :: Measure a => Text -> Gen (Rope a)
ropeOf t =
  oneof
    [ pure (Rope.fromText t)
    , do
        cuts <- L.sort <$> resize 6 (listOf (choose (0, n)))
        pure (mconcat [Rope.fromText (T.take (j - i) (T.drop i t)) | (i, j) <- zip (0 : cuts) (cuts ++ [n])])
    , do
        k <- choose (max 0 (n - 8 * sizeFactor), n)
        pure (L.foldl' (\r (i, c) -> Rope.insert Chars i (T.singleton c) r) (Rope.fromText (T.take k t)) (zip [k ..] (T.unpack (T.drop k t))))
    , do
        i <- choose (0, n)
        j <- choose (i, n)
        pure (Rope.insert Chars i (T.take (j - i) (T.drop i t)) (Rope.fromText (T.take i t <> T.drop j t)))
    , do
        Snippet before <- arbitrary
        Snippet after <- arbitrary
        let i = T.length before
        pure (Rope.slice Chars i (i + n) (Rope.fromText (before <> t <> after)))
    ]
  where
    n = T.length t

-- | For the laws only, see 'lawTexts'. Everything else is tried on 'Edited'.
instance Measure a => Arbitrary (Rope a) where
  arbitrary = frequency [(w, pure t) | (w, t) <- lawTexts] >>= ropeOf
  shrink r = Rope.fromText <$> shrinkText (Rope.toText r)

-- | The laws are tried on what they are about: equal ropes in different
-- chunks, and different ropes which no metric tells apart.
prop_lawRopes :: R -> R -> Property
prop_lawRopes a b =
  checkCoverage $
    cover 10 (a == b && Rope.toChunks a /= Rope.toChunks b) "equal, in different chunks" $
      cover 5 (a /= b && Rope.metrics a == Rope.metrics b) "different, with the same metrics" $
        holds a (Rope.toText a) .&&. holds b (Rope.toText b)

-- | What "Test.QuickCheck.Classes.Base" leaves out. The instances define '=='
-- and 'compare', and the rest of both classes has to agree with those.
eqOrdLaws :: forall a. (Ord a, Arbitrary a, Show a) => Proxy a -> Laws
eqOrdLaws _ =
  Laws
    "Eq and Ord"
    [ ("Negation", property $ \(a :: a) b -> (a /= b) === not (a == b))
    , ("compare is EQ where == holds", property $ \(a :: a) b -> (compare a b == EQ) === (a == b))
    , ("compare, turned around", property $ \(a :: a) b -> compare a b === opposite (compare b a))
    ,
      ( "Operators"
      , property $ \(a :: a) b ->
          let o = compare a b
           in conjoin [(a < b) === (o == LT), (a <= b) === (o /= GT), (a > b) === (o == GT), (a >= b) === (o /= LT)]
      )
    , ("min and max", property $ \(a :: a) b -> (min a b, max a b) === (if a <= b then (a, b) else (b, a)))
    ]
  where
    opposite o = case o of
      LT -> GT
      EQ -> EQ
      GT -> LT

-- | Whatever is read off a rope is read off its parts.
homomorphismLaws :: Laws
homomorphismLaws =
  Laws
    "Monoid homomorphisms"
    [ ("toText", homomorphism Rope.toText)
    , ("metrics", homomorphism Rope.metrics)
    , ("measure", homomorphism Rope.measure)
    ]
  where
    homomorphism :: (Monoid b, Eq b, Show b) => (R -> b) -> Property
    homomorphism f = property $ \a b -> f (a <> b) === f a <> f b .&&. f mempty === mempty

-- | 'IsString' has no laws. Ropes are held to what 'Text' does, which is to
-- replace surrogates.
isStringLaws :: Laws
isStringLaws =
  Laws
    "IsString"
    [ ("fromString, like Text", forAll genString $ \s -> holds (fromString s) (fromString s))
    , ("toString . fromString", forAll genString $ \s -> Rope.toString (fromString s :: R) === T.unpack (fromString s))
    ]
  where
    genString = concat <$> listOf (frequency [(9, genPiece), (1, vectorOf 1 (choose ('\xD800', '\xDFFF')))])

-- | The laws of 'Measure': chunks are cut anywhere, and none of it may show.
measureLaws :: forall a. (Measure a, Eq a, Show a) => Proxy a -> Laws
measureLaws _ =
  Laws
    "Measure"
    [ ("Homomorphism", property $ \(Snippet x) (Snippet y) -> (measureChunk (x <> y) :: a) === measureChunk x <> measureChunk y)
    , ("Identity", (measureChunk T.empty :: a) === mempty)
    ]

-- | Measurements of some text, as they come up in a rope.
measured :: (Text -> a) -> Gen a
measured f = (\(Snippet t) -> f t) <$> arbitrary

instance Arbitrary Metrics where
  arbitrary = measured naiveMetrics

instance Arbitrary Breaks where
  arbitrary = measured measureChunk

instance Arbitrary Width where
  arbitrary = measured measureChunk

prop_plain :: Edited -> Unit -> Offset -> Snippet -> Property
prop_plain (Edited r t) u i (Snippet s) =
  conjoin
    [ Plain.toText plain === t
    , Plain.metrics plain === Rope.metrics r
    , Plain.toText (Plain.insert u k s plain) === Rope.toText (Rope.insert u k s r)
    , Plain.toText (fst (Plain.splitAt u k plain)) === Rope.toText (Rope.take u k r)
    , Plain.metricsWhere (\m -> Rope.count Chars m > k) plain === Rope.metricsAt Chars k r
    , holds (Plain.measured plain) t
    , Plain.fromText t === plain
    ]
  where
    k = resolve u i t
    plain = Plain.unmeasured r

-- | One big document, tall even in the real build, edited all over.
prop_big :: Property
prop_big = forAll (genText (6000 * sizeFactor)) $ \t0 ->
  forAll (vectorOf 30 arbitrary) $ \ops ->
    let r0 = Rope.fromText t0 :: R
     in counterexample ("height " ++ show (height r0)) $
          height r0 >= 2 .&&. prop_ops (Doc t0) ops

------------------------------------------------------------------------------
-- Chunk scans

-- | UTF-8 in an array of its own, for the scans to be held against a model:
-- every implementation of them (see 'kernels') gives the same results. Also
-- long lines, as line feeds found in the first vector looked at leave the
-- rest of a scan untried; and now and then a long run of one piece, which
-- fills the byte counters of the vector loops (255 vectors of 32 bytes, all
-- of them matching).
newtype Scanned = Scanned Text
  deriving (Show)

instance Arbitrary Scanned where
  arbitrary =
    Scanned
      <$> frequency
        [ (6, choose (0, 1100) >>= \n -> T.pack . concat <$> vectorOf n genPiece)
        , (3, choose (0, 1100) >>= \n -> T.pack . concat <$> vectorOf n (frequency [(1, pure "\n"), (60, filter (/= '\n') <$> genPiece)]))
        , (1, genPiece >>= \p -> choose (8000, 9000) >>= \n -> pure (T.pack (concat (replicate n p))))
        ]
  shrink (Scanned t) = Scanned <$> shrinkText t

bytesOfText :: Text -> ByteArray
bytesOfText (TI.Text (A.ByteArray ba) off len) = cloneByteArray (ByteArray ba) off len

byteList :: ByteArray -> [Word8]
byteList arr = [indexByteArray arr i | i <- [0 .. sizeofByteArray arr - 1]]

isContByte :: Word8 -> Bool
isContByte b = b >= 0x80 && b < 0xC0

-- | Every implementation gives what the model says.
allKernels :: (Eq b, Show b) => (Kernels -> b) -> b -> Property
allKernels run expected = conjoin [counterexample (kernelsName k) (run k === expected) | k <- kernels]

-- | A slice of an array of this size: often short, often long.
genSlice :: Int -> Gen (Int, Int)
genSlice size = do
  off <- frequency [(1, pure 0), (4, choose (0, size))]
  len <- frequency [(1, pure (size - off)), (2, choose (0, min 40 (size - off))), (2, choose (0, size - off))]
  pure (off, len)

prop_scanMetrics :: Scanned -> Property
prop_scanMetrics (Scanned t) = forAll (genSlice (sizeofByteArray arr)) $ \(off, len) ->
  let bs = L.take len (L.drop off (byteList arr))
      cs = len - L.length (filter isContByte bs)
   in allKernels
        (\k -> kernelMetrics k arr off len)
        (Metrics len cs (cs + L.length (filter (>= 0xF0) bs)) (L.length (filter (== 0x0A) bs)))
  where
    arr = bytesOfText t

prop_scanNewlines :: Scanned -> Property
prop_scanNewlines (Scanned t) = forAll (genSlice (sizeofByteArray arr)) $ \(off, len) ->
  allKernels (\k -> kernelNewlines k arr off len) (L.length (filter (== 0x0A) (L.take len (L.drop off (byteList arr)))))
  where
    arr = bytesOfText t

-- | Offsets of the line feeds.
lineFeeds :: ByteArray -> [Int]
lineFeeds arr = [i | (i, b) <- zip [0 ..] (byteList arr), b == 0x0A]

prop_scanNext :: Scanned -> Property
prop_scanNext (Scanned t) = forAll (choose (0, size)) $ \from ->
  allKernels (\k -> kernelFindNewline k arr from) (fromMaybe size (L.find (>= from) (lineFeeds arr)))
  where
    arr = bytesOfText t
    size = sizeofByteArray arr

prop_scanPrevious :: Scanned -> Property
prop_scanPrevious (Scanned t) = forAll (choose (0, sizeofByteArray arr)) $ \to ->
  allKernels (\k -> kernelFindNewlineBack k arr to) (last (-1 : [i | i <- lineFeeds arr, i < to]))
  where
    arr = bytesOfText t

prop_scanNth :: Scanned -> Property
prop_scanNth (Scanned t) = forAll (choose (1, L.length lfs + 2)) $ \n ->
  allKernels (\k -> kernelNthNewline k n arr) (case L.drop (n - 1) lfs of i : _ -> i + 1; [] -> sizeofByteArray arr)
  where
    arr = bytesOfText t
    lfs = lineFeeds arr

-- | Between two code point boundaries: the end of the longest run of whole
-- code points that fits into @k@ units, decoded rather than scanned.
prop_scanUnits :: Scanned -> Bool -> Property
prop_scanUnits (Scanned t) wide = forAll (genSlice (L.length bounds - 1)) $ \(i, n) ->
  let from = bounds !! i
      to = bounds !! (i + n)
      piece = T.unpack (takeWord8 (to - from) (dropWord8 from t))
      units c = if wide then utf16Len c else 1
      model k = go from 0 piece
        where
          go b _ [] = b
          go b u (c : cs)
            | u + units c > k = b
            | otherwise = go (b + utf8Len c) (u + units c) cs
   in forAll (choose (-1, sum (map units piece) + 2)) $ \k ->
        allKernels (\kn -> kernelScanUnits kn wide k arr from to) (model k)
  where
    arr = bytesOfText t
    bounds = scanl (+) 0 (map utf8Len (T.unpack t))
