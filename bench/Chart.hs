-- | Render benchmark timings, allocation, and retained heap as an SVG chart.
-- Timings and allocation come from tasty-bench's CSV output.
module Chart
  ( Chart (..)
  , Group (..)
  , Row (..)
  , Sample (..)
  , Footprint (..)
  , readSamples
  , render
  , showBytes
  , commas
  ) where

import Data.List (find, intercalate, nub, unfoldr)
import Numeric (showFFloat)

-- | One benchmark: a workload run on a library.
data Sample = Sample
  { sampleWorkload :: String
  , sampleLibrary :: String
  , sampleSeconds :: Double
  , sampleAllocated :: Maybe Double
  -- ^ Bytes allocated per run, when the RTS keeps statistics.
  }

-- | Retained heap for a document in a given state.
data Footprint = Footprint
  { footprintState :: String
  , footprintLibrary :: String
  , footprintBytes :: Double
  }

-- | One chart row with a label and (state, workload) pairs. State labels
-- are hidden when the row has only one run.
data Row = Row String [(String, String)]

data Group = Group String [Row]

data Chart = Chart
  { chartTitle :: String
  , chartSubtitle :: String
  , chartNotes :: [String]
  , chartLibraries :: [String]
  -- ^ All known libraries, with the subject first. Order keeps colours and
  -- shapes consistent even when only some libraries run.
  , chartGroups :: [Group]
  -- ^ Workload groups. Ungrouped results appear in an additional section.
  , chartSamples :: [Sample]
  , chartSkipped :: [(String, String)]
  -- ^ Workload/library pairs omitted because of run time.
  , chartTextHeap :: Maybe Double
  -- ^ The live heap of the document as one plain 'Data.Text.Text'.
  , chartFootprints :: [Footprint]
  }

------------------------------------------------------------------------------
-- Reading

-- | The rows of tasty-bench's CSV file, named @All.<group>.<workload>.<library>@.
readSamples :: String -> [Sample]
readSamples = concatMap sample . drop 1 . csvRows
  where
    sample (name : mean : _ : rest) =
      [ Sample
          { sampleWorkload = snd (breakLast path)
          , sampleLibrary = library
          , sampleSeconds = read mean / 1e12
          , sampleAllocated = case rest of
              allocated : _ -> Just (read allocated)
              [] -> Nothing
          }
      ]
      where
        (path, library) = breakLast name
    sample _ = []
    breakLast s = let (b, a) = break (== '.') (reverse s) in (reverse (drop 1 a), reverse b)

csvRows :: String -> [[String]]
csvRows [] = []
csvRows s = let (row, rest) = fields s in row : csvRows rest
  where
    fields xs = case field xs of
      (f, ',' : more) -> let (fs, r) = fields more in (f : fs, r)
      (f, '\r' : '\n' : more) -> ([f], more)
      (f, '\n' : more) -> ([f], more)
      (f, _) -> ([f], [])
    field ('"' : xs) = quoted xs
    field xs = break (`elem` ",\r\n") xs
    quoted ('"' : '"' : xs) = let (f, r) = quoted xs in ('"' : f, r)
    quoted ('"' : xs) = ([], xs)
    quoted (c : xs) = let (f, r) = quoted xs in (c : f, r)
    quoted [] = ([], [])

------------------------------------------------------------------------------
-- Layout

-- | Column positions for labels, plots, and relative performance factors.
width, margin, stateX, timeX0, timeX1, allocX0, allocX1, factorX :: Double
width = 900
margin = 20
stateX = 222
timeX0 = 236
timeX1 = 526
allocX0 = 552
allocX1 = 712
factorX = 812

-- | Rows of runs.
runH, rowPad :: Double
runH = 24
rowPad = 6

-- | A block's height and a renderer taking its top coordinate.
type Block = (Double, Double -> [String])

stack :: Double -> [Block] -> (Double, [String])
stack y [] = (y, [])
stack y ((h, draw) : blocks) = let (end, rest) = stack (y + h) blocks in (end, draw y ++ rest)

render :: Chart -> String
render c =
  concat $
    [ "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 " ++ num width ++ " " ++ num height
        ++ "\" width=\"" ++ num width ++ "\" height=\"" ++ num height
        ++ "\" role=\"img\" aria-label=\"" ++ escape (chartTitle c) ++ "\">"
    , tag "title" [] [escape (chartTitle c)]
    , tag "style" [] [style]
    , tag "rect" [("class", "bg"), ("width", "100%"), ("height", "100%"), ("rx", "10")] []
    ]
      ++ body
      ++ ["</svg>\n"]
  where
    (bottom, body) = stack 0 (header c : timePanel c ++ memoryPanel c ++ [notes c])
    height = bottom + 16

-- | Light and dark palettes. Distinct marker shapes provide an additional
-- way to identify libraries without relying on colour alone.
style :: String
style =
  concat
    [ "svg{font-family:system-ui,-apple-system,\"Segoe UI\",Roboto,sans-serif;"
    , "--surface:#fcfcfb;--ink:#0b0b0b;--ink2:#52514e;--muted:#898781;--grid:#e8e7e1;--axis:#c3c2b7;--rule:#d6d5ce;"
    , "--hover:rgba(11,11,11,.04);--s0:#2a78d6;--s1:#e87ba4;--s2:#eda100;--s3:#008300}"
    , "@media (prefers-color-scheme:dark){svg{"
    , "--surface:#1a1a19;--ink:#fff;--ink2:#c3c2b7;--muted:#898781;--grid:#262624;--axis:#44443f;--rule:#383835;"
    , "--hover:rgba(255,255,255,.05);--s0:#3987e5;--s1:#d55181;--s2:#c98500;--s3:#008300}}"
    , ".bg{fill:var(--surface)}"
    , "text{fill:var(--ink2);font-size:12.5px}"
    , ".t{fill:var(--ink);font-size:20px;font-weight:650;letter-spacing:-.01em}"
    , ".st{font-size:12.5px}"
    , ".gh{fill:var(--ink);font-size:13px;font-weight:650}"
    , ".ch{fill:var(--ink);font-size:11.5px;font-weight:600}"
    , ".rl{fill:var(--ink)}"
    , ".sl{fill:var(--muted);font-size:11px}"
    , ".k{fill:var(--muted);font-size:10.5px;font-variant-numeric:tabular-nums}"
    , ".q{fill:var(--ink);font-size:15px;font-weight:650;font-variant-numeric:tabular-nums}"
    , ".q.w{fill:var(--ink2);font-weight:500}"
    , ".qw{fill:var(--muted);font-size:11px;font-weight:400}"
    , ".n{fill:var(--muted);font-size:11.5px}"
    , ".g{stroke:var(--grid);stroke-width:1}"
    , ".a{stroke:var(--axis);stroke-width:1}"
    , ".ru{stroke:var(--rule);stroke-width:1}"
    , ".c{stroke:var(--axis);stroke-width:2;stroke-linecap:round}"
    , ".m{stroke:var(--surface);stroke-width:4;paint-order:stroke;stroke-linejoin:round}"
    , ".s0{fill:var(--s0)}.s1{fill:var(--s1)}.s2{fill:var(--s2)}.s3{fill:var(--s3)}"
    , ".o{stroke-width:1.5;paint-order:normal}"
    , ".o.s0{fill:var(--surface);stroke:var(--s0)}.o.s1{fill:var(--surface);stroke:var(--s1)}"
    , ".o.s2{fill:var(--surface);stroke:var(--s2)}.o.s3{fill:var(--surface);stroke:var(--s3)}"
    , ".hit{fill:transparent}.run:hover .hit{fill:var(--hover)}"
    ]

-- | Libraries present in the results, paired with their stable palette slot.
present :: Chart -> [(Int, String)]
present c =
  [ (i, l)
  | (i, l) <- zip [0 ..] (chartLibraries c)
  , l `elem` map sampleLibrary (chartSamples c) ++ map footprintLibrary (chartFootprints c)
  ]

subject :: Chart -> String
subject c = case chartLibraries c of
  l : _ -> l
  [] -> ""

header :: Chart -> Block
header c =
  ( 112
  , \y ->
      [ text margin (y + 40) [("class", "t")] (chartTitle c)
      , text margin (y + 62) [("class", "st")] (chartSubtitle c)
      ]
        ++ legend (y + 92)
  )
  where
    legend y = go margin (present c)
      where
        go _ [] = []
        go x ((i, l) : rest) =
          mark i (x + 6) (y - 4)
            : text (x + 18) y [("class", "rl")] l
            : go (x + 18 + 7.2 * fromIntegral (length l) + 26) rest

notes :: Chart -> Block
notes c = (28 + 17 * fromIntegral (length ls), \y -> rule y : [text margin (y + 30 + 17 * i) [("class", "n")] l | (i, l) <- zip [0 ..] ls])
  where
    ls = concatMap (wrapAt 118) (chartNotes c)

rule :: Double -> String
rule y = line margin y (width - margin) y "ru"

groupHeading :: String -> Block
groupHeading title = (38, \y -> [text margin (y + 27) [("class", "gh")] title])

------------------------------------------------------------------------------
-- Time and allocation

-- | Configured groups followed by any ungrouped workloads.
groups :: Chart -> [Group]
groups c = chartGroups c ++ [Group "Other" [Row w [("", w)] | w <- rest] | not (null rest)]
  where
    placed = [w | Group _ rows <- chartGroups c, Row _ runs <- rows, (_, w) <- runs]
    rest = filter (`notElem` placed) (nub (map sampleWorkload (chartSamples c)))

timePanel :: Chart -> [Block]
timePanel c
  | null samples = []
  | otherwise = [(headH + sum (map fst items) + footH, draw)]
  where
    samples = chartSamples c
    headH = 58
    footH = 30
    items =
      concat
        [ groupHeading title : map (timeRow c times bytes) rows
        | Group title all' <- groups c
        , let rows = filter (any (`elem` map sampleWorkload samples) . map snd . runs) all'
        , not (null rows)
        ]
    runs (Row _ rs) = rs
    times = decades (map sampleSeconds samples)
    allocs = [a | s <- samples, Just a <- [sampleAllocated s], a > 0]
    bytes = decades allocs

    draw y =
      [ rule (y + 4)
      , text timeX0 (y + 26) [("class", "ch")] "Time per run"
      , text factorX (y + 26) [("class", "ch"), ("text-anchor", "end")] (subject c ++ " against")
      , text factorX (y + 40) [("class", "ch"), ("text-anchor", "end")] "the fastest other"
      ]
        ++ logAxis times timeTick (timeX0, timeX1) (y + 46) (y + headH) bottom (bottom + 18)
        ++ ( if null allocs
               then []
               else
                 text allocX0 (y + 26) [("class", "ch")] "Allocated per run"
                   : logAxis bytes byteTick (allocX0, allocX1) (y + 46) (y + headH) bottom (bottom + 18)
           )
        ++ snd (stack (y + headH) items)
      where
        bottom = y + headH + sum (map fst items)

timeRow :: Chart -> (Int, Int) -> (Int, Int) -> Row -> Block
timeRow c times bytes (Row label runs) =
  (fromIntegral (length runs) * runH + rowPad, \y -> concat [drawRun (y + rowPad / 2 + runH * fromIntegral k) k r | (k, r) <- zip [0 ..] runs])
  where
    shown = length runs > 1
    drawRun y k (state, w) =
      [ "<g class=\"run\">"
      , tag "title" [] [escape tip]
      , hit y runH
      ]
        ++ [text margin (cy + 4) [("class", "rl")] label | k == (0 :: Int)]
        ++ [text stateX (cy + 4) [("class", "sl"), ("text-anchor", "end")] state | shown]
        ++ dots cy [(i, logX times (timeX0, timeX1) (sampleSeconds s)) | (i, s) <- got]
        ++ [mark' "m o" i (timeX1 - 1) cy | (i, l) <- present c, (w, l) `elem` chartSkipped c, i `notElem` map fst got]
        ++ factor cy "faster" "slower" [(sampleSeconds s, others) | (0, s) <- got]
        ++ dots cy [(i, logX bytes (allocX0, allocX1) a) | (i, s) <- got, Just a <- [sampleAllocated s], a > 0]
        ++ ["</g>"]
      where
        cy = y + runH / 2
        cells = [(i, l, find (\s -> sampleWorkload s == w && sampleLibrary s == l) (chartSamples c)) | (i, l) <- present c]
        got = [(i, s) | (i, _, Just s) <- cells]
        others = [sampleSeconds s | (i, s) <- got, i /= 0]
        tip =
          unlines $
            (label ++ (if null state then "" else ", " ++ state))
              : [ l ++ ": " ++ maybe (if (w, l) `elem` chartSkipped c then "omitted: run time" else "not benchmarked") describe s
                | (_, l, s) <- cells
                ]
        describe s = showSeconds (sampleSeconds s) ++ maybe "" (\a -> ", " ++ showBytes a ++ " allocated") (sampleAllocated s)

-- | Logarithmic gridlines with labels above and below the panel, spaced
-- to avoid overlapping text.
logAxis :: (Int, Int) -> (Int -> String) -> (Double, Double) -> Double -> Double -> Double -> Double -> [String]
logAxis scale@(lo, hi) label (x0, x1) topLabelY top bottom bottomLabelY =
  line x0 top x1 top "a"
    : line x0 bottom x1 bottom "a"
    : concat
      [ line x top x bottom "g"
          : concat
            [ [ text x topLabelY [("class", "k"), ("text-anchor", "middle")] (label k)
              , text x bottomLabelY [("class", "k"), ("text-anchor", "middle")] (label k)
              ]
            | k `mod` every == 0
            ]
      | k <- [lo .. hi]
      , let x = logX scale (x0, x1) (10 ^^ k)
      ]
  where
    perDecade = (x1 - x0) / fromIntegral (hi - lo)
    every = case [e | e <- [1, 2, 3, 6], perDecade * fromIntegral e >= 46] of
      e : _ -> e
      [] -> 6 :: Int

decades :: [Double] -> (Int, Int)
decades [] = (0, 1)
decades vs = (lo, max (lo + 1) (ceiling (logBase 10 (maximum vs))))
  where
    lo = floor (logBase 10 (minimum vs))

logX :: (Int, Int) -> (Double, Double) -> Double -> Double
logX (lo, hi) (x0, x1) v = x0 + (logBase 10 v - fromIntegral lo) / fromIntegral (hi - lo) * (x1 - x0)

timeTick, byteTick :: Int -> String
timeTick k = seconds num (10 ^^ k)
byteTick k = trimBytes (10 ^^ k)

------------------------------------------------------------------------------
-- Memory

memoryPanel :: Chart -> [Block]
memoryPanel c
  | null fps = []
  | otherwise = [groupHeading "Memory", (headH + rowH + footH, draw)]
  where
    fps = chartFootprints c
    headH = 46
    footH = maybe 8 (const 26) (chartTextHeap c)
    states = nub (map footprintState fps)
    rowH = fromIntegral (length states) * runH + rowPad
    top = niceTop (maximum (map footprintBytes fps ++ maybe [] pure (chartTextHeap c)))
    step = niceStep top
    x0 = timeX0
    x1 = allocX1
    x v = x0 + v / top * (x1 - x0)

    draw y =
      [ text factorX (y + 8) [("class", "ch"), ("text-anchor", "end")] (subject c ++ " against")
      , text factorX (y + 22) [("class", "ch"), ("text-anchor", "end")] "the smallest other"
      , line x0 (y + headH) x1 (y + headH) "a"
      , line x0 bottom x1 bottom "a"
      ]
        ++ concat
          [ [ line (x v) (y + headH) (x v) bottom "g"
            , text (x v) (y + headH - 8) [("class", "k"), ("text-anchor", "middle")] (trimBytes v)
            ]
          | v <- takeWhile (<= top * 1.0001) (iterate (+ step) 0)
          ]
        ++ reference
        ++ concat [drawRun (y + headH + rowPad / 2 + runH * fromIntegral k) k st | (k, st) <- zip [0 ..] states]
      where
        bottom = y + headH + rowH
        reference = case chartTextHeap c of
          Nothing -> []
          Just t ->
            [ line (x t) (y + headH) (x t) (bottom + 5) "c"
            , text (x t) (bottom + 19) [("class", "k"), ("text-anchor", "middle")] ("the Text alone, " ++ showBytes t)
            ]

    drawRun y k st =
      [ "<g class=\"run\">"
      , tag "title" [] [escape tip]
      , hit y runH
      ]
        ++ [text margin (cy + 4) [("class", "rl")] "Live heap" | k == (0 :: Int)]
        ++ [text stateX (cy + 4) [("class", "sl"), ("text-anchor", "end")] st | length states > 1]
        ++ dots cy [(i, x b) | (i, b) <- got]
        ++ factor cy "smaller" "larger" [(b, [o | (i, o) <- got, i /= 0]) | (0, b) <- got]
        ++ ["</g>"]
      where
        cy = y + runH / 2
        cells = [(i, l, find (\f -> footprintState f == st && footprintLibrary f == l) fps) | (i, l) <- present c]
        got = [(i, footprintBytes f) | (i, _, Just f) <- cells]
        tip =
          unlines $
            ("Live heap, " ++ st)
              : [l ++ ": " ++ maybe "not measured" (showBytes . footprintBytes) f | (_, l, f) <- cells]
              ++ maybe [] (\t -> ["the Text alone: " ++ showBytes t]) (chartTextHeap c)

niceTop :: Double -> Double
niceTop v = niceStep v * fromIntegral (ceiling (v / niceStep v) :: Int)

-- | A round step that cuts the range into at most six parts.
niceStep :: Double -> Double
niceStep v = case [s | s <- map (* 10 ^^ p) [1, 2, 2.5, 5], v / s <= 6] of
  s : _ -> s
  [] -> 10 ^^ (p + 1)
  where
    p = floor (logBase 10 v) - 1 :: Int

------------------------------------------------------------------------------
-- Marks

-- | Connect a run's smallest and largest values, then draw its markers.
-- Draw the subject last so it remains visible when markers overlap.
dots :: Double -> [(Int, Double)] -> [String]
dots _ [] = []
dots cy ms =
  [line (minimum xs) cy (maximum xs) cy "c" | length ms > 1]
    ++ [mark i x cy | (i, x) <- reverse ms]
  where
    xs = map snd ms

-- | A filled shape in the colour of a slot, ringed with the surface.
mark :: Int -> Double -> Double -> String
mark = mark' "m"

mark' :: String -> Int -> Double -> Double -> String
mark' cls0 i x y = case i `mod` 4 of
  0 -> tag "circle" [cls, ("cx", num x), ("cy", num y), ("r", "5.5")] []
  1 -> tag "rect" [cls, ("x", num (x - 4.75)), ("y", num (y - 4.75)), ("width", "9.5"), ("height", "9.5"), ("rx", "1.5")] []
  2 -> polygon [(x, y - 6.5), (x + 6.5, y), (x, y + 6.5), (x - 6.5, y)]
  _ -> polygon [(x, y - 6.5), (x + 6.5, y + 5), (x - 6.5, y + 5)]
  where
    cls = ("class", cls0 ++ " s" ++ show i)
    polygon ps = tag "polygon" [cls, ("points", unwords [num px ++ "," ++ num py | (px, py) <- ps])] []

-- | Ratio to the lowest comparison value, with differences below 5%
-- labelled as approximately equal.
factor :: Double -> String -> String -> [(Double, [Double])] -> [String]
factor cy better worse cmp = case cmp of
  [(mine, others@(_ : _))] ->
    let f = minimum others / mine
        (cls, n, s)
          | f >= 1.05 = ("q", times f, better)
          | f <= 1 / 1.05 = ("q w", times (1 / f), worse)
          | otherwise = ("q w", "1.0×", "on par")
     in [ text factorX (cy + 5) [("class", cls), ("text-anchor", "end")] n
        , text (factorX + 6) (cy + 4) [("class", "qw")] s
        ]
  [(_, [])] -> [text factorX (cy + 4) [("class", "qw"), ("text-anchor", "end")] "no comparison"]
  _ -> []
  where
    times x
      | x >= 10 = commas (round x) ++ "×"
      | otherwise = showFFloat (Just 1) x "×"

hit :: Double -> Double -> String
hit y h = tag "rect" [("class", "hit"), ("x", num (margin - 8)), ("y", num y), ("width", num (width - 2 * margin + 16)), ("height", num h), ("rx", "4")] []

-- | Wrap at word boundaries. A single word longer than the limit stays intact.
wrapAt :: Int -> String -> [String]
wrapAt n = go . words
  where
    go [] = []
    go (w : ws) = let (l, rest) = fill w ws in l : go rest
    fill l (w : ws) | length l + 1 + length w <= n = fill (l ++ " " ++ w) ws
    fill l ws = (l, ws)

------------------------------------------------------------------------------
-- SVG and numbers

tag :: String -> [(String, String)] -> [String] -> String
tag name attrs [] = "<" ++ name ++ attributes attrs ++ "/>"
tag name attrs body = "<" ++ name ++ attributes attrs ++ ">" ++ concat body ++ "</" ++ name ++ ">"

attributes :: [(String, String)] -> String
attributes = concatMap (\(k, v) -> ' ' : k ++ "=\"" ++ escape v ++ "\"")

text :: Double -> Double -> [(String, String)] -> String -> String
text x y attrs s = tag "text" (("x", num x) : ("y", num y) : attrs) [escape s]

line :: Double -> Double -> Double -> Double -> String -> String
line x1 y1 x2 y2 cls = tag "line" [("class", cls), ("x1", num x1), ("y1", num y1), ("x2", num x2), ("y2", num y2)] []

escape :: String -> String
escape = concatMap $ \ch -> case ch of
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  _ -> [ch]

num :: Double -> String
num v = let s = showFFloat (Just 1) v "" in if drop (length s - 2) s == ".0" then take (length s - 2) s else s

-- | Three significant digits.
sig :: Double -> String
sig v = showFFloat (Just (if v >= 100 then 0 else if v >= 10 then 1 else 2)) v ""

-- | Format a quantity in the largest supplied unit with a value of at
-- least one, falling back to the smallest unit. Units are powers of 1000.
scaled :: Int -> [String] -> (Double -> String) -> Double -> String
scaled e0 units shown v = last [shown (scale e) ++ u | (e, u) <- zip [e0, e0 + 3 ..] units, e == e0 || v >= 10 ^^ e]
  where
    -- Multiply or divide by an integer scale to avoid reciprocal rounding.
    scale e = if e < 0 then v * 10 ^^ negate e else v / 10 ^^ e

seconds :: (Double -> String) -> Double -> String
seconds = scaled (-9) [" ns", " µs", " ms", " s"]

showSeconds, showBytes, trimBytes :: Double -> String
showSeconds = seconds sig
showBytes = scaled 0 [" B", " kB", " MB", " GB", " TB"] sig
-- A round number of bytes, without trailing zeros.
trimBytes = scaled 0 [" B", " kB", " MB", " GB", " TB"] num

-- | With a comma every three digits.
commas :: Int -> String
commas = reverse . intercalate "," . unfoldr (\s -> if null s then Nothing else Just (splitAt 3 s)) . reverse . show
