# nano-rope

A persistent text rope for editors, language servers and parsers.

```haskell
import Data.Text.NanoRope (Rope, Unit (..), Position (..))
import qualified Data.Text.NanoRope as Rope

rope :: Rope
rope = Rope.fromText "let x = \"😀\"\nlet y = x\n"

Rope.length Utf16 rope                            -- 23
Rope.insert Chars 4 "xs@" rope
Rope.replace Lines 1 2 "let z = x\n" rope
Rope.splitAtPosition Utf16 (Position 1 4) rope    -- an LSP position
Rope.offsetToPosition Bytes Utf16 17 rope         -- Position 1 2
Rope.getLine 1 rope                               -- "let y = x"
```

* Bytes, code points, UTF-16 code units and lines are tracked at every node,
  so you can index by any of them and convert between them in `O(log n)`.
* Measuring chunks and searching them for line feeds, code points or UTF-16
  offsets is done with SIMD (SSE2, or AVX2 where the CPU has it) through a
  little C. Build with `-f -simd` for pure Haskell.
* A B-tree of flat UTF-8 chunks (up to 512 bytes, up to 16 children per node).
  Chunks are split by size, not by line, so one huge line is fine.
* An edit copies one chunk and the path to it. Consecutive keystrokes are
  buffered (up to 128 bytes) and flushed into the tree when you next read.
* Cache your own monoid at every node and search by it.
* Persistent: old versions stay valid and share structure, so undo is just a
  list of ropes. Strict, apart from the keystroke buffer.

## Units

```haskell
data Unit = Bytes | Chars | Utf16 | Lines
```

Anything that takes an offset takes a `Unit`. `Bytes` is what tree-sitter,
PCRE and FFI want; `Utf16` is LSP's default. If the client negotiates a
`positionEncoding`, `"utf-8"` is `Bytes` and `"utf-32"` is `Chars`. Offsets are
clamped to the rope and rounded down to a code point.

```haskell
Rope.splitAt Bytes 120 rope
Rope.take    Lines 10  rope
Rope.slice   Utf16 5 9 rope
Rope.convert Bytes Utf16 120 rope
```

`metricsAt` gives you a location in every unit at once, e.g. to turn an LSP
position into what tree-sitter needs:

```haskell
let m      = Rope.metricsAtPosition Utf16 lspPosition rope
    byte   = bytes m
    row    = newlines m
    column = posColumn (Rope.metricsToPosition Bytes m rope)
```

A column past the end of a line clamps to before its `\n` or `\r\n`, as LSP
expects. `Rope.chunkAt Bytes i rope` is a zero-copy view of the text at an
offset, for parsers that read through a callback.

`metricsAtLineAndPosition` also tells where the line starts, out of the same
descent. The difference of the two is the column in every unit, so this is
how a server turns a client's UTF-16 column into the code points GHC counts,
and notices a column that does not exist:

```haskell
let (line, at) = Rope.metricsAtLineAndPosition Utf16 lspPosition rope
    reached    = utf16Units at - utf16Units line   -- short of the column: it was clamped,
                                                   -- or fell inside a surrogate pair
    column     = chars at - chars line             -- the same column in code points
```

## Getting the text out

```haskell
Rope.writeFileUtf8 path rope                       -- or hPutUtf8 to a handle
Rope.foldlChunks' (\h chunk -> hash h chunk) h0 rope
Rope.toText rope
```

The chunks are UTF-8 already, so saving streams them through a 32 kB buffer:
the 4 MB document of the benchmarks allocates 98 kB on its way to a file,
and 8 MB by way of `toText`. It writes bytes, whatever the encoding and
newline mode of the handle. `foldlChunks'` walks the chunks as zero-copy
`Text`s and allocates nothing of its own (22 μs for that document, where
the lazy list of `toChunks` takes 83 μs and a megabyte). `toText` is for
when one `Text` is what you need: it costs a `memcpy` of the document.

## Custom measures

```haskell
import Data.Text.NanoRope.Measured (Rope, Measure (..))
import qualified Data.Text.NanoRope.Measured as Rope

newtype Width = Width Int deriving (Eq, Ord, Show)

instance Semigroup Width where Width a <> Width b = Width (a + b)
instance Monoid Width where mempty = Width 0

instance Measure Width where
  measureChunk = Width . T.foldl' (\n c -> n + wcwidth c) 0

Rope.measure rope :: Width                           -- O(1)
fst (Rope.splitWhere (\_ w -> w > Width 80) rope)    -- O(log n): what fits in 80 columns
```

The rope picks the chunk boundaries, so `measureChunk` has to be a monoid
homomorphism. Tuples of measures are measures. `measured` and `unmeasured`
convert to and from the plain rope without copying text.

## Complexity

| operation | cost |
| --- | --- |
| `length`, `metrics`, `measure`, `lineCount` | `O(1)` |
| `splitAt`, `take`, `drop`, `slice`, `<>` | `O(log n)` |
| `insert`, `delete`, `replace` | `O(log n + new text)` |
| `insert` where the last one ended, `delete` of what was just typed | `O(1)` |
| `metricsAt`, `convert`, positions, `splitWhere`, `chunkAt` | `O(log n)` |
| `getLine`, `sliceText` | `O(log n + result)`, zero-copy within a chunk |
| `fromText`, `toText` | `O(n)` |
| `foldlChunks'`, `toChunks`, `toLazyText`, `hPutUtf8` | `O(n)`, zero-copy but for the write buffer |

## Benchmarks

10,000 operations on 4 MB of source code, GHC 9.14.1. Rows marked * run on a
rope that has already had 10,000 random inserts.

| workload | nano-rope | text-rope 0.3 | yi-rope 0.11 | core-text 0.3.8 |
| --- | ---: | ---: | ---: | ---: |
| random inserts * | 4.0 ms | 22.1 ms | 99 ms | 140 ms |
| edits at UTF-16 positions | 5.8 ms | 45.1 ms | — | — |
| `getLine` * | 1.5 ms | 16.1 ms | 102 ms | — |
| typing, 100 bursts of 100 | 0.4 ms | 3.0 ms | 59 ms | 1.7 s |
| the same, reading the line after each key | 2.6 ms | 146 ms | 227 ms | — |
| `splitAt`, both halves * | 6.9 ms | 21.8 ms | 89 ms | 54 ms |
| `toText` (once) * | 0.21 ms | 0.49 ms | 2.3 ms | 4.0 ms |
| `fromText` (once) | 0.71 ms | 2.0 ms | 4.0 ms | 10 ns |
| `toText` (once), fresh rope | 0.20 ms | 52 ns | 1.2 ms | 93 ns |

yi-rope has no UTF-16, and core-text has neither UTF-16 nor lines.

nano-rope is far slower at loading and saving than core-text, and at saving
a fresh rope than text-rope. A freshly loaded text-rope or core-text is one
big chunk, and core-text is the `Text` it was loaded from, so they hand it
back as it is. The cost comes later: their first reads and splits walk the
whole text (10,000 `getLine`s on a fresh text-rope take 0.22 s), and core-text
re-measures the chunk on every edit next to it, hence its 1.7 s of typing.

### A language server

What `lsp` and `haskell-language-server` do with the rope of an open
document, call for call (`bench/Lsp.hs`): a module of 8,000 lines, 326 kB,
nearly all of it ASCII as source code is. A position and the start of its
line come out of one descent with `metricsAtLineAndPosition`; the last
column is the same work asking for them one at a time.

| workload | one descent | two |
| --- | ---: | ---: |
| typing, 5,130 keystrokes at UTF-16 positions | 1.4 ms | 2.3 ms |
| the same, reading the line under the cursor after each key | 1.7 ms | 2.9 ms |
| a rename, 199 edits at once, and `toText` | 0.11 ms | 0.13 ms |
| where 47,761 tokens start and end, and their text | 10.0 ms | 15.1 ms |
| 11,941 positions from UTF-16 to code points and back | 2.2 ms | 3.3 ms |
| three lines around each of those, as `Text` and as `lines` of a `slice` | 4.9 ms | |

### Memory

The same runs, by what they allocate, and the heap that stays live holding the
4.03 MB document once the `Text` it came from is dropped.

| | nano-rope | text-rope 0.3 | yi-rope 0.11 | core-text 0.3.8 |
| --- | ---: | ---: | ---: | ---: |
| allocated by random inserts * | 12.6 MB | 87.3 MB | 336 MB | 304 MB |
| allocated by edits at UTF-16 positions | 12.9 MB | 148 MB | — | — |
| allocated by `getLine` * | 0.37 MB | 78.9 MB | 260 MB | — |
| allocated by `splitAt`, both halves * | 26.6 MB | 107 MB | 185 MB | 160 MB |
| allocated by `fromText` (once) | 4.55 MB | 11.7 MB | 2.71 MB | 55 B |
| live heap, fresh rope | 4.55 MB | 5.08 MB | 4.56 MB | 4.03 MB |
| live heap after 10,000 random inserts | 4.60 MB | 11.6 MB | 7.90 MB | 4.62 MB |

An edit allocates the new chunk, a node and an array of pointers for each level
of the tree, and nothing else: about 1.3 kB per insert here. That is also what
an old version keeps alive, so it is the price of a step of undo. Looking
something up allocates nothing but the answer, and `fromText` allocates the
rope it builds and no garbage.

A fresh core-text is smaller because it is the `Text` it was loaded from, and
yi-rope allocates less to load than the text is long, so it does not copy all
of it either. nano-rope copies the text into chunks of its own. Of the 0.52
MB nano-rope adds to the text, each of the 7,900 chunks accounts for some 66
bytes: the array's header, a leaf of four words (its metrics share one of
them) and its share of the inner nodes.

![Time, allocation and live heap of nano-rope, text-rope, yi-rope and core-text](bench/results.svg)

The chart has everything, including allocation and memory use. To regenerate
it:

```
cabal bench -f compare-text-rope -f compare-yi-rope -f compare-core-text \
  --benchmark-options="--chart bench/results.svg"
```

The numbers are saved next to it in `bench/results.csv`; add `--redraw` to
redraw from those without re-running.

## Development

```
cabal test     # properties against a Text model and the instance laws,
               # also with 16-byte chunks
cabal test -f -simd   # the same without the C
cabal bench
```
