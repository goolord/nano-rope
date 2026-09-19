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

## Benchmarks

10,000 operations on 4 MB of source code, GHC 9.14.1. Rows marked * run on a
rope that has already had 10,000 random inserts.

| workload | nano-rope | text-rope 0.3 | yi-rope 0.11 | core-text 0.3.8 |
| --- | ---: | ---: | ---: | ---: |
| random inserts * | 5.1 ms | 19.8 ms | 108 ms | 186 ms |
| edits at UTF-16 positions | 10.0 ms | 43.5 ms | — | — |
| `getLine` * | 2.9 ms | 15.6 ms | 113 ms | — |
| typing, 100 bursts of 100 | 0.5 ms | 3.8 ms | 65 ms | 4.5 s |
| the same, reading the line after each key | 4.8 ms | 210 ms | 260 ms | — |
| `splitAt`, both halves * | 11.5 ms | 20.8 ms | 98 ms | 81 ms |
| `toText` (once) * | 0.26 ms | 0.43 ms | 1.8 ms | 3.1 ms |

yi-rope has no UTF-16, and core-text has neither UTF-16 nor lines.

A freshly loaded text-rope or core-text is one big chunk. That makes `toText`
free, but the first reads and splits walk the whole text (10,000 `getLine`s on
text-rope take 1.5 s), and core-text re-measures the chunk on every edit next
to it, hence its 4.5 s of typing. Most rows above use an edited rope to keep
that out of the comparison.

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
cabal bench
```
