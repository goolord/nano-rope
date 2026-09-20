# nano-rope

A persistent UTF-8 text rope for Haskell, built for editors, language servers,
and parsers. Edit a document, look up a line, or convert between byte and UTF-16
offsets without scanning the whole text.

- **One rope, four units.** Index by bytes, Unicode code points, UTF-16 code
  units, or lines, with logarithmic-time lookups and conversions.
- **Small edits, small copies.** A B-tree of chunks up to 512 bytes shares
  unchanged text between versions. Keep old ropes for undo or snapshots.
- **Efficient typing.** Consecutive insertions can share a buffer of up to
  128 bytes, reducing tree updates during a run of keystrokes.
- **Predictable chunk sizes.** Chunks are split by size, so even a document
  with one long line has a balanced tree.
- **Chunk-based reads and output.** Read chunks without copying their text,
  or stream UTF-8 to a handle without flattening the document.
- **Custom summaries.** Cache a monoidal measure at each node and search by it.

## Quick start

Requires GHC 9.4 or later. Add `nano-rope` and `text` to your Cabal
`build-depends`. Import the API qualified and enable `OverloadedStrings`
for text literals:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.NanoRope (Rope, Unit (..), Position (..))
import qualified Data.Text.NanoRope as Rope

document :: Rope
document = Rope.fromText "let x = \"😀\"\nlet y = x\n"

edited :: Rope
edited = Rope.replace Lines 1 2 "let z = x\n" document
```

In GHCi:

```haskell
> Rope.length Utf16 document
23
> Rope.getLine 1 edited
"let z = x"
> Rope.offsetToPosition Bytes Utf16 17 document
Position {posLine = 1, posColumn = 2}
```

`document` is still valid after the edit. Both versions share the unchanged
parts of the tree.

## Offsets, ranges, and positions

Every offset has an explicit unit:

| Unit | Meaning |
| --- | --- |
| `Bytes` | UTF-8 bytes |
| `Chars` | Unicode code points, not grapheme clusters or display columns |
| `Utf16` | UTF-16 code units |
| `Lines` | Zero-based line starts; `length Lines` counts `\n` characters |

Offsets are zero-based and clamped to the document. An offset inside a UTF-8
sequence or UTF-16 surrogate pair rounds down to the start of that code point.
Ranges are half-open: `slice u i j` includes `i` and excludes `j`. Both endpoints
refer to the original rope. If `j <= i`, a slice is empty, a deletion does
nothing, and a replacement inserts at `i`.

```haskell
firstLine = Rope.take Lines 1 document
name      = Rope.sliceText Chars 4 5 document  -- "x"
byte      = Rope.convert Utf16 Bytes 11 document  -- 13
```

`Position` holds a zero-based line and column. The column's unit is supplied
separately. UTF-16 is the default position encoding in LSP; for negotiated
encodings, use `Bytes` for `"utf-8"` and `Chars` for `"utf-32"`.

Columns beyond a line's content clamp to before its `\n` or `\r\n`. Lines
beyond the document clamp to its end; negative lines and columns clamp to zero.
Only `\n` starts a new line. A lone `\r` is ordinary content.

To get several coordinates for one location, reuse its prefix metrics:

```haskell
location   = Rope.metricsAtPosition Utf16 (Position 1 4) document
byteOffset = Rope.bytes location
row        = Rope.newlines location
byteColumn = posColumn (Rope.metricsToPosition Bytes location document)
```

The metrics contain all four absolute offsets. Computing a column also looks
up the start of the line.

If you need columns in several units, `metricsAtLineAndPosition` retrieves
the line start and position together:

```haskell
(lineStart, at) = Rope.metricsAtLineAndPosition Utf16 (Position 1 4) document
reachedColumn  = Rope.utf16Units at - Rope.utf16Units lineStart  -- 4
codePointColumn = Rope.chars at - Rope.chars lineStart          -- 4
```

Compare the reached column with the requested one to detect clamping or
rounding inside a code point.

`getLine` strips the line terminator and returns empty text for an invalid
index. `lineCount` includes the final, possibly empty line, so an empty rope
has one line. `lines` returns `[]` for an empty rope and omits the empty line
after a trailing `\n`.

## Reading and writing

Choose the form your consumer needs:

- `chunkAt Bytes offset` returns the rest of the chunk at an offset as a
  zero-copy `Text` view. This is useful for parser read callbacks.
- `foldlChunks'` walks chunks with a strict accumulator, without building an
  intermediate list. `foldrChunks` provides a lazy right fold.
- `toChunks` and `toLazyText` share chunk buffers without copying text.
- `toText` creates one contiguous `Text`, copying the document unless it
  already fits in a single chunk.
- `hPutUtf8` and `writeFileUtf8` stream chunks through a 32 KiB buffer.

```haskell
save :: FilePath -> Rope -> IO ()
save = Rope.writeFileUtf8
```

UTF-8 output preserves the rope's bytes, including line endings. It bypasses
the handle's encoding and newline translation. Use text I/O with `toLazyText`
when you want the handle's encoding instead.

## Custom measures

`Data.Text.NanoRope.Measured` adds a type parameter for a cached summary. For
example, count tabs and find the prefix before the third one:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Text.NanoRope.Measured (Measure (..), Rope)
import qualified Data.Text.NanoRope.Measured as Rope

newtype Tabs = Tabs Int deriving (Eq, Ord, Show)

instance Semigroup Tabs where
  Tabs a <> Tabs b = Tabs (a + b)

instance Monoid Tabs where
  mempty = Tabs 0

instance Measure Tabs where
  measureChunk = Tabs . T.count "\t"

document :: Rope Tabs
document = Rope.fromText "a\tb\tc\td"

tabCount = Rope.measure document  -- Tabs 3
beforeThirdTab = Rope.toText (fst (Rope.splitWhere (\_ n -> n >= Tabs 3) document))
-- "a\tb\tc"
```

Chunk boundaries can change after an edit, so a measure must obey these laws:

```haskell
measureChunk (a <> b) == measureChunk a <> measureChunk b
measureChunk mempty   == mempty
```

Search predicates must be monotone: once true for a prefix, they must stay true
as it grows. Pairs and triples of measures are supported. The plain module's
`measured` and `unmeasured` functions rebuild annotations while sharing the
text buffers.

## Performance

For a document of `n` bytes and inserted or returned text of `k` bytes:

| Operation | Cost |
| --- | --- |
| `null`, `length`, `metrics`, `lineCount` | `O(1)` |
| `measure` on a settled rope | `O(1)` |
| `splitAt`, `take`, `drop`, `slice`, `append` | `O(log n)` |
| `insert`, `replace` | `O(log n + k)` |
| `delete` | `O(log n)` |
| `metricsAt`, `convert`, position conversions, `splitWhere`, `chunkAt` | `O(log n)` |
| `getLine`, `sliceText` | `O(log n + k)`; zero-copy within a chunk |
| `fromText`, `toText`, `lines`, UTF-8 output | `O(n)` |
| Chunk folds, `toChunks`, `toLazyText` | `O(n)` traversal, excluding consumer work |

These bounds assume constant-time combination of custom measures and linear-time
chunk measurement. Searches also assume a constant-time predicate.

An insertion that continues the previous one in the same unit (`Bytes`, `Chars`,
or `Utf16`) can be buffered while space remains. Updating this bounded buffer
is `O(1)` in document size. Deleting a suffix of buffered `Chars` input can use
the same fast path. Reading the tree, including `measure`, applies any pending
insertion first; `length` and `metrics` include it without forcing that update.

Chunk scans use SSE2 or AVX2 on supported x86-64 systems, with portable C
elsewhere. Short scans use Haskell. Build with `-f -simd` to use only Haskell
scans and omit the C code.

### Benchmarks

Selected results from the checked-in run on about 4 MB of generated source
text (100,000 lines), using GHC 9.14.1:

| Workload | Time |
| --- | ---: |
| 10,000 random inserts, after 10,000 prior inserts | 4.0 ms |
| 10,000 edits at UTF-16 positions | 5.8 ms |
| 10,000 line lookups, after 10,000 prior inserts | 1.5 ms |
| Typing: 100 bursts of 100 characters | 0.4 ms |
| Typing with a line read after every character | 2.6 ms |
| Constructing a rope with `fromText` | 0.71 ms |
| Converting an edited rope with `toText` | 0.21 ms |

The fresh rope retains about 4.55 MB for 4.03 MB of text, and about 4.60 MB after
10,000 random inserts. Retaining earlier versions for undo uses additional
memory for the chunks and tree paths those versions still reference.

nano-rope builds bounded chunks and their metrics up front. This gives fast
lookups from the first edit, at the cost of copying most input text during
construction. Converting back to a contiguous `Text` usually copies it again.

The [full chart](bench/results.svg) includes timings, allocation, live heap,
and comparisons with text-rope, yi-rope, and core-text. The
[raw results](bench/results.csv) and [benchmark source](bench/Main.hs) provide
the workload details. Results depend on hardware, compiler, and document shape;
fresh and edited ropes can have different costs.

### Language-server workloads

[Language-server benchmarks](bench/Lsp.hs) model document changes, completion
prefix reads, semantic tokens, and position conversions on an 8,000-line
module (326 kB). They compare the combined `metricsAtLineAndPosition` lookup
with separate line-start and position lookups within nano-rope.

| Workload | Combined lookup | Separate lookups |
| --- | ---: | ---: |
| Typing: 5,130 changes at UTF-16 positions | 1.4 ms | 2.3 ms |
| Typing with a completion-prefix read after each change | 1.7 ms | 2.9 ms |
| Rename: 199 edits followed by `toText` | 0.11 ms | 0.13 ms |
| Locate and read 47,761 tokens | 10.0 ms | 15.1 ms |
| Convert 11,941 positions to code points and back | 2.2 ms | 3.3 ms |

Reading three-line ranges around those positions takes 4.9 ms, using both
`sliceText` and `lines` of a `slice`. All results are in the same CSV.

## Development

```sh
cabal test
cabal test -f -simd
cabal haddock
cabal bench
```

The tests compare operations with a `Text` model, check tree invariants and
instance laws, and exercise each available scan implementation. They run with
both normal chunks and 16-byte chunks to exercise deeper trees on small inputs.

To regenerate the comparison chart and its CSV:

```sh
cabal bench -f compare-text-rope -f compare-yi-rope -f compare-core-text --benchmark-options="--chart bench/results.svg"
```

Add `--redraw` inside `--benchmark-options` to reuse the CSV timings and
allocation results. Live-heap measurements are collected again.
