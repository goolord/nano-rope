# nano-rope

A persistent UTF-8 rope for Haskell. Index and edit by bytes, Unicode code
points, UTF-16 units, or lines in logarithmic time. Versions share unchanged
text for undo and snapshots.

Uses a B-tree of chunks up to 512 bytes, buffered consecutive insertions,
zero-copy chunk reads, and optional custom monoidal summaries.

## Quick start

Requires GHC 9.4+. Add `nano-rope` and `text` to your Cabal `build-depends`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.NanoRope (Rope, Unit (..), Position (..))
import qualified Data.Text.NanoRope as Rope

document :: Rope
document = Rope.fromText "let x = \"😀\"\nlet y = x\n"

edited = Rope.replace Lines 1 2 "let z = x\n" document
-- document remains valid

line = Rope.getLine 1 edited                          -- "let z = x"
size = Rope.length Utf16 document                     -- 23
byte = Rope.convert Utf16 Bytes 11 document           -- 13
pos  = Rope.offsetToPosition Bytes Utf16 17 document   -- Position 1 2
```

## Coordinates

| Unit | Meaning |
| --- | --- |
| `Bytes` | UTF-8 bytes |
| `Chars` | Unicode code points, not graphemes or display columns |
| `Utf16` | UTF-16 code units; LSP's default position encoding |
| `Lines` | Line starts; `length Lines` counts `\n` characters |

- Offsets are zero-based and clamped. Offsets inside a code point round down.
- Ranges are half-open; both endpoints refer to the original rope. If `j <= i`,
  slicing is empty, deletion does nothing, and replacement inserts at `i`.
- `Position` is a zero-based line and column, with the column unit supplied
  separately. Columns clamp before `\n` or `\r\n`; lines past the document
  clamp to its end. Negative lines and columns clamp to zero. Only `\n` starts
  a new line.
- `getLine` strips the terminator and returns empty text for invalid indices.
  `lineCount` includes the final, possibly empty line. `lines` omits that empty
  trailing line and returns `[]` for an empty rope.

For multiple coordinates, reuse `metricsAtPosition`, or
`metricsAtLineAndPosition` for both line-start and position metrics. Subtract
their `bytes`, `chars`, or `utf16Units` fields to get columns.

## Reading and writing

- `chunkAt` returns a zero-copy view of the remaining chunk at an offset.
- `foldlChunks'` / `foldrChunks` traverse chunks; `toChunks` / `toLazyText`
  share their buffers. `toText` copies unless the rope fits in one chunk.
- `hPutUtf8` / `writeFileUtf8` stream UTF-8, preserving bytes and line endings
  regardless of handle encoding or newline translation.

## Custom measures

`Data.Text.NanoRope.Measured` provides `Rope m` with cached monoidal summaries.
Implement `Measure.measureChunk`, then use `measure` to read the summary or
`splitWhere` to search by it. Measures must obey:

```haskell
measureChunk (a <> b) == measureChunk a <> measureChunk b
measureChunk mempty   == mempty
```

Search predicates must stay true once true for a growing prefix. Pairs and
triples of measures are supported. `measured` / `unmeasured` in the plain
module rebuild annotations while sharing text buffers.

## Performance

For `n` document bytes and `k` inserted or returned bytes:

| Operation | Cost |
| --- | --- |
| `null`, `length`, `metrics`, `lineCount`; settled `measure` | `O(1)` |
| Tree edits, slices, lookups, coordinate conversions, searches | `O(log n)` |
| `insert`, `replace`, `getLine`, `sliceText` | `O(log n + k)` |
| Construction, flattening, chunk traversal, UTF-8 output | `O(n)` |

Bounds assume constant-time measure combination and search predicates, and
linear-time chunk measurement. Consecutive insertions can use a bounded
buffer; tree reads flush it, while `length` and `metrics` do not.

Scans use SSE2/AVX2 on supported x86-64 systems, portable C elsewhere, and
Haskell for short scans. Build with `-f -simd` for Haskell-only scans.

### Benchmarks

GHC 9.14.1, about 4 MB / 100,000 lines of generated source. A fresh rope retains
4.55 MB for 4.03 MB of text; after 10,000 random inserts, 4.60 MB. Retaining
older versions uses additional memory. Construction and flattening usually
copy the text.

![Timings, allocation, and live heap: nano-rope, text-rope, yi-rope, core-text](bench/results.svg)

[Raw results](bench/results.csv) · [Benchmark source](bench/Main.hs) ·
[Language-server workloads](bench/Lsp.hs) (edits, completion, tokens, positions).
Results vary with hardware, compiler, and document shape.

## Development

```sh
cabal test
cabal test -f -simd
cabal haddock
cabal bench
```

Regenerate the comparison chart and CSV:

```sh
cabal bench -f compare-text-rope -f compare-yi-rope -f compare-core-text --benchmark-options="--chart bench/results.svg"
```

Add `--redraw` inside `--benchmark-options` to reuse CSV timings and allocations;
live heap is measured again.
