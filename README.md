# nano-rope

[![CI](https://github.com/goolord/nano-rope/actions/workflows/ci.yml/badge.svg)](https://github.com/goolord/nano-rope/actions/workflows/ci.yml)

A persistent UTF-8 rope for Haskell, designed for editors and language servers.
Depends only on `base` and `text`, with `O(log n)` indexing and editing across
bytes, Unicode code points, UTF-16 code units, and lines.

## Why `nano-rope` for HLS

`haskell-language-server` (via `lsp`'s VFS) currently uses `text-rope`
(`Data.Text.Utf16.Rope.Mixed`). `nano-rope` addresses its main bottlenecks:

- **One tree, four coordinate systems.** Every B-tree node caches UTF-8 bytes,
  code points, UTF-16 units, and newlines. `metricsAtLineAndPosition` converts
  between GHC code-point columns and LSP UTF-16 columns in a single `O(log n)`
  descent with no tree splitting or text allocation.
- **Cursor typing buffer.** Consecutive edits at the cursor stay in a small
  bounded buffer; `length`, `metrics`, `getLine`, and cursor-area lookups read
  it directly without flushing the tree.
- **Direct reads instead of split/concat.** `getLine`, `sliceText`, and
  `metricsAtLineAndPosition` read straight from the tree rather than splitting
  and rebuilding subtrees on every completion, token, or line query.
- **Compact edited heap.** Chunks stay bounded (≤512 bytes) and rebalance on
  edits, avoiding the fragment bloat `text-rope` accumulates during typing.
- **Portable and TH-safe.** Runtime SSE2/AVX2 dispatch on x86-64 (portable C
  elsewhere, pure Haskell via `-f -simd`), tested in CI on GHC 9.4–9.14 across
  Linux, macOS (ARM64), and Windows including Template Haskell splices.

### Measured in HLS (`rope-bench`)

Measured directly inside HLS ([`lsp`](https://github.com/goolord/lsp/tree/nano-rope)
and [`haskell-language-server`](https://github.com/goolord/haskell-language-server/tree/nano-rope)
`nano-rope` branches vs `text-rope` 0.3, GHC 9.14.1, AVX2, identical checksums
across all 60 checks): **6.04× faster (geomean)** across 48 workloads on 4
Haskell modules (`Config.hs`, `Logic.hs`, `CodeAction.hs`, `Ppr.hs`).

Representative results on `Ppr.hs` (320 kB GHC module):

| Workload (`Ppr.hs`) | `text-rope` | `nano-rope` | Speedup | Allocation |
| --- | ---: | ---: | ---: | ---: |
| Typing, edits alone | 11.3 ms | 681 µs | **16.6×** | 17.5× less |
| Typing, completion prefix | 15.2 ms | 1.84 ms | **8.3×** | 12.1× less |
| Rename (200 edits) | 7.71 ms | 130 µs | **59×** | 2.2× less |
| Semantic tokens (after typing) | 110 ms | 19.4 ms | **5.7×** | 10.9× less |
| Position conversions (fresh) | 298 ms | 1.23 ms | **243×** | 36× less |
| Position conversions (after typing) | 49.0 ms | 1.39 ms | **35×** | 185× less |
| Reading lines (after typing) | 23.7 ms | 2.47 ms | **9.6×** | 22.6× less |

**Retained heap (`--footprint`):**

| Target | Raw text | `text-rope` (edited) | `nano-rope` (edited) | Reduction |
| --- | ---: | ---: | ---: | ---: |
| `ghcide/src` (77 files) | 877 kB | 1.64 MB | 1.12 MB | **1.46× smaller** |
| `CodeAction.hs` | 97 kB | 287 kB | 121 kB | **2.36× smaller** |
| `Ppr.hs` | 320 kB | 571 kB | 377 kB | **1.51× smaller** |

## Benchmarks

Standalone suite on GHC 9.14.1 (AVX2): 100,000 generated lines (4.03 MB text)
plus 8,000-line language-server workloads. Fresh rope live heap is 4.55 MB;
after 10,000 random inserts, 4.60 MB (vs 5.08 MB / 11.6 MB for `text-rope`).

![Timings, allocation, and live heap: nano-rope, text-rope, yi-rope, core-text](bench/results.svg)

[Raw CSV](bench/results.csv) · [Benchmark harness](bench/Main.hs) ·
[LSP workloads](bench/Lsp.hs)

## Quick start

Requires GHC 9.4+. Add `nano-rope` and `text` to `build-depends`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.NanoRope (Metrics (..), Position (..), Rope, Unit (..))
import qualified Data.Text.NanoRope as Rope

doc :: Rope
doc = Rope.fromText "let x = \"😀\"\nlet y = x\n"

edited = Rope.replace Lines 1 2 "let z = x\n" doc  -- doc stays intact
line   = Rope.getLine 1 edited                     -- "let z = x"
size16 = Rope.length Utf16 doc                     -- 23

-- Line start and column in one O(log n) descent (e.g. UTF-16 <-> code points):
(lineStart, at) = Rope.metricsAtLineAndPosition Utf16 (Position 0 11) doc
charCol = chars at - chars lineStart               -- 10
byteOff = bytes at                                 -- 13
```

## API and semantics

| `Unit` | Meaning |
| --- | --- |
| `Bytes` | UTF-8 bytes |
| `Chars` | Unicode code points (not graphemes) |
| `Utf16` | UTF-16 code units (LSP default) |
| `Lines` | Zero-based line starts (`\n`-delimited) |

- **Offsets & ranges:** zero-based, clamped to bounds; offsets inside a code
  point round down. Ranges are half-open `[i, j)` in the original rope (`j <= i`
  slices empty, deletes nothing, replaces as insert at `i`).
- **Positions & lines:** `Position line col` clamps columns before `\n` /
  `\r\n` and lines past EOF to the end. `getLine` strips the terminator;
  `lineCount` includes a trailing empty line, while `lines` omits it.
- **Zero-copy reads & I/O:** `chunkAt`, `foldlChunks'`, `foldrChunks`,
  `toChunks`, and `toLazyText` share underlying buffers. `hPutUtf8` and
  `writeFileUtf8` stream raw UTF-8 bytes without handle encoding or newline
  translation.
- **Custom measures:** `Data.Text.NanoRope.Measured` caches any monoidal
  `Measure` per node (`measure`, monotone `splitWhere`, pairs/triples).
  `measured` and `unmeasured` convert while sharing chunk buffers.

## Complexity

For document size `n` and inserted/returned size `k` (assuming `O(1)` measure
combining):

| Operation | Cost |
| --- | --- |
| `null`, `length`, `metrics`, `lineCount`, settled `measure` | `O(1)` |
| Tree edits, splits, slices, lookups, coordinate conversions | `O(log n)` |
| `insert`, `replace`, `getLine`, `sliceText` | `O(log n + k)` |
| `fromText`, `toText`, chunk folds, UTF-8 output | `O(n)` |

## Development

```sh
cabal test
cabal test -f -simd
cabal haddock
cabal bench
```

Regenerate the comparison chart and CSV:

```sh
cabal bench -f compare-text-rope -f compare-yi-rope -f compare-core-text \
  --benchmark-options="--chart bench/results.svg"
```

Add `--redraw` inside `--benchmark-options` to re-render the SVG from the
existing CSV (live heap is re-measured).
