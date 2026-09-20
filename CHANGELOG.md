# Changelog

## 0.1.0.0 — unreleased

Initial release.

- Persistent B-tree rope with UTF-8 chunks up to 512 bytes and structural
  sharing between versions.
- Indexing and conversion in bytes, code points, UTF-16 code units, and lines,
  plus zero-based line-and-column positions.
- Insert, delete, and replace operations, with a single-descent fast path for
  small edits and a bounded buffer for consecutive keystrokes.
- Cached metrics for constant-time length queries, including pending input.
- Line lookup, slicing, chunk folds, and zero-copy chunk views.
- Shared range descent for `slice` and `sliceText`, avoiding tree rebuilding
  above the lowest node containing the range.
- Combined line-start and position lookup with `metricsAtLineAndPosition`,
  for column conversion and detection of clamped or rounded positions.
- Unlifted tree nodes to avoid evaluation checks during traversal. Requires
  GHC 9.4 or later.
- Buffered UTF-8 output through `hPutUtf8` and `writeFileUtf8`.
- Runtime-selected SSE2 and AVX2 chunk scans on supported x86-64 systems,
  with portable C elsewhere and an optional Haskell-only build.
- Direct CPU feature detection without compiler-runtime symbols, supporting
  GHCi and Template Haskell linking on Windows.
- Custom monoidal measures and prefix searches in `Data.Text.NanoRope.Measured`.
- Representation, scan primitives, and invariant checks in
  `Data.Text.NanoRope.Internal`, without API stability guarantees.
