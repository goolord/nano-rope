# Changelog

## 0.1.0.0 (unreleased)

Initial release.

- Persistent UTF-8 B-tree rope (chunks up to 512 bytes, up to 16 children per
  node) with structural sharing across versions. Requires GHC 9.4+.
- `O(log n)` indexing, slicing, and conversion in bytes, code points, UTF-16
  code units, and lines, plus zero-based line/column positions and combined
  line-start and position lookup (`metricsAtLineAndPosition`).
- Local single-descent edits and a bounded cursor buffer for consecutive
  insertions, suffix erasures, and cursor-area reads.
- Zero-copy chunk views, direct chunk folds, and buffered UTF-8 output
  (`hPutUtf8`, `writeFileUtf8`).
- Runtime-selected SSE2 and AVX2 chunk scans on x86-64, portable C elsewhere,
  and an optional Haskell-only build (`-f -simd`).
- Custom monoidal measures and prefix searches in `Data.Text.NanoRope.Measured`.
- Internals, scan kernels, and invariant checks exposed in
  `Data.Text.NanoRope.Internal` (no API stability guarantees).
