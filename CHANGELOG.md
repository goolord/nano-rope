# Revision history for nano-rope

## 0.1.0.0 -- unreleased

* First version.
* `Data.Text.NanoRope`: a persistent B-tree rope over flat UTF-8 chunks,
  addressed in bytes, code points, UTF-16 code units or lines, with
  conversions between all of them, LSP-style positions, and editing
  (`insert`, `delete`, `replace`) with a fast path for edits that stay within
  one chunk.
* `Data.Text.NanoRope.Measured`: the same rope carrying a custom monoidal
  `Measure`, searchable with `splitWhere`.
* `Data.Text.NanoRope.Internal`: the representation and an invariant checker,
  without stability guarantees.
