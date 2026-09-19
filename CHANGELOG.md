# Revision history for nano-rope

## 0.1.0.0 -- unreleased

* First version.
* `Data.Text.NanoRope`: a persistent B-tree rope over flat UTF-8 chunks,
  addressed in bytes, code points, UTF-16 code units or lines, with
  conversions between all of them, LSP-style positions, and editing
  (`insert`, `delete`, `replace`) in a single descent for edits that stay
  within one chunk.
* Typing is buffered: keystrokes that continue each other wait next to the
  tree and are inserted at once when the rope is next read, and erasing what
  was just typed takes them back. `length` and `metrics` stay `O(1)`.
* `splitAt` finds both halves in one descent.
* Chunks are scanned with SIMD instructions through C (SSE2, AVX2 when the
  CPU supports it, picked at run time); the `simd` flag turns this off.
* `Data.Text.NanoRope.Measured`: the same rope carrying a custom monoidal
  `Measure`, searchable with `splitWhere`.
* `Data.Text.NanoRope.Internal`: the representation and an invariant checker,
  without stability guarantees.
