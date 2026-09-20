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
* `metricsAtLineAndPosition` finds a position and the start of its line in
  one descent. Their difference is the column that was reached in every unit,
  which is what a language server converts columns with, and how it tells a
  column that was clamped or rounded from one that was not.
* Nodes are unlifted (`UnliftedDatatypes`), so that the compiler knows the
  children of a node for evaluated: seeking reads them out of their array
  without the evaluation, and the saving and restoring of registers around
  it, that an element of a lifted array costs whether it needs it or not.
  Every descent is 10 to 25% faster for it. Needs GHC 9.4.
* Allocation is kept to what persistence needs: an edit allocates the new
  chunk, a node and an array of pointers per level, and nothing else; looking
  something up allocates nothing but the answer; loading a text allocates the
  rope and no garbage. A leaf keeps its four metrics in one word.
* Getting the text out without copying it: `hPutUtf8` and `writeFileUtf8`
  stream the chunks to a handle through one small buffer, `foldlChunks'` is a
  strict fold over them that allocates nothing, next to the lazy
  `foldrChunks`, `toChunks` and `toLazyText`. `toText` costs a `memcpy` of the
  document.
* Chunks are scanned with SIMD instructions through C (SSE2, AVX2 when the
  CPU supports it, picked at run time); the `simd` flag turns this off. The
  C asks the CPU what it supports with `CPUID` itself and counts bits without
  the compiler's runtime, so that it links wherever GHC's own linker has to
  load it: Template Haskell on Windows, for one.
* `Data.Text.NanoRope.Measured`: the same rope carrying a custom monoidal
  `Measure`, searchable with `splitWhere`.
* `Data.Text.NanoRope.Internal`: the representation and an invariant checker,
  without stability guarantees.
