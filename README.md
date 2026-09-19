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

* **Every unit at once.** Bytes, code points, UTF-16 code units and lines are
  tracked at every node. Any of them addresses the rope, and any converts to
  any other, in `O(log n)`.
* **Flat chunks in a B-tree.** Leaves are unpinned byte arrays of at most
  512 bytes of UTF-8. Inner nodes have up to 16 children. Every node carries
  the sizes of its subtree next to its header, so seeking reads the heads of
  a node's children, and an edit copies one small array of pointers per level.
* **Long lines are nothing special.** Chunks are cut by size, never by line.
* **Cheap keystrokes.** An edit within one chunk copies that chunk and the
  path to it, in one descent; a chunk that overflows splits in two. Typing is
  cheaper still: keystrokes that continue each other wait next to the tree,
  up to 128 bytes of them, and go into it at once when the rope is read.
* **Custom measures.** Cache a monoid of your own at every node and search by
  it.
* **Persistent and strict.** Old versions stay valid and share structure with
  new ones: undo is a list of ropes. The keystrokes that wait are the one
  lazy spot.

## Units

```haskell
data Unit = Bytes | Chars | Utf16 | Lines
```

Everything that takes an offset takes a `Unit`. The text is stored as UTF-8,
so `Bytes` is what tree-sitter, PCRE and FFI buffers speak. `Utf16` is what the
Language Server Protocol counts columns in by default; for a negotiated
`positionEncoding`, `"utf-8"` is `Bytes` and `"utf-32"` is `Chars`. Offsets are
clamped to the rope and rounded down to a code point boundary.

```haskell
Rope.splitAt Bytes 120 rope
Rope.take    Lines 10  rope
Rope.slice   Utf16 5 9 rope
Rope.convert Bytes Utf16 120 rope
```

`metricsAt` describes a location in every unit with one descent, which is all
tree-sitter wants to know about an edit that arrived as an LSP position:

```haskell
let m      = Rope.metricsAtPosition Utf16 lspPosition rope
    byte   = bytes m
    row    = newlines m
    column = posColumn (Rope.metricsToPosition Bytes m rope)
```

Positions clamp the way LSP asks: a column past the end of a line is the end
of its content, before the `\n` or `\r\n`. For parsers that pull their input
through a callback, `Rope.chunkAt Bytes i rope` is a zero-copy view of the text
at an offset.

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

Chunk boundaries are up to the rope, so a measure has to be a monoid
homomorphism: `measureChunk (x <> y) == measureChunk x <> measureChunk y`.
Pairs and triples of measures are measures, and `measured` / `unmeasured`
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

10,000 operations each on 3.4 MB of source text (GHC 9.14.1,
`cabal bench -f compare-text-rope`). Rows marked * run on a rope that has
already been through 10,000 edits, the others on a freshly loaded one.

| workload | nano-rope | text-rope 0.3 |
| --- | ---: | ---: |
| random inserts * | 5.2 ms | 20.6 ms |
| edits at UTF-16 positions | 9.1 ms | 42.0 ms |
| `getLine` * | 3.0 ms | 15.6 ms |
| keystrokes in one spot | 0.5 ms | 3.7 ms |
| the same, reading the line after each | 4.7 ms | 210 ms |
| `splitAt`, both halves * | 10.3 ms | 19.0 ms |
| `toText` (once) * | 0.27 ms | 0.49 ms |

Keystrokes in one spot are 100 bursts of 100. Left alone they wait next to the
tree; an editor that redraws the line after every one has them inserted every
time, which is the row below.

A freshly loaded `text-rope` is a single chunk. Its `toText` is free, which
no tree of chunks can match, and its first `getLine`s and splits walk the
whole text: 10,000 of either take 1.5 s, against 3 ms and 10 ms here. The
rows above stay clear of both.

## Development

```
cabal test     # properties against a Text model, also with 16-byte chunks
cabal bench
```
