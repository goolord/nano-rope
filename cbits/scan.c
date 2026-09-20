/*
 * Scanning the chunks of a rope with SIMD instructions.
 *
 * Every function here reads bytes of UTF-8 (or any bytes, for the counts) and
 * nothing else: no allocation, no state but the choice of instruction set.
 * They are called from Data.Text.NanoRope.Internal through unsafe foreign
 * calls, which hand over the payload of an unpinned byte array directly.
 * Slices shorter than 32 bytes are left to the Haskell, 8 bytes at a time.
 *
 * Three levels, all computing exactly the same results:
 *
 *   0  portable C, which compilers are free to vectorise;
 *   1  SSE2, 16 bytes at a time (every x86-64 has it);
 *   2  AVX2, 32 bytes at a time, if the CPU and the OS support it.
 *
 * Every exported function takes the level to run at, no higher than
 * nano_rope_simd_level(): the Haskell asks for that once, and the test suite
 * holds every level to the same results.
 *
 * A vector loop leaves a tail shorter than a vector. As long as the whole
 * range is at least a vector long, the tail is read as the last vector of
 * the range, overlapping bytes already seen, which are masked off. Nothing is
 * ever read outside the range asked about, except before it for the
 * functions that are handed the size of the whole array.
 */

#ifndef LEVEL

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "HsFFI.h"

#if defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__))
#define NR_X86 1
#include <cpuid.h>
#include <immintrin.h>
#else
#define NR_X86 0
#endif

typedef const uint8_t *bytes;

/* The three counts of a metrics scan in one word, 21 bits each. The caller
 * never asks about 2^21 bytes or more at once. */
#define PACK(conts, fours, nls) \
  ((HsWord64)(conts) | (HsWord64)(fours) << 21 | (HsWord64)(nls) << 42)

/* ------------------------------------------------------------------------
 * Level 0: portable C. They also handle what is shorter than a vector, and
 * scan_units_c finishes off the vector a unit count falls in.
 */

static HsWord64 metrics_c(bytes s, size_t n)
{
  size_t conts = 0, fours = 0, nls = 0;
  for (size_t i = 0; i < n; i++) {
    uint8_t b = s[i];
    conts += (b & 0xC0) == 0x80;
    fours += b >= 0xF0;
    nls += b == '\n';
  }
  return PACK(conts, fours, nls);
}

static HsInt newlines_c(bytes s, size_t n)
{
  size_t nls = 0;
  for (size_t i = 0; i < n; i++)
    nls += s[i] == '\n';
  return (HsInt)nls;
}

/* The first '\n' at or after i, or n. */
static HsInt find_newline_c(bytes s, size_t i, size_t n)
{
  const uint8_t *p = i < n ? memchr(s + i, '\n', n - i) : NULL;
  return p ? (HsInt)(p - s) : (HsInt)n;
}

/* The last '\n' before i, or -1. */
static HsInt find_newline_back_c(bytes s, size_t i)
{
  while (i > 0)
    if (s[--i] == '\n')
      return (HsInt)i;
  return -1;
}

/* Just after the k-th '\n' (k >= 1) from i on, or n. */
static HsInt nth_newline_c(bytes s, size_t i, size_t n, HsInt k)
{
  for (; i < n; i++)
    if (s[i] == '\n' && --k == 0)
      return (HsInt)(i + 1);
  return (HsInt)n;
}

/* From the code point boundary i, the offset in front of the first code
 * point that does not fit into k units, with u units counted already, or
 * `to`. A unit is a code point, or a UTF-16 code unit if wide. */
static HsInt scan_units_c(bytes s, size_t i, size_t to, HsInt k, HsInt u, int wide)
{
  for (; i < to; i++) {
    uint8_t b = s[i];
    if ((b & 0xC0) == 0x80)
      continue;
    HsInt w = wide && b >= 0xF0 ? 2 : 1;
    if (u + w > k)
      return (HsInt)i;
    u += w;
  }
  return (HsInt)to;
}

#if NR_X86

/* A byte counter goes up by one per vector, so it has to be emptied into
 * the wide sums at least every 255 vectors. */
#define FLUSH 255

/* Bits set, without the POPCNT instruction, which not every x86-64 has.
 * Spelled out: left to __builtin_popcount this is a call into the runtime of
 * the compiler, one more symbol for a linker to go looking for. */
static inline uint32_t popcount_sse2(uint32_t m)
{
  m = m - ((m >> 1) & 0x55555555u);
  m = (m & 0x33333333u) + ((m >> 2) & 0x33333333u);
  return (((m + (m >> 4)) & 0x0F0F0F0Fu) * 0x01010101u) >> 24;
}

/* Bits set, with the instruction, which every CPU with AVX2 has. */
#define AVX2 __attribute__((target("avx2,popcnt")))

AVX2 static inline uint32_t popcount_avx2(uint32_t m)
{
  return (uint32_t)__builtin_popcount(m);
}

/* Position of the k-th (1-based) set bit, for k <= popcount m. */
static inline uint32_t nth_bit(uint32_t m, HsInt k)
{
  while (--k > 0)
    m &= m - 1;
  return (uint32_t)__builtin_ctz(m);
}

#define NR_CAT_(a, b) a##b
#define NR_CAT(a, b) NR_CAT_(a, b)

/* ------------------------------------------------------------------------
 * Level 1: SSE2, and level 2: AVX2, the same twice as wide. Both are the
 * second half of this file, which includes itself once for each; whatever is
 * shorter than a vector goes a level down.
 */

static inline HsWord64 sum128(__m128i v)
{
  return (HsWord64)_mm_cvtsi128_si64(v) + (HsWord64)_mm_cvtsi128_si64(_mm_unpackhi_epi64(v, v));
}

#define LEVEL sse2
#define LOWER c
#define ATTR
#define W 16
#define V __m128i
#define MM(op) _mm_##op
#define LOAD(p) _mm_loadu_si128((const __m128i *)(p))
#define ZERO _mm_setzero_si128()
#define SUM(v) sum128(v)
#include "scan.c"

AVX2 static inline HsWord64 sum256(__m256i v)
{
  __m128i w = _mm_add_epi64(_mm256_castsi256_si128(v), _mm256_extracti128_si256(v, 1));
  return (HsWord64)_mm_cvtsi128_si64(w) + (HsWord64)_mm_extract_epi64(w, 1);
}

#define LEVEL avx2
#define LOWER sse2
#define ATTR AVX2
#define W 32
#define V __m256i
#define MM(op) _mm256_##op
#define LOAD(p) _mm256_loadu_si256((const __m256i *)(p))
#define ZERO _mm256_setzero_si256()
#define SUM(v) sum256(v)
#include "scan.c"

#endif /* NR_X86 */

/* ------------------------------------------------------------------------
 * The exported functions, one per scan, at a given level no higher than
 * nano_rope_simd_level(): Haskell asks for that once and passes it along.
 */

#if NR_X86
/* Does the CPU have AVX2, and does the OS save the registers it needs?
 *
 * Asked of the CPU itself. __builtin_cpu_supports("avx2") says the same, but
 * reads what __cpu_indicator_init of the compiler's runtime leaves behind,
 * and that is a symbol the linker of GHCi does not resolve on Windows: there
 * went Template Haskell in everything that depends on this package. */
static int has_avx2(void)
{
  unsigned int a, b, c, d;
  if (__get_cpuid_max(0, NULL) < 7)
    return 0;
  __cpuid_count(1, 0, a, b, c, d);
  /* OSXSAVE, so that XGETBV may be asked, and AVX. */
  if ((c & (1u << 27)) == 0 || (c & (1u << 28)) == 0)
    return 0;
  /* XCR0: the state of the XMM and of the YMM registers is kept. */
  unsigned int lo, hi;
  __asm__ volatile("xgetbv" : "=a"(lo), "=d"(hi) : "c"(0));
  (void)hi;
  if ((lo & 6) != 6)
    return 0;
  __cpuid_count(7, 0, a, b, c, d);
  return (b & (1u << 5)) != 0;
}
#endif

HsInt nano_rope_simd_level(void)
{
#if NR_X86
  return has_avx2() ? 2 : 1;
#else
  return 0;
#endif
}

#if NR_X86
#define DISPATCH(level, name, ...)           \
  switch (level) {                            \
  case 2: return name##_avx2(__VA_ARGS__);    \
  case 1: return name##_sse2(__VA_ARGS__);    \
  default: return name##_c(__VA_ARGS__);      \
  }
#else
#define DISPATCH(level, name, ...) \
  (void)(level);                   \
  return name##_c(__VA_ARGS__);
#endif

/* Continuation bytes, 4-byte leaders and '\n' in s[off .. off+len), packed
 * by PACK. len < 2^21. */
HsWord64 nano_rope_metrics(HsInt level, bytes s, HsInt off, HsInt len)
{
  DISPATCH(level, metrics, s + off, (size_t)len)
}

/* Number of '\n' in s[off .. off+len). */
HsInt nano_rope_newlines(HsInt level, bytes s, HsInt off, HsInt len)
{
  DISPATCH(level, newlines, s + off, (size_t)len)
}

/* The first '\n' in s[from .. n), or n; from <= n. */
HsInt nano_rope_find_newline(HsInt level, bytes s, HsInt from, HsInt n)
{
  DISPATCH(level, find_newline, s, (size_t)from, (size_t)n)
}

/* The last '\n' in s[0 .. to), or -1. */
HsInt nano_rope_find_newline_back(HsInt level, bytes s, HsInt to)
{
  DISPATCH(level, find_newline_back, s, (size_t)to)
}

/* The offset just after the k-th '\n' in s[0 .. n) for k >= 1, or n. */
HsInt nano_rope_nth_newline(HsInt level, bytes s, HsInt n, HsInt k)
{
  if (k <= 0)
    return n;
  DISPATCH(level, nth_newline, s, 0, (size_t)n, k)
}

/* A line of s[0 .. n): the offset just after the k-th '\n', or 0 for k <= 0,
 * and that of the first '\n' at or after it, or n. The former in the low 32
 * bits, the latter in the high ones. Whoever wants a line wants both, and
 * here they are one call. */
HsWord64 nano_rope_line_span(HsInt level, bytes s, HsInt n, HsInt k)
{
  HsInt from = k <= 0 ? 0 : nano_rope_nth_newline(level, s, n, k);
  HsInt lf = nano_rope_find_newline(level, s, from, n);
  return (HsWord64)from | (HsWord64)lf << 32;
}

/* Walking over the code points of s[from .. to) from the boundary `from`,
 * the offset in front of the first one that does not fit into k units, or
 * `to`. Units are code points, or UTF-16 code units if wide. */
HsInt nano_rope_scan_units(HsInt level, bytes s, HsInt from, HsInt to, HsInt k, HsInt wide)
{
  /* Nothing fits into no units, and the vector loops count on k >= 0. */
  if (k <= 0)
    return from < to ? from : to;
  DISPATCH(level, scan_units, s, (size_t)from, (size_t)to, k, 0, (int)wide)
}

#else
/* ------------------------------------------------------------------------
 * One level of SIMD: the scans over vectors of W bytes. The file includes
 * itself here once per level, having said what a vector is:
 *
 *   LEVEL     the suffix of the functions of this level
 *   LOWER     that of the level taking whatever is shorter than a vector
 *   ATTR      the attributes of a function of this level
 *   W         the bytes in a vector, V its type, MM(op) its intrinsics
 *   LOAD(p)   the vector at p, unaligned; ZERO, the one of zeros
 *   SUM(v)    the sum of the 64-bit lanes of a vector
 *
 * and popcount_LEVEL, the bits set in a word.
 */

#define FN(name) NR_CAT(name##_, LEVEL)
#define LO(name) NR_CAT(name##_, LOWER)
/* Every lane of a vector, as movemask bits. */
#define ALL ((uint32_t)(((uint64_t)1 << W) - 1))

/* Continuation bytes 0x80 .. 0xBF are exactly the signed bytes below -64;
 * leaders of 4-byte sequences are the bytes whose unsigned maximum with 0xF0
 * is themselves. */
ATTR static inline V FN(is_cont)(V x)
{
  return MM(cmpgt_epi8)(MM(set1_epi8)((char)0xC0), x);
}

ATTR static inline V FN(is_four)(V x)
{
  return MM(cmpeq_epi8)(MM(max_epu8)(x, MM(set1_epi8)((char)0xF0)), x);
}

ATTR static inline V FN(is_newline)(V x)
{
  return MM(cmpeq_epi8)(x, MM(set1_epi8)('\n'));
}

ATTR static inline uint32_t FN(bits)(V m)
{
  return (uint32_t)MM(movemask_epi8)(m);
}

ATTR static inline uint32_t FN(newline_mask)(bytes p)
{
  return FN(bits)(FN(is_newline)(LOAD(p)));
}

/* The counts of a metrics scan in the last t lanes of a vector. */
ATTR static inline HsWord64 FN(pack_tail)(uint32_t conts, uint32_t fours, uint32_t nls, size_t t)
{
  int shift = W - (int)t;
  return PACK(FN(popcount)(conts >> shift), FN(popcount)(fours >> shift), FN(popcount)(nls >> shift));
}

/* Units in the lanes set in `lanes`, out of the continuation and 4-byte
 * leader bits of a vector. */
ATTR static inline HsInt FN(units)(uint32_t lanes, uint32_t conts, uint32_t fours, int wide)
{
  return (HsInt)FN(popcount)(lanes & ~conts) + (wide ? (HsInt)FN(popcount)(lanes & fours) : 0);
}

/* The lane of the first code point that does not fit into k units, with
 * u <= k of them counted already, in a vector whose `lanes` hold more than
 * the rest. Read off the bits: where every unit is a code point it is the
 * leader after those that fit. */
ATTR static inline uint32_t FN(units_stop)(uint32_t lanes, uint32_t conts, uint32_t fours, HsInt k, HsInt u,
                                           int wide)
{
  uint32_t leaders = lanes & ~conts;
  if (!wide || (fours & lanes) == 0)
    return nth_bit(leaders, k - u + 1);
  for (;;) {
    uint32_t lane = (uint32_t)__builtin_ctz(leaders);
    HsInt w = (fours >> lane) & 1 ? 2 : 1;
    if (u + w > k)
      return lane;
    u += w;
    leaders &= leaders - 1;
  }
}

ATTR static HsWord64 FN(metrics)(bytes s, size_t n)
{
  if (n < W)
    return LO(metrics)(s, n);
  const V zero = ZERO;
  V sum = zero; /* packed as by PACK, in each 64-bit lane */
  size_t i = 0;
  while (n - i >= W) {
    size_t v = (n - i) / W;
    if (v > FLUSH)
      v = FLUSH;
    V ac = zero, af = zero, an = zero;
    for (; v > 0; v--, i += W) {
      V x = LOAD(s + i);
      ac = MM(sub_epi8)(ac, FN(is_cont)(x));
      af = MM(sub_epi8)(af, FN(is_four)(x));
      an = MM(sub_epi8)(an, FN(is_newline)(x));
    }
    sum = MM(add_epi64)(sum, MM(sad_epu8)(ac, zero));
    sum = MM(add_epi64)(sum, MM(slli_epi64)(MM(sad_epu8)(af, zero), 21));
    sum = MM(add_epi64)(sum, MM(slli_epi64)(MM(sad_epu8)(an, zero), 42));
  }
  HsWord64 packed = SUM(sum);
  if (i < n) {
    V x = LOAD(s + n - W);
    packed += FN(pack_tail)(FN(bits)(FN(is_cont)(x)), FN(bits)(FN(is_four)(x)), FN(bits)(FN(is_newline)(x)), n - i);
  }
  return packed;
}

ATTR static HsInt FN(newlines)(bytes s, size_t n)
{
  if (n < W)
    return LO(newlines)(s, n);
  const V zero = ZERO;
  V sn = zero;
  size_t i = 0;
  while (n - i >= W) {
    size_t v = (n - i) / W;
    if (v > FLUSH)
      v = FLUSH;
    V an = zero;
    for (; v > 0; v--, i += W)
      an = MM(sub_epi8)(an, FN(is_newline)(LOAD(s + i)));
    sn = MM(add_epi64)(sn, MM(sad_epu8)(an, zero));
  }
  HsInt nls = (HsInt)SUM(sn);
  if (i < n)
    nls += FN(popcount)(FN(newline_mask)(s + n - W) >> (W - (n - i)));
  return nls;
}

ATTR static HsInt FN(find_newline)(bytes s, size_t i, size_t n)
{
  if (n < W)
    return LO(find_newline)(s, i, n);
  for (; n - i >= W; i += W) {
    uint32_t m = FN(newline_mask)(s + i);
    if (m)
      return (HsInt)(i + __builtin_ctz(m));
  }
  if (i < n) {
    uint32_t m = FN(newline_mask)(s + n - W) >> (W - (n - i));
    if (m)
      return (HsInt)(i + __builtin_ctz(m));
  }
  return (HsInt)n;
}

ATTR static HsInt FN(find_newline_back)(bytes s, size_t i)
{
  if (i < W)
    return LO(find_newline_back)(s, i);
  for (; i >= W; i -= W) {
    uint32_t m = FN(newline_mask)(s + i - W);
    if (m)
      return (HsInt)(i - W + 31 - __builtin_clz(m));
  }
  if (i > 0) {
    uint32_t m = FN(newline_mask)(s) & ((1u << i) - 1);
    if (m)
      return (HsInt)(31 - __builtin_clz(m));
  }
  return -1;
}

ATTR static HsInt FN(nth_newline)(bytes s, size_t i, size_t n, HsInt k)
{
  if (n < W)
    return LO(nth_newline)(s, i, n, k);
  for (; n - i >= W; i += W) {
    uint32_t m = FN(newline_mask)(s + i);
    HsInt c = FN(popcount)(m);
    if (c >= k)
      return (HsInt)(i + nth_bit(m, k) + 1);
    k -= c;
  }
  if (i < n) {
    uint32_t m = FN(newline_mask)(s + n - W) >> (W - (n - i));
    if (FN(popcount)(m) >= k)
      return (HsInt)(i + nth_bit(m, k) + 1);
  }
  return (HsInt)n;
}

ATTR static HsInt FN(scan_units)(bytes s, size_t i, size_t to, HsInt k, HsInt u, int wide)
{
  if (to < W)
    return LO(scan_units)(s, i, to, k, u, wide);
  for (; to - i >= W; i += W) {
    V x = LOAD(s + i);
    uint32_t conts = FN(bits)(FN(is_cont)(x)), fours = FN(bits)(FN(is_four)(x));
    HsInt c = FN(units)(ALL, conts, fours, wide);
    if (u + c > k)
      return (HsInt)(i + FN(units_stop)(ALL, conts, fours, k, u, wide));
    u += c;
  }
  if (i < to) {
    V x = LOAD(s + to - W);
    uint32_t conts = FN(bits)(FN(is_cont)(x)), fours = FN(bits)(FN(is_four)(x));
    uint32_t lanes = ALL & ~((1u << (W - (to - i))) - 1);
    if (u + FN(units)(lanes, conts, fours, wide) > k)
      return (HsInt)(to - W + FN(units_stop)(lanes, conts, fours, k, u, wide));
  }
  return (HsInt)to;
}

#undef FN
#undef LO
#undef ALL
#undef LEVEL
#undef LOWER
#undef ATTR
#undef W
#undef V
#undef MM
#undef LOAD
#undef ZERO
#undef SUM

#endif /* LEVEL */
