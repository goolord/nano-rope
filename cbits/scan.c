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

/* What both levels make of the movemask bits of a vector, each with its own
 * way of counting them.
 *
 * pack_tail: the counts of a metrics scan in the last t lanes of a vector of
 * the given width. units: units in the lanes set in `lanes`, out of the
 * continuation and 4-byte leader bits of a vector. */
#define MASK_HELPERS(level, attr)                                                                       \
  attr static inline HsWord64 pack_tail_##level(uint32_t conts, uint32_t fours, uint32_t nls, int width, \
                                                size_t t)                                               \
  {                                                                                                     \
    int shift = width - (int)t;                                                                         \
    return PACK(popcount_##level(conts >> shift), popcount_##level(fours >> shift),                     \
                popcount_##level(nls >> shift));                                                        \
  }                                                                                                     \
  attr static inline HsInt units_##level(uint32_t lanes, uint32_t conts, uint32_t fours, int wide)      \
  {                                                                                                     \
    return (HsInt)popcount_##level(lanes & ~conts) + (wide ? (HsInt)popcount_##level(lanes & fours) : 0); \
  }                                                                                                     \
  /* The lane of the first code point that does not fit into k units, with u <= k of them counted      \
   * already, in a vector whose `lanes` hold more than the rest. Read off the bits: where every unit   \
   * is a code point it is the leader after those that fit. */                                         \
  attr static inline uint32_t units_stop_##level(uint32_t lanes, uint32_t conts, uint32_t fours, HsInt k, \
                                                 HsInt u, int wide)                                     \
  {                                                                                                     \
    uint32_t leaders = lanes & ~conts;                                                                  \
    if (!wide || (fours & lanes) == 0)                                                                  \
      return nth_bit(leaders, k - u + 1);                                                               \
    for (;;) {                                                                                          \
      uint32_t lane = (uint32_t)__builtin_ctz(leaders);                                                 \
      HsInt w = (fours >> lane) & 1 ? 2 : 1;                                                            \
      if (u + w > k)                                                                                    \
        return lane;                                                                                    \
      u += w;                                                                                           \
      leaders &= leaders - 1;                                                                           \
    }                                                                                                   \
  }

MASK_HELPERS(sse2, )
MASK_HELPERS(avx2, AVX2)

/* ------------------------------------------------------------------------
 * Level 1: SSE2.
 *
 * Continuation bytes 0x80 .. 0xBF are exactly the signed bytes below -64;
 * leaders of 4-byte sequences are the bytes whose unsigned maximum with 0xF0
 * is themselves.
 */

static inline __m128i conts128(__m128i x)
{
  return _mm_cmpgt_epi8(_mm_set1_epi8((char)0xC0), x);
}

static inline __m128i fours128(__m128i x)
{
  return _mm_cmpeq_epi8(_mm_max_epu8(x, _mm_set1_epi8((char)0xF0)), x);
}

static inline __m128i newlines128(__m128i x)
{
  return _mm_cmpeq_epi8(x, _mm_set1_epi8('\n'));
}

static inline uint32_t bits128(__m128i m)
{
  return (uint32_t)_mm_movemask_epi8(m);
}

static inline HsWord64 sum128(__m128i v)
{
  return (HsWord64)_mm_cvtsi128_si64(v) + (HsWord64)_mm_cvtsi128_si64(_mm_unpackhi_epi64(v, v));
}

static HsWord64 metrics_sse2(bytes s, size_t n)
{
  if (n < 16)
    return metrics_c(s, n);
  const __m128i zero = _mm_setzero_si128();
  __m128i sum = zero; /* packed as by PACK, in each 64-bit lane */
  size_t i = 0;
  while (n - i >= 16) {
    size_t v = (n - i) / 16;
    if (v > FLUSH)
      v = FLUSH;
    __m128i ac = zero, af = zero, an = zero;
    for (; v > 0; v--, i += 16) {
      __m128i x = _mm_loadu_si128((const __m128i *)(s + i));
      ac = _mm_sub_epi8(ac, conts128(x));
      af = _mm_sub_epi8(af, fours128(x));
      an = _mm_sub_epi8(an, newlines128(x));
    }
    sum = _mm_add_epi64(sum, _mm_sad_epu8(ac, zero));
    sum = _mm_add_epi64(sum, _mm_slli_epi64(_mm_sad_epu8(af, zero), 21));
    sum = _mm_add_epi64(sum, _mm_slli_epi64(_mm_sad_epu8(an, zero), 42));
  }
  HsWord64 packed = sum128(sum);
  if (i < n) {
    __m128i x = _mm_loadu_si128((const __m128i *)(s + n - 16));
    packed += pack_tail_sse2(bits128(conts128(x)), bits128(fours128(x)), bits128(newlines128(x)), 16, n - i);
  }
  return packed;
}

static HsInt newlines_sse2(bytes s, size_t n)
{
  if (n < 16)
    return newlines_c(s, n);
  const __m128i zero = _mm_setzero_si128();
  __m128i sn = zero;
  size_t i = 0;
  while (n - i >= 16) {
    size_t v = (n - i) / 16;
    if (v > FLUSH)
      v = FLUSH;
    __m128i an = zero;
    for (; v > 0; v--, i += 16)
      an = _mm_sub_epi8(an, newlines128(_mm_loadu_si128((const __m128i *)(s + i))));
    sn = _mm_add_epi64(sn, _mm_sad_epu8(an, zero));
  }
  HsInt nls = (HsInt)sum128(sn);
  if (i < n) {
    __m128i x = _mm_loadu_si128((const __m128i *)(s + n - 16));
    nls += popcount_sse2(bits128(newlines128(x)) >> (16 - (n - i)));
  }
  return nls;
}

static uint32_t newline_mask128(bytes p)
{
  return bits128(newlines128(_mm_loadu_si128((const __m128i *)p)));
}

static HsInt find_newline_sse2(bytes s, size_t i, size_t n)
{
  if (n < 16)
    return find_newline_c(s, i, n);
  for (; n - i >= 16; i += 16) {
    uint32_t m = newline_mask128(s + i);
    if (m)
      return (HsInt)(i + __builtin_ctz(m));
  }
  if (i < n) {
    uint32_t m = newline_mask128(s + n - 16) >> (16 - (n - i));
    if (m)
      return (HsInt)(i + __builtin_ctz(m));
  }
  return (HsInt)n;
}

static HsInt find_newline_back_sse2(bytes s, size_t i)
{
  if (i < 16)
    return find_newline_back_c(s, i);
  for (; i >= 16; i -= 16) {
    uint32_t m = newline_mask128(s + i - 16);
    if (m)
      return (HsInt)(i - 16 + 31 - __builtin_clz(m));
  }
  if (i > 0) {
    uint32_t m = newline_mask128(s) & ((1u << i) - 1);
    if (m)
      return (HsInt)(31 - __builtin_clz(m));
  }
  return -1;
}

static HsInt nth_newline_sse2(bytes s, size_t i, size_t n, HsInt k)
{
  if (n < 16)
    return nth_newline_c(s, i, n, k);
  for (; n - i >= 16; i += 16) {
    uint32_t m = newline_mask128(s + i);
    HsInt c = popcount_sse2(m);
    if (c >= k)
      return (HsInt)(i + nth_bit(m, k) + 1);
    k -= c;
  }
  if (i < n) {
    uint32_t m = newline_mask128(s + n - 16) >> (16 - (n - i));
    if (popcount_sse2(m) >= k)
      return (HsInt)(i + nth_bit(m, k) + 1);
  }
  return (HsInt)n;
}

static HsInt scan_units_sse2(bytes s, size_t i, size_t to, HsInt k, HsInt u, int wide)
{
  if (to < 16)
    return scan_units_c(s, i, to, k, u, wide);
  for (; to - i >= 16; i += 16) {
    __m128i x = _mm_loadu_si128((const __m128i *)(s + i));
    uint32_t conts = bits128(conts128(x)), fours = bits128(fours128(x));
    HsInt c = units_sse2(0xFFFF, conts, fours, wide);
    if (u + c > k)
      return (HsInt)(i + units_stop_sse2(0xFFFF, conts, fours, k, u, wide));
    u += c;
  }
  if (i < to) {
    __m128i x = _mm_loadu_si128((const __m128i *)(s + to - 16));
    uint32_t conts = bits128(conts128(x)), fours = bits128(fours128(x));
    uint32_t lanes = 0xFFFFu & ~((1u << (16 - (to - i))) - 1);
    if (u + units_sse2(lanes, conts, fours, wide) > k)
      return (HsInt)(to - 16 + units_stop_sse2(lanes, conts, fours, k, u, wide));
  }
  return (HsInt)to;
}

/* ------------------------------------------------------------------------
 * Level 2: AVX2. The same as SSE2, twice as wide; whatever is shorter than
 * a vector goes to SSE2.
 */

AVX2 static inline __m256i conts256(__m256i x)
{
  return _mm256_cmpgt_epi8(_mm256_set1_epi8((char)0xC0), x);
}

AVX2 static inline __m256i fours256(__m256i x)
{
  return _mm256_cmpeq_epi8(_mm256_max_epu8(x, _mm256_set1_epi8((char)0xF0)), x);
}

AVX2 static inline __m256i newlines256(__m256i x)
{
  return _mm256_cmpeq_epi8(x, _mm256_set1_epi8('\n'));
}

AVX2 static inline uint32_t bits256(__m256i m)
{
  return (uint32_t)_mm256_movemask_epi8(m);
}

AVX2 static inline HsWord64 sum256(__m256i v)
{
  __m128i w = _mm_add_epi64(_mm256_castsi256_si128(v), _mm256_extracti128_si256(v, 1));
  return (HsWord64)_mm_cvtsi128_si64(w) + (HsWord64)_mm_extract_epi64(w, 1);
}

AVX2 static HsWord64 metrics_avx2(bytes s, size_t n)
{
  if (n < 32)
    return metrics_sse2(s, n);
  const __m256i zero = _mm256_setzero_si256();
  __m256i sum = zero; /* packed as by PACK, in each 64-bit lane */
  size_t i = 0;
  while (n - i >= 32) {
    size_t v = (n - i) / 32;
    if (v > FLUSH)
      v = FLUSH;
    __m256i ac = zero, af = zero, an = zero;
    for (; v > 0; v--, i += 32) {
      __m256i x = _mm256_loadu_si256((const __m256i *)(s + i));
      ac = _mm256_sub_epi8(ac, conts256(x));
      af = _mm256_sub_epi8(af, fours256(x));
      an = _mm256_sub_epi8(an, newlines256(x));
    }
    sum = _mm256_add_epi64(sum, _mm256_sad_epu8(ac, zero));
    sum = _mm256_add_epi64(sum, _mm256_slli_epi64(_mm256_sad_epu8(af, zero), 21));
    sum = _mm256_add_epi64(sum, _mm256_slli_epi64(_mm256_sad_epu8(an, zero), 42));
  }
  HsWord64 packed = sum256(sum);
  if (i < n) {
    __m256i x = _mm256_loadu_si256((const __m256i *)(s + n - 32));
    packed += pack_tail_avx2(bits256(conts256(x)), bits256(fours256(x)), bits256(newlines256(x)), 32, n - i);
  }
  return packed;
}

AVX2 static HsInt newlines_avx2(bytes s, size_t n)
{
  if (n < 32)
    return newlines_sse2(s, n);
  const __m256i zero = _mm256_setzero_si256();
  __m256i sn = zero;
  size_t i = 0;
  while (n - i >= 32) {
    size_t v = (n - i) / 32;
    if (v > FLUSH)
      v = FLUSH;
    __m256i an = zero;
    for (; v > 0; v--, i += 32)
      an = _mm256_sub_epi8(an, newlines256(_mm256_loadu_si256((const __m256i *)(s + i))));
    sn = _mm256_add_epi64(sn, _mm256_sad_epu8(an, zero));
  }
  HsInt nls = (HsInt)sum256(sn);
  if (i < n) {
    __m256i x = _mm256_loadu_si256((const __m256i *)(s + n - 32));
    nls += popcount_avx2(bits256(newlines256(x)) >> (32 - (n - i)));
  }
  return nls;
}

AVX2 static inline uint32_t newline_mask256(bytes p)
{
  return bits256(newlines256(_mm256_loadu_si256((const __m256i *)p)));
}

AVX2 static HsInt find_newline_avx2(bytes s, size_t i, size_t n)
{
  if (n < 32)
    return find_newline_sse2(s, i, n);
  for (; n - i >= 32; i += 32) {
    uint32_t m = newline_mask256(s + i);
    if (m)
      return (HsInt)(i + __builtin_ctz(m));
  }
  if (i < n) {
    uint32_t m = newline_mask256(s + n - 32) >> (32 - (n - i));
    if (m)
      return (HsInt)(i + __builtin_ctz(m));
  }
  return (HsInt)n;
}

AVX2 static HsInt find_newline_back_avx2(bytes s, size_t i)
{
  if (i < 32)
    return find_newline_back_sse2(s, i);
  for (; i >= 32; i -= 32) {
    uint32_t m = newline_mask256(s + i - 32);
    if (m)
      return (HsInt)(i - 32 + 31 - __builtin_clz(m));
  }
  if (i > 0) {
    uint32_t m = newline_mask256(s) & ((1u << i) - 1);
    if (m)
      return (HsInt)(31 - __builtin_clz(m));
  }
  return -1;
}

AVX2 static HsInt nth_newline_avx2(bytes s, size_t i, size_t n, HsInt k)
{
  if (n < 32)
    return nth_newline_sse2(s, i, n, k);
  for (; n - i >= 32; i += 32) {
    uint32_t m = newline_mask256(s + i);
    HsInt c = popcount_avx2(m);
    if (c >= k)
      return (HsInt)(i + nth_bit(m, k) + 1);
    k -= c;
  }
  if (i < n) {
    uint32_t m = newline_mask256(s + n - 32) >> (32 - (n - i));
    if (popcount_avx2(m) >= k)
      return (HsInt)(i + nth_bit(m, k) + 1);
  }
  return (HsInt)n;
}

AVX2 static HsInt scan_units_avx2(bytes s, size_t i, size_t to, HsInt k, HsInt u, int wide)
{
  if (to < 32)
    return scan_units_sse2(s, i, to, k, u, wide);
  for (; to - i >= 32; i += 32) {
    __m256i x = _mm256_loadu_si256((const __m256i *)(s + i));
    uint32_t conts = bits256(conts256(x)), fours = bits256(fours256(x));
    HsInt c = units_avx2(0xFFFFFFFFu, conts, fours, wide);
    if (u + c > k)
      return (HsInt)(i + units_stop_avx2(0xFFFFFFFFu, conts, fours, k, u, wide));
    u += c;
  }
  if (i < to) {
    __m256i x = _mm256_loadu_si256((const __m256i *)(s + to - 32));
    uint32_t conts = bits256(conts256(x)), fours = bits256(fours256(x));
    uint32_t lanes = ~((1u << (32 - (to - i))) - 1);
    if (u + units_avx2(lanes, conts, fours, wide) > k)
      return (HsInt)(to - 32 + units_stop_avx2(lanes, conts, fours, k, u, wide));
  }
  return (HsInt)to;
}

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
