// SPDX-License-Identifier: AGPL-3.0-or-later
// A libFuzzer target: one input is one Sonic Pi program, run live in a fresh runtime as a plugin runs it, with time
// stepped by hand. A program can ask for nothing of the machine (runtime/build_config.rb), so what stands between
// a stranger's program and the process is the memory safety of mruby, our patches to it and the C beneath the
// runtime (sp_host.c, mrbgems/sonic-pi-core). This runs them under ASan and UBSan.
//
// A program may loop for ever or ask for all the memory there is, and both are its right, not a finding. So the
// harness gives each input a budget of VM instructions, of time and of memory. Past the instruction or time
// budget it leaves the interpreter where it stands and frees everything it allocated; past the memory budget the
// allocator answers NULL, which mruby raises as NoMemoryError.
//
// libFuzzer's own mutations work on bytes, and a program with a random byte changed is nearly always a syntax
// error: it tests the parser and little else. So most mutations here work on whole lines and whole words
// (LLVMFuzzerCustomMutator below), which keeps most of what runs a program that runs.
//
// An input can hold several runs, each after a line that says "#>>": the program run again, changed, while it
// plays, which is how Sonic Pi is used. Between them the harness does what a host does: reads the messages the
// runtime leaves for the engine and its table of threads, holds the clock, changes the tempo, sends a cue.
//
//   scripts/build-fuzz.sh && build/fuzz/fuzz_program <a scratch corpus> build/fuzz/seeds runtime/fuzz/corpus
#include <mruby.h>
#include <ctype.h>
#include <setjmp.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef struct sp_host sp_host;
sp_host *sp_open(void);
void sp_close(sp_host *h);
void sp_abandon(sp_host *h);
void *sp_mrb_state(void);
int sp_install_table(const char *source, const uint8_t *bytes, int len);
int sp_live_boot(void);
int sp_run(const char *code, double now);
double sp_tick(double now);
void sp_stop_all(void);
void sp_cue(const char *address, const char *nums, double now);
int sp_set_samples_dir(const char *dir);
int sp_install_sample(const char *path, int num_frames, int num_chans, int sample_rate, const double *onsets, int num_onsets);
void sp_hold(double seconds);
void sp_set_link_bpm(double bpm, double at);
const double *sp_process_table(double now);
int sp_process_table_len(void);
void sp_silence(double fade, double since, double now);
void sp_engine_lost(void);
const uint8_t *sp_out_ptr(void);
int sp_out_len(void);

// ── memory: every block mruby holds is on a list, so an interpreter left mid-run can be freed whole ──────────────
typedef struct block { struct block *prev, *next; size_t size; size_t pad; } block;
static block all = { &all, &all, 0, 0 };
static size_t held;
#define MEMORY_BUDGET ((size_t)192 << 20)
#define LARGEST_BLOCK ((size_t)64 << 20)

void *mrb_basic_alloc_func(void *p, size_t size) {
  block *old = p ? (block *)p - 1 : NULL;
  if (size == 0) {
    if (old) { old->prev->next = old->next; old->next->prev = old->prev; held -= old->size; free(old); }
    return NULL;
  }
  const size_t was = old ? old->size : 0;
  if (size > LARGEST_BLOCK || held - was + size > MEMORY_BUDGET) return NULL;
  if (old) { old->prev->next = old->next; old->next->prev = old->prev; }
  block *b = realloc(old, sizeof(block) + size);
  if (!b) {
    if (old) { old->prev = all.prev; old->next = &all; all.prev->next = old; all.prev = old; }
    return NULL;
  }
  b->size = size;
  b->prev = all.prev; b->next = &all; all.prev->next = b; all.prev = b;
  held = held - was + size;
  return b + 1;
}

static void free_everything(void) {
  while (all.next != &all) { block *b = all.next; all.next = b->next; free(b); }
  all.prev = &all;
  held = 0;
}

// ── instructions and time ───────────────────────────────────────────────────────────────────────────────────────
// One instruction can be a long call into C (inspect of a large object), so a loop of those is ended by the clock.
#define INSTRUCTION_BUDGET 3000000
#define SECONDS_BUDGET 10
static long instructions;
static time_t began;
static jmp_buf out_of_budget;
static void count(mrb_state *mrb, const struct mrb_irep *irep, const mrb_code *pc, mrb_value *regs) {
  (void)mrb; (void)irep; (void)pc; (void)regs;
  if (++instructions > INSTRUCTION_BUDGET) longjmp(out_of_budget, 1);
  if ((instructions & 0xff) == 0 && time(NULL) - began > SECONDS_BUDGET) longjmp(out_of_budget, 1);
}

// a random table: the runtime wants a 16-bit PCM wav's data chunk (rand.rb data_chunk)
static uint8_t table[44 + 4096];
static void make_table(void) {
  memcpy(table, "RIFF", 4); memcpy(table + 8, "WAVEfmt ", 8);
  table[16] = 16; table[20] = 1; table[22] = 1; table[34] = 16;
  memcpy(table + 36, "data", 4);
  const uint32_t n = sizeof table - 44;
  memcpy(table + 40, &n, 4);
  uint32_t x = 2463534242u;
  for (size_t i = 44; i < sizeof table; i++) { x ^= x << 13; x ^= x >> 17; x ^= x << 5; table[i] = (uint8_t)x; }
}

// What a host reads after each call: every byte of the messages for the engine and every number of the table of
// threads, so that ASan sees a length that runs past what is there.
static volatile uint64_t read_back;
static void read_what_a_host_reads(double now) {
  const uint8_t *out = sp_out_ptr();
  uint64_t sum = 0;
  for (int i = 0, n = sp_out_len(); i < n; i++) sum += out[i];
  const double *rows = sp_process_table(now);
  for (int i = 0, n = sp_process_table_len(); i < n; i++) sum += (uint64_t)(rows[i] != 0);
  read_back = sum;
}

#define RUN_AGAIN "#>>"
#define MOST_RUNS 4
#define TICKS_PER_RUN 24

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
  static const char *TABLES[] = { "white", "pink", "light_pink", "dark_pink", "perlin" };
  static const char *SAMPLES[] = { "/samples/bd_haus.flac", "/samples/loop_amen.flac", "/samples/ambi_choir.flac" };
  static const double ONSETS[] = { 0.0, 0.21, 0.43, 0.65, 0.87, 1.08, 1.3, 1.52 };
  if (!table[0]) make_table();
  if (size > 1 << 16) return 0;
  char *code = malloc(size + 1);
  memcpy(code, data, size);
  code[size] = 0;                                       // a program is text: it ends at its first NUL
  char *runs[MOST_RUNS] = { code };
  int run_count = 1;
  for (char *at = code; run_count < MOST_RUNS && (at = strstr(at, "\n" RUN_AGAIN)) != NULL; ) {
    *at = 0;
    at += 1 + strlen(RUN_AGAIN);
    runs[run_count++] = at;
  }

  instructions = 0;
  began = time(NULL);
  sp_host *volatile h = NULL;
  if (setjmp(out_of_budget) == 0) {
    h = sp_open();
    if (h) {
      ((mrb_state *)sp_mrb_state())->code_fetch_hook = count;
      for (size_t i = 0; i < sizeof TABLES / sizeof *TABLES; i++) sp_install_table(TABLES[i], table, (int)sizeof table);
      sp_set_samples_dir("/samples");
      for (size_t i = 0; i < sizeof SAMPLES / sizeof *SAMPLES; i++) sp_install_sample(SAMPLES[i], 77175, 2, 44100, ONSETS, 8);
      if (sp_live_boot() == 0) {
        double now = 1000.0;
        for (int r = 0; r < run_count; r++) {
          sp_run(runs[r], now);
          read_what_a_host_reads(now);
          for (int i = 0; i < TICKS_PER_RUN; i++) {
            if (i == 4) sp_hold(0.1);                        // the host lost a tenth of a second
            if (i == 6) sp_set_link_bpm(120.0 + 20.0 * r, now);
            if (i == 8) sp_cue("/cue/fuzz", "[1.0]", now);   // something for a sync to hear
            const double next = sp_tick(now);
            read_what_a_host_reads(now);
            if (next < 0) break;
            now = next > now && next - now < 3600.0 ? next : now + 0.05;
          }
        }
        sp_stop_all();
        sp_silence(0.5, now, now);
        read_what_a_host_reads(now);
        sp_engine_lost();
        sp_tick(now + 1.0);
        read_what_a_host_reads(now + 1.0);
      }
      sp_close(h);
    }
  } else {
    // left mid-run: nothing of this interpreter is used again
    sp_abandon(h);
  }
  free_everything();
  free(code);
  return 0;
}

// ── mutation: whole lines and whole words ───────────────────────────────────────────────────────────────────────
size_t LLVMFuzzerMutate(uint8_t *data, size_t size, size_t max_size);

static const char *const WORDS[] = {
#include "words.inc"                                    // the dictionary's entries (scripts/build-fuzz.sh)
};
static const char *const NUMBERS[] = {
  "0", "1", "-1", "0.25", "0.5", "2", "4", "60", "127", "128", "-0.0", "1e9", "1e308", "1/0.0", "0x7fffffff",
  "4294967296", "9223372036854775807", "nil", ":c4", "[]", "(ring 1, 2, 3)", "rrand(0, 1)",
};
static const char *const OPENERS[] = {                  // each is closed by "end"
  "in_thread do", "live_loop :fuzz do", "live_loop :fuzz, sync: :fuzz do", "with_fx :reverb do", "with_synth :saw do",
  "with_bpm 240 do", "4.times do", "loop do", "define :fuzz do", "at [0, 0.5] do", "time_warp 0.5 do",
  "density 2 do", "with_random_seed 1 do", "begin", "if one_in(2)", "[1, 2].each do |x|",
};
#define COUNT(a) (sizeof(a) / sizeof *(a))

static uint32_t rng;
static uint32_t pick(uint32_t n) { rng ^= rng << 13; rng ^= rng >> 17; rng ^= rng << 5; return n ? rng % n : 0; }

// line i of a program is [at[i], at[i + 1])
#define MOST_LINES 512
static size_t lines_of(const uint8_t *d, size_t size, size_t *at) {
  size_t n = 0;
  at[n++] = 0;
  for (size_t i = 0; i + 1 < size && n < MOST_LINES; i++) if (d[i] == '\n') at[n++] = i + 1;
  at[n] = size;
  return n;
}

typedef struct { uint8_t *p; size_t n, max; } out;
static void put(out *o, const void *s, size_t len) {
  if (len > o->max - o->n) len = o->max - o->n;
  memcpy(o->p + o->n, s, len);
  o->n += len;
}
static void put_text(out *o, const char *s) { put(o, s, strlen(s)); }
// lines [from, to), each ending in a newline whether or not the last one did
static void put_lines(out *o, const uint8_t *d, const size_t *at, size_t from, size_t to) {
  if (from >= to) return;
  put(o, d + at[from], at[to] - at[from]);
  if (at[to] > at[from] && d[at[to] - 1] != '\n') put(o, "\n", 1);
}

// A word is a name (play, each_slice, empty?) or a number (60, 0.25, 1_000); the dot of a method call is neither.
static int word_char(int c) { return isalnum(c) || c == '_'; }
static size_t word_end(const uint8_t *line, size_t len, size_t i) {
  const int number = isdigit(line[i]);
  while (i < len && (word_char(line[i]) || (number && line[i] == '.' && i + 1 < len && isdigit(line[i + 1])))) i++;
  if (!number && i < len && (line[i] == '?' || line[i] == '!')) i++;
  return i;
}
// one word or number of the line swapped for another the runtime knows
static void put_line_with_a_word_changed(out *o, const uint8_t *line, size_t len) {
  size_t words = 0;
  for (size_t i = 0; i < len; i = word_char(line[i]) ? word_end(line, len, i) : i + 1) if (word_char(line[i])) words++;
  if (!words) { put(o, line, len); return; }
  size_t want = pick((uint32_t)words), i = 0;
  for (;; i = word_char(line[i]) ? word_end(line, len, i) : i + 1) if (word_char(line[i]) && want-- == 0) break;
  const size_t end = word_end(line, len, i);
  put(o, line, i);
  put_text(o, isdigit(line[i]) ? NUMBERS[pick(COUNT(NUMBERS))] : WORDS[pick(COUNT(WORDS))]);
  put(o, line + end, len - end);
}

static uint8_t copy[1 << 16];

size_t LLVMFuzzerCustomMutator(uint8_t *data, size_t size, size_t max_size, unsigned int seed) {
  rng = seed | 1;
  if (size > sizeof copy || pick(4) == 0) return LLVMFuzzerMutate(data, size, max_size);
  memcpy(copy, data, size);
  size_t at[MOST_LINES + 1];
  const size_t n = lines_of(copy, size, at);
  const size_t a = pick((uint32_t)n), b = a + 1 + pick((uint32_t)(n - a) < 6 ? (uint32_t)(n - a) : 6);   // [a, b)
  out o = { data, 0, max_size };
  switch (pick(8)) {
  case 0:                                               // said twice
    put_lines(&o, copy, at, 0, b); put_lines(&o, copy, at, a, b); put_lines(&o, copy, at, b, n);
    break;
  case 1:                                               // taken out
    put_lines(&o, copy, at, 0, a); put_lines(&o, copy, at, b, n);
    break;
  case 2: {                                             // moved earlier
    const size_t to = pick((uint32_t)a + 1);
    put_lines(&o, copy, at, 0, to); put_lines(&o, copy, at, a, b);
    put_lines(&o, copy, at, to, a); put_lines(&o, copy, at, b, n);
    break;
  }
  case 3:                                               // inside a block of its own
    put_lines(&o, copy, at, 0, a);
    put_text(&o, OPENERS[pick(COUNT(OPENERS))]); put_text(&o, "\n");
    put_lines(&o, copy, at, a, b);
    put_text(&o, "end\n");
    put_lines(&o, copy, at, b, n);
    break;
  case 4:                                               // a new call before it
    put_lines(&o, copy, at, 0, a);
    put_text(&o, WORDS[pick(COUNT(WORDS))]); put_text(&o, " "); put_text(&o, NUMBERS[pick(COUNT(NUMBERS))]);
    if (pick(2)) { put_text(&o, ", "); put_text(&o, WORDS[pick(COUNT(WORDS))]); put_text(&o, ": "); put_text(&o, NUMBERS[pick(COUNT(NUMBERS))]); }
    put_text(&o, "\n");
    put_lines(&o, copy, at, a, n);
    break;
  case 5:                                               // run again while it plays, some of it said twice
    put_lines(&o, copy, at, 0, n);
    put_text(&o, RUN_AGAIN "\n");
    put_lines(&o, copy, at, 0, b); put_lines(&o, copy, at, a, n);
    break;
  default:                                              // one word of one line changed
    put_lines(&o, copy, at, 0, a);
    put_line_with_a_word_changed(&o, copy + at[a], at[a + 1] - at[a]);
    if (o.n && o.p[o.n - 1] != '\n') put(&o, "\n", 1);
    put_lines(&o, copy, at, a + 1, n);
    break;
  }
  return o.n;
}

// some lines of one program set into another
size_t LLVMFuzzerCustomCrossOver(const uint8_t *one, size_t one_size, const uint8_t *two, size_t two_size,
                                 uint8_t *result, size_t max_size, unsigned int seed) {
  rng = seed | 1;
  size_t at1[MOST_LINES + 1], at2[MOST_LINES + 1];
  const size_t n1 = lines_of(one, one_size, at1), n2 = lines_of(two, two_size, at2);
  const size_t into = pick((uint32_t)n1 + 1), a = pick((uint32_t)n2), b = a + 1 + pick((uint32_t)(n2 - a));
  out o = { result, 0, max_size };
  put_lines(&o, one, at1, 0, into); put_lines(&o, two, at2, a, b); put_lines(&o, one, at1, into, n1);
  return o.n;
}
