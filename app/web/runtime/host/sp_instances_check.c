// SPDX-License-Identifier: AGPL-3.0-or-later
// Several runtimes in one process, each on a thread of its own, as a DAW holds a plugin's instances: each must
// trace, and play a live session, exactly as one alone does. A runtime is its thread's (sp_init), so what one
// thread does never reaches another's.
//
//   cc $(mruby flags) sp_host.c sp_instances_check.c runtime_irep.c $(mruby libs) -lpthread -o sp-instances-check
//   sp-instances-check <etc/buffers>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int sp_init(void);
int sp_install_table(const char *source, const uint8_t *bytes, int len);
const char *sp_trace(const char *code, const char *file);
int sp_live_boot(void);
int sp_run(const char *code, double now);
double sp_tick(double now);
void sp_stop_all(void);
const uint8_t *sp_out_ptr(void);
int sp_out_len(void);
const double *sp_process_table(double now);
int sp_process_table_len(void);

#ifndef THREADS
#define THREADS 4
#endif
#define ROUNDS 6

static const char *PROGRAMS[] = {
  "use_random_seed 42\n4.times do\n  play rrand(50, 70), release: 0.1\n  sleep 0.25\nend\n",
  "in_thread do\n  3.times { play 72; sleep 0.5 }\nend\nuse_synth :tb303\nplay_pattern_timed [60, 62, 64], [0.25]\n",
  "with_fx :reverb, room: 0.8 do\n  play chord(:e3, :minor), amp: 0.5\n  sleep 1\n  puts (ring 1, 2, 3).tick\nend\n",
  "define :bass do |n|\n  synth :fm, note: n, divisor: 2\nend\n[40, 43, 47].each { |n| bass n; sleep 0.5 }\n",
};
#define NPROGRAMS (sizeof PROGRAMS / sizeof *PROGRAMS)

static uint8_t *white;
static long white_len;

typedef struct { char *text; size_t len; } out_t;

static void append(out_t *o, const void *p, size_t n) {
  o->text = realloc(o->text, o->len + n + 1);
  memcpy(o->text + o->len, p, n);
  o->len += n;
  o->text[o->len] = 0;
}

// what one program does in a live session: every frame its outbox carried, and the process table each tick
static out_t live(const char *code) {
  out_t o = { 0 };
  if (sp_live_boot() != 0) { append(&o, "boot failed", 11); return o; }
  double now = 1000.0;
  sp_run(code, now);
  for (int i = 0; i < 200; i++) {
    append(&o, sp_out_ptr(), (size_t)sp_out_len());
    const double *t = sp_process_table(now);
    append(&o, t, sizeof(double) * (size_t)sp_process_table_len());
    double next = sp_tick(now);
    if (next < 0) break;
    if (next > now) now = next;
  }
  append(&o, sp_out_ptr(), (size_t)sp_out_len());
  sp_stop_all();
  append(&o, sp_out_ptr(), (size_t)sp_out_len());
  return o;
}

static int boot(void) {
  if (sp_init() != 0) return 1;
  return sp_install_table("white", white, (int)white_len);
}

// A runtime numbers its synthdefs, nodes and buffers for as long as it lives, so what a program plays depends on
// what ran before it in that runtime: each thread is held to a fresh runtime that ran the same sequence alone.
#define STEPS (ROUNDS * NPROGRAMS)
static out_t expected_trace[THREADS][STEPS], expected_live[THREADS][STEPS];

static size_t program_at(long id, size_t step) {   // each thread in its own order
  return (step % NPROGRAMS + (size_t)id + step / NPROGRAMS) % NPROGRAMS;
}

static void *worker(void *arg) {
  long id = (long)arg, bad = 0;
  if (boot() != 0) { fprintf(stderr, "thread %ld: the runtime did not boot\n", id); return (void *)1; }
  for (size_t step = 0; step < STEPS; step++) {
    const size_t which = program_at(id, step);
    const char *trace = sp_trace(PROGRAMS[which], "check.rb");
    if (!trace || strcmp(trace, expected_trace[id][step].text) != 0) {
      if (!bad++) fprintf(stderr, "thread %ld: program %zu traced differently at step %zu\n", id, which, step);
    }
    out_t l = live(PROGRAMS[which]);
    if (l.len != expected_live[id][step].len || memcmp(l.text, expected_live[id][step].text, l.len) != 0) {
      if (!bad++) fprintf(stderr, "thread %ld: program %zu played differently live at step %zu (%zu bytes, alone %zu)\n",
                          id, which, step, l.len, expected_live[id][step].len);
    }
    free(l.text);
  }
  return (void *)bad;
}

// What a thread's sequence does in a fresh runtime alone, on this thread.
static int alone(long id) {
  if (boot() != 0) { fprintf(stderr, "the runtime did not boot\n"); return 1; }
  for (size_t step = 0; step < STEPS; step++) {
    const size_t which = program_at(id, step);
    const char *trace = sp_trace(PROGRAMS[which], "check.rb");
    if (!trace || strstr(trace, "host_error")) { fprintf(stderr, "program %zu: %s\n", which, trace ? trace : "no trace"); return 1; }
    append(&expected_trace[id][step], trace, strlen(trace));
    expected_live[id][step] = live(PROGRAMS[which]);
    if (expected_live[id][step].len < 64) { fprintf(stderr, "program %zu played nothing live\n", which); return 1; }
  }
  return 0;
}

int main(int argc, char **argv) {
  if (argc != 2) { fprintf(stderr, "usage: sp-instances-check <etc/buffers>\n"); return 2; }
  char path[4096];
  snprintf(path, sizeof path, "%s/rand-stream.wav", argv[1]);
  FILE *f = fopen(path, "rb");
  if (!f) { fprintf(stderr, "no %s\n", path); return 2; }
  fseek(f, 0, SEEK_END);
  white_len = ftell(f);
  fseek(f, 0, SEEK_SET);
  white = malloc((size_t)white_len);
  if (fread(white, 1, (size_t)white_len, f) != (size_t)white_len) return 2;
  fclose(f);

  for (long t = 0; t < THREADS; t++)
    if (alone(t) != 0) return 1;

  pthread_t threads[THREADS];
  for (long t = 0; t < THREADS; t++) pthread_create(&threads[t], NULL, worker, (void *)t);
  long bad = 0;
  for (int t = 0; t < THREADS; t++) {
    void *r;
    pthread_join(threads[t], &r);
    bad += (long)r;
  }
  const int runs = THREADS * (int)STEPS;
  if (bad) { printf("FAIL %ld of %d runs on %d threads differed from the same program alone\n", bad, runs * 2, THREADS); return 1; }
  printf("pass %d traces and %d live sessions on %d threads, each as it is alone\n", runs, runs, THREADS);
  return 0;
}
