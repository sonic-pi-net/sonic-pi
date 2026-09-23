// SPDX-License-Identifier: AGPL-3.0-or-later
// The runtime as a native command: mruby, the runtime's bytecode and the
// same assets the page hands it, running one program and printing its trace
// as JSON. runtime/bin/trace.rb prints that JSON under MRI; this prints it
// under real mruby, so scripts/check.rb can hold the interpreter Sonic Pi
// actually ships to the specs, where a core method mruby lacks would
// otherwise only show up in the browser.
//
//   build/runtime/sp-trace specs/play/use_arg_checks.rb
//   ADAPTER="$PWD/build/runtime/sp-trace" ruby scripts/check.rb
//
// The assets are the oracle's copy of Sonic Pi (the random tables and the
// built-in sounds); SP_RUNTIME_ASSETS, or a second argument, names another
// root. Paths are joined with "/" on every platform because that is the one
// separator the runtime's own Samples.dirname knows.
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <dirent.h>
#endif

// The seam sp_host.c exports; the page reaches the same functions through
// the wasm, in this order (web/runtime.js loadRuntime).
int sp_init(void);
int sp_install_table(const char *source, const uint8_t *bytes, int len);
int sp_set_samples_dir(const char *dir);
int sp_install_sample(const char *path, int num_frames, int num_chans, int sample_rate, const double *onsets, int num_onsets);
const char *sp_trace(const char *code, const char *file);

#ifndef SP_ASSET_ROOT
#define SP_ASSET_ROOT "."
#endif

// The five random tables, each the bytes of its wav file, named by the
// source a program asks for (rand.rb's Tables).
static const struct { const char *source, *file; } TABLES[] = {
  { "white",      "rand-stream.wav" },
  { "pink",       "rand-stream-pink.wav" },
  { "light_pink", "rand-stream-light-pink.wav" },
  { "dark_pink",  "rand-stream-dark-pink.wav" },
  { "perlin",     "rand-stream-perlin.wav" },
};

static void die(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fputs("sp-trace: ", stderr);
  vfprintf(stderr, fmt, ap);
  fputc('\n', stderr);
  va_end(ap);
  exit(1);
}

static char *join(const char *dir, const char *name) {
  size_t n = strlen(dir) + 1 + strlen(name) + 1;
  char *p = malloc(n);
  if (!p) die("out of memory");
  snprintf(p, n, "%s/%s", dir, name);
  return p;
}

// A whole file as bytes, with a NUL after them so it reads as a C string too.
static uint8_t *slurp(const char *path, long *len) {
  FILE *f = fopen(path, "rb");
  if (!f) die("cannot read %s", path);
  fseek(f, 0, SEEK_END);
  long n = ftell(f);
  rewind(f);
  uint8_t *buf = malloc((size_t)n + 1);
  if (!buf) die("out of memory");
  if (n && fread(buf, 1, (size_t)n, f) != (size_t)n) die("cannot read %s", path);
  buf[n] = 0;
  fclose(f);
  *len = n;
  return buf;
}

static int ends_with(const char *s, const char *suffix) {
  size_t n = strlen(s), m = strlen(suffix);
  return n >= m && strcmp(s + n - m, suffix) == 0;
}

// What the runtime needs to know about a sound file, read from the front of
// it: FLAC's STREAMINFO, where the last 64 bits hold the rate in 20, the
// channels in 3, the depth in 5 and the frame count in 36 (scripts/lib/
// flac-info.mjs reads the same field for the page's samples.json).
static int install_flac(const char *dir, const char *name) {
  char *path = join(dir, name);
  uint8_t h[26];
  FILE *f = fopen(path, "rb");
  if (!f) die("cannot read %s", path);
  size_t got = fread(h, 1, sizeof h, f);
  fclose(f);
  if (got != sizeof h || memcmp(h, "fLaC", 4) != 0) die("%s is not a FLAC file", path);
  uint32_t hi = (uint32_t)h[18] << 24 | (uint32_t)h[19] << 16 | (uint32_t)h[20] << 8 | h[21];
  uint32_t lo = (uint32_t)h[22] << 24 | (uint32_t)h[23] << 16 | (uint32_t)h[24] << 8 | h[25];
  int rate = (int)(hi >> 12);
  int chans = (int)((hi >> 9) & 7) + 1;
  int frames = (int)(((uint64_t)(hi & 0xf) << 32) | lo);
  if (sp_install_sample(path, frames, chans, rate, NULL, 0) != 0) die("cannot install %s", path);
  free(path);
  return 1;
}

// Every built-in sound, as the page installs every entry of samples.json.
// The onsets come from the table the runtime already carries (Data::ONSETS),
// so none are passed here.
static void install_samples(const char *dir) {
  int n = 0;
#ifdef _WIN32
  char *glob = join(dir, "*.flac");
  WIN32_FIND_DATAA found;
  HANDLE h = FindFirstFileA(glob, &found);
  if (h != INVALID_HANDLE_VALUE) {
    // the glob is checked again by name: Windows matches a file's short name too
    do { if (ends_with(found.cFileName, ".flac")) n += install_flac(dir, found.cFileName); } while (FindNextFileA(h, &found));
    FindClose(h);
  }
  free(glob);
#else
  DIR *d = opendir(dir);
  if (d) {
    for (struct dirent *e; (e = readdir(d)) != NULL; )
      if (ends_with(e->d_name, ".flac")) n += install_flac(dir, e->d_name);
    closedir(d);
  }
#endif
  if (!n) die("no samples in %s", dir);
}

int main(int argc, char **argv) {
  if (argc < 2 || argc > 3) {
    fprintf(stderr, "usage: sp-trace <program.rb> [asset root]\n");
    return 2;
  }
  const char *spec = argv[1];
  const char *root = argc > 2 ? argv[2] : getenv("SP_RUNTIME_ASSETS");
  if (!root || !*root) root = SP_ASSET_ROOT;

  if (sp_init() != 0) die("the runtime did not boot");

  char *buffers = join(root, "../../etc/buffers");
  for (size_t i = 0; i < sizeof TABLES / sizeof *TABLES; i++) {
    char *path = join(buffers, TABLES[i].file);
    long len;
    uint8_t *bytes = slurp(path, &len);
    if (sp_install_table(TABLES[i].source, bytes, (int)len) != 0) die("cannot install the %s table", TABLES[i].source);
    free(bytes);
    free(path);
  }
  free(buffers);

  // The folder programs see as SAMPLES_DIR. A trace never carries a machine
  // path (lang.rb writes it as "<samples>"), so this only has to be the
  // folder the sounds were installed from.
  char *samples = join(root, "../../etc/samples");
  if (sp_set_samples_dir(samples) != 0) die("cannot set the samples dir");
  install_samples(samples);
  free(samples);

  long len;
  char *code = (char *)slurp(spec, &len);
  const char *json = sp_trace(code, spec);
  if (!json) die("no trace for %s", spec);
  // sp_trace answers with host_error rather than raising when the runtime
  // itself fell over; that is a broken run, not a trace to compare.
  if (strncmp(json, "{\"host_error\"", 13) == 0) die("%s", json);
  printf("%s\n", json);
  free(code);
  return 0;
}
