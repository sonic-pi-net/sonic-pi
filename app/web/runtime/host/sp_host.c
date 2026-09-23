// SPDX-License-Identifier: AGPL-3.0-or-later
// The seam between the wasm and its host: a handful of C functions the page
// (or Node) calls, each a thin wrapper over a Ruby method of the runtime.
// The runtime's Ruby is linked in as bytecode (sp_runtime_irep, generated
// by mrbc from runtime/lib).
#include <mruby.h>
#include <mruby/irep.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/error.h>
#include <mruby/gc.h>
#include <mruby/array.h>
#include <mruby/numeric.h>
#include <mruby/proc.h>
#include <mruby/debug.h>
#include <mruby/hash.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#define SP_EXPORT EMSCRIPTEN_KEEPALIVE
#else
#define SP_EXPORT
#endif

extern const uint8_t sp_runtime_irep[];

static mrb_state *mrb;
static char *last_result;   // what sp_trace last returned; freed on the next call

static void report_exception(const char *where) {
  if (!mrb->exc) return;
  mrb_value msg = mrb_funcall(mrb, mrb_obj_value(mrb->exc), "inspect", 0);
  fprintf(stderr, "sp_host: %s: %s\n", where, mrb_str_to_cstr(mrb, msg));
  mrb->exc = NULL;
}

static mrb_value rand_module(void) {
  struct RClass *sonic_pi = mrb_module_get(mrb, "SonicPi");
  return mrb_obj_value(mrb_module_get_under(mrb, sonic_pi, "Rand"));
}

// Boots the interpreter and loads the runtime. Returns 0 when it is ready.
SP_EXPORT int sp_init(void) {
  mrb = mrb_open();
  if (!mrb) return 1;
  mrb_load_irep(mrb, sp_runtime_irep);
  if (mrb->exc) { report_exception("loading the runtime"); return 2; }
  return 0;
}

// Hands one random table over as the bytes of its wav file.
SP_EXPORT int sp_install_table(const char *source, const uint8_t *bytes, int len) {
  int ai = mrb_gc_arena_save(mrb);
  mrb_value tables = mrb_funcall(mrb, rand_module(), "tables", 0);
  mrb_value wav = mrb_str_new(mrb, (const char *)bytes, len);
  mrb_value chunk = mrb_funcall(mrb, tables, "data_chunk", 1, wav);
  mrb_funcall(mrb, tables, "install", 2, mrb_symbol_value(mrb_intern_cstr(mrb, source)), chunk);
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) { report_exception("installing a table"); return 1; }
  return 0;
}

// A synth's metadata (the .json beside its .scsyndef; runtime/lib/sonic_pi/synth_meta.rb): installed as a synth the
// runtime knows, played and checked as a built-in is. "" when it is, else why not, in words for whoever wrote the file.
SP_EXPORT const char *sp_install_synth(const char *json) {
  static char said[512];
  int ai = mrb_gc_arena_save(mrb);
  struct RClass *sonic_pi = mrb_module_get(mrb, "SonicPi");
  mrb_value meta = mrb_obj_value(mrb_module_get_under(mrb, sonic_pi, "SynthMeta"));
  mrb_funcall(mrb, meta, "install", 1, mrb_str_new_cstr(mrb, json));
  said[0] = 0;
  if (mrb->exc) {
    mrb_value msg = mrb_funcall(mrb, mrb_obj_value(mrb->exc), "message", 0);
    snprintf(said, sizeof said, "%s", mrb_str_to_cstr(mrb, msg));
    mrb->exc = NULL;
  }
  mrb_gc_arena_restore(mrb, ai);
  return said;
}

// Tells the runtime where the built-in samples live (the name programs see
// as SAMPLES_DIR) and describes one sound file the host knows about. The
// onsets are in seconds; pass none when unknown.
SP_EXPORT int sp_set_samples_dir(const char *dir) {
  int ai = mrb_gc_arena_save(mrb);
  struct RClass *sonic_pi = mrb_module_get(mrb, "SonicPi");
  mrb_value samples = mrb_obj_value(mrb_module_get_under(mrb, sonic_pi, "Samples"));
  mrb_funcall(mrb, samples, "builtin_dir=", 1, mrb_str_new_cstr(mrb, dir));
  mrb_const_set(mrb, mrb_obj_value(mrb->object_class), mrb_intern_cstr(mrb, "SAMPLES_DIR"), mrb_str_new_cstr(mrb, dir));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) { report_exception("setting the samples dir"); return 1; }
  return 0;
}

SP_EXPORT int sp_install_sample(const char *path, int num_frames, int num_chans, int sample_rate, const double *onsets, int num_onsets) {
  int ai = mrb_gc_arena_save(mrb);
  struct RClass *sonic_pi = mrb_module_get(mrb, "SonicPi");
  mrb_value samples = mrb_obj_value(mrb_module_get_under(mrb, sonic_pi, "Samples"));
  mrb_value p = mrb_str_new_cstr(mrb, path);
  mrb_funcall(mrb, samples, "install", 4, p, mrb_int_value(mrb, num_frames), mrb_int_value(mrb, num_chans), mrb_int_value(mrb, sample_rate));
  if (onsets) {
    mrb_value list = mrb_ary_new_capa(mrb, num_onsets);
    for (int i = 0; i < num_onsets; i++) mrb_ary_push(mrb, list, mrb_float_value(mrb, onsets[i]));
    mrb_funcall(mrb, samples, "install_onsets", 2, p, list);
  }
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) { report_exception("installing a sample"); return 1; }
  return 0;
}

// Runs a program and returns its trace as JSON. The string is the host's to
// read until the next call.
SP_EXPORT const char *sp_trace(const char *code, const char *file) {
  int ai = mrb_gc_arena_save(mrb);
  struct RClass *sonic_pi = mrb_module_get(mrb, "SonicPi");
  mrb_value adapter = mrb_obj_value(mrb_class_get_under(mrb, sonic_pi, "Adapter"));
  mrb_value json = mrb_funcall(mrb, adapter, "trace", 2, mrb_str_new_cstr(mrb, code), mrb_str_new_cstr(mrb, file));
  free(last_result);
  if (mrb->exc) {
    mrb_value msg = mrb_funcall(mrb, mrb_obj_value(mrb->exc), "inspect", 0);
    const char *m = mrb_str_to_cstr(mrb, msg);
    last_result = malloc(strlen(m) + 64);
    sprintf(last_result, "{\"host_error\":\"%s\"}", m);
    mrb->exc = NULL;
  } else {
    last_result = strdup(mrb_str_to_cstr(mrb, json));
  }
  mrb_gc_arena_restore(mrb, ai);
  return last_result;
}

SP_EXPORT const char *sp_version(void) { return MRUBY_VERSION; }

// ── RT ─────────────────────────────────────────────────────────────────────
// A live session: programs run as jobs against the host's clock, and every
// record (a sound, a sample to load, output, log, an error, a thread starting
// or sleeping) leaves the moment it is made, as OSC in the outbox below. The
// host owns the one real wait: sp_tick(now) runs what is due and says when to
// call next.

// The line of the running program a record comes from: the innermost frame
// compiled from that program's file (the runtime's own bytecode carries no
// debug info, so its frames are passed over). nil outside the program.
static mrb_value native_line(mrb_state *m, mrb_value self) {
  const char *file;
  mrb_get_args(m, "z", &file);
  for (ptrdiff_t i = m->c->ci - m->c->cibase; i >= 0; i--) {
    const mrb_callinfo *ci = &m->c->cibase[i];
    if (!ci->proc || MRB_PROC_CFUNC_P(ci->proc) || !ci->pc) continue;
    const mrb_irep *irep = ci->proc->body.irep;
    if (!irep || !irep->debug_info) continue;
    uint32_t idx = (uint32_t)(&ci->pc[-1] - irep->iseq);
    const char *fn = mrb_debug_get_filename(m, irep, idx);
    if (fn && strcmp(fn, file) == 0) {
      int32_t line = mrb_debug_get_line(m, irep, idx);
      return line > 0 ? mrb_int_value(m, line) : mrb_nil_value();
    }
  }
  return mrb_nil_value();
}

// ── The audio stream ───────────────────────────────────────────────────────
//
// A sound leaves the runtime as the OSC bundle the engine plays, encoded here
// from the record's own values into an outbox that the host reads after each
// call (sp_out_ptr, sp_out_len) and hands on as it is. The outbox empties at
// the start of every call that can fill it. A frame is [u32 size, u32 kind]
// (little-endian, as wasm is) and one OSC packet:
//
//   kind 1, a sound:     /sonic-pi/sound ,iib synthdef buffer bundle
//                        what must be loaded before the bundle can play (the
//                        runtime's numbers, -1 for nothing), then the bundle:
//                        /s_new, /n_set or /n_free at the sound's time
//   kind 2, to the host: /sonic-pi/synthdef ,is    number name   load it
//                        /sonic-pi/sample ,is      bufnum file   load it
//                        /sonic-pi/sample_free ,is bufnum file   free it
//   kind 3, for the page: a record (see "The GUI stream")
//
// Numbers are typed as Sonic Pi types them: an Integer as int32 (float32 when
// it does not fit), a Float as float32. web/osc.js reads all of this.
typedef struct { uint8_t *p; size_t len, cap; } sp_bytes;
static sp_bytes outbox, tags, data, message, bundle, packet;

static void put(sp_bytes *b, const void *src, size_t n) {
  if (b->len + n > b->cap) {
    size_t cap = b->cap ? b->cap : 1024;
    while (cap < b->len + n) cap *= 2;
    b->p = realloc(b->p, cap);
    b->cap = cap;
  }
  memcpy(b->p + b->len, src, n);
  b->len += n;
}

static void put_u32(sp_bytes *b, uint32_t v) {
  uint8_t x[4] = { (uint8_t)(v >> 24), (uint8_t)(v >> 16), (uint8_t)(v >> 8), (uint8_t)v };
  put(b, x, 4);
}

static void put_i32(sp_bytes *b, mrb_int v) { put_u32(b, (uint32_t)(int32_t)v); }

static void put_f32(sp_bytes *b, float f) {
  uint32_t v;
  memcpy(&v, &f, 4);
  put_u32(b, v);
}

// an OSC string: its bytes, then one to four NULs to a multiple of four
static void put_str(sp_bytes *b, const char *s, size_t n) {
  static const uint8_t nul[4];
  put(b, s, n);
  put(b, nul, 4 - n % 4);
}

static void put_tag(char t) { put(&tags, &t, 1); }

static void begin_message(const char *fixed_tags) {
  tags.len = 0;
  data.len = 0;
  put_tag(',');
  put(&tags, fixed_tags, strlen(fixed_tags));
}

// A synth's opt as its name and its number (buf as the buffer's number);
// anything that is not a number has no place in the message.
static int put_opt(mrb_state *m, mrb_value key, mrb_value val, void *ud) {
  mrb_int bufnum = *(mrb_int *)ud;
  const char *name;
  mrb_int len;
  if (mrb_symbol_p(key)) name = mrb_sym_name_len(m, mrb_symbol(key), &len);
  else if (mrb_string_p(key)) { name = RSTRING_PTR(key); len = RSTRING_LEN(key); }
  else return 0;
  if (len == 3 && memcmp(name, "buf", 3) == 0) {
    if (bufnum < 0) return 0;
    put_tag('s'); put_str(&data, name, (size_t)len);
    put_tag('i'); put_i32(&data, bufnum);
  } else if (mrb_float_p(val)) {
    put_tag('s'); put_str(&data, name, (size_t)len);
    put_tag('f'); put_f32(&data, (float)mrb_float(val));
  } else if (mrb_integer_p(val)) {
    mrb_int v = mrb_integer(val);
    put_tag('s'); put_str(&data, name, (size_t)len);
    if (v >= INT32_MIN && v <= INT32_MAX) { put_tag('i'); put_i32(&data, v); }
    else { put_tag('f'); put_f32(&data, (float)v); }
  }
  return 0;
}

static void frame(uint32_t kind, const sp_bytes *b) {
  uint32_t head[2] = { (uint32_t)b->len, kind };
  put(&outbox, head, 8);
  put(&outbox, b->p, b->len);
}

// The message gathered, as a bundle for its time, framed as a sound.
static void send_sound(double time, const char *address, mrb_int synthdef, mrb_int bufnum) {
  message.len = 0;
  put_str(&message, address, strlen(address));
  put_str(&message, (const char *)tags.p, tags.len);
  put(&message, data.p, data.len);
  bundle.len = 0;
  put(&bundle, "#bundle", 8);                 // with its NUL
  if (time <= 0) {                            // OSC's "immediately" (Scheduler::IMMEDIATE)
    put_u32(&bundle, 0);
    put_u32(&bundle, 1);
  } else {
    double secs = floor(time);
    put_u32(&bundle, (uint32_t)secs);         // NTP: seconds, and a fraction of 2^32
    put_u32(&bundle, (uint32_t)((time - secs) * 4294967296.0));
  }
  put_u32(&bundle, (uint32_t)message.len);
  put(&bundle, message.p, message.len);
  packet.len = 0;
  put_str(&packet, "/sonic-pi/sound", 15);
  put_str(&packet, ",iib", 4);
  put_i32(&packet, synthdef);
  put_i32(&packet, bufnum);
  put_u32(&packet, (uint32_t)bundle.len);
  put(&packet, bundle.p, bundle.len);         // a bundle is a multiple of four already
  frame(1, &packet);
}

// Native.s_new(time, synthdef, bufnum, synth, node, opts[, action, target]):
// a synth starts, at the head of the root group unless told where.
static mrb_value native_s_new(mrb_state *m, mrb_value self) {
  mrb_float time;
  mrb_int synthdef, bufnum, node, action = 0, target = 0;
  const char *synth;
  mrb_value opts;
  mrb_get_args(m, "fiiziH|ii", &time, &synthdef, &bufnum, &synth, &node, &opts, &action, &target);
  begin_message("siii");
  put_str(&data, synth, strlen(synth));
  put_i32(&data, node);
  put_i32(&data, action);
  put_i32(&data, target);
  mrb_hash_foreach(m, mrb_hash_ptr(opts), put_opt, &bufnum);
  send_sound(time, "/s_new", synthdef, bufnum);
  return mrb_nil_value();
}

// Native.n_set(time, bufnum, node, opts): new values for a running synth.
// False, with nothing sent, when no opt is a number.
static mrb_value native_n_set(mrb_state *m, mrb_value self) {
  mrb_float time;
  mrb_int bufnum, node;
  mrb_value opts;
  mrb_get_args(m, "fiiH", &time, &bufnum, &node, &opts);
  begin_message("i");
  put_i32(&data, node);
  mrb_hash_foreach(m, mrb_hash_ptr(opts), put_opt, &bufnum);
  if (tags.len == 2) return mrb_false_value();
  send_sound(time, "/n_set", -1, bufnum);
  return mrb_true_value();
}

// Native.n_free(time, node): a running synth stops.
static mrb_value native_n_free(mrb_state *m, mrb_value self) {
  mrb_float time;
  mrb_int node;
  mrb_get_args(m, "fi", &time, &node);
  begin_message("i");
  put_i32(&data, node);
  send_sound(time, "/n_free", -1, -1);
  return mrb_nil_value();
}

// Native.n_run(time, node, flag): a synth paused (0) or run again (1), as native's node.pause and node.run.
static mrb_value native_n_run(mrb_state *m, mrb_value self) {
  mrb_float time;
  mrb_int node, flag;
  mrb_get_args(m, "fii", &time, &node, &flag);
  begin_message("ii");
  put_i32(&data, node);
  put_i32(&data, flag);
  send_sound(time, "/n_run", -1, -1);
  return mrb_nil_value();
}

// Native.g_new(time, group, action, target): a group, for a with_fx block.
static mrb_value native_g_new(mrb_state *m, mrb_value self) {
  mrb_float time;
  mrb_int group, action, target;
  mrb_get_args(m, "fiii", &time, &group, &action, &target);
  begin_message("iii");
  put_i32(&data, group);
  put_i32(&data, action);
  put_i32(&data, target);
  send_sound(time, "/g_new", -1, -1);
  return mrb_nil_value();
}

// Native.n_order(time, action, target, node): a node moves (a live loop
// into another with_fx).
static mrb_value native_n_order(mrb_state *m, mrb_value self) {
  mrb_float time;
  mrb_int action, target, node;
  mrb_get_args(m, "fiii", &time, &action, &target, &node);
  begin_message("iii");
  put_i32(&data, action);
  put_i32(&data, target);
  put_i32(&data, node);
  send_sound(time, "/n_order", -1, -1);
  return mrb_nil_value();
}

// Native.host(address, number, name): word for the host (kind 2).
static mrb_value native_host(mrb_state *m, mrb_value self) {
  const char *address, *name;
  mrb_int number;
  mrb_get_args(m, "ziz", &address, &number, &name);
  packet.len = 0;
  put_str(&packet, address, strlen(address));
  put_str(&packet, ",is", 3);
  put_i32(&packet, number);
  put_str(&packet, name, strlen(name));
  frame(2, &packet);
  return mrb_nil_value();
}

// ── The GUI stream ─────────────────────────────────────────────────────────
//
// Every record the page shows leaves as OSC too, kind 3 in the outbox:
//
//   /sonic-pi/<kind> ,i d i i|N fields...
//
// the thread's uid, the record's time on the host's clock, its job and its
// program line (N for none), then what the kind carries (scheduler.rb sends
// each kind; web/gui-stream.js reads them back). Values keep their Ruby
// types: a Float is a double (N when not finite), an Integer int32 (int64
// when it does not fit), a String or Symbol a string, true T, false F, nil N,
// an Array an OSC array [...], a Hash an array of its keys and values, and
// anything else its to_s.
static void put_f64(sp_bytes *b, double d) {
  uint64_t bits;
  memcpy(&bits, &d, 8);
  put_u32(b, (uint32_t)(bits >> 32));
  put_u32(b, (uint32_t)bits);
}

static void put_value(mrb_state *m, mrb_value v, int depth);

static int put_pair(mrb_state *m, mrb_value key, mrb_value val, void *ud) {
  int depth = *(int *)ud;
  put_value(m, key, depth);
  put_value(m, val, depth);
  return 0;
}

// a string up to any NUL in it: OSC strings end at the first
static void put_string_value(const char *s, mrb_int len) {
  put_tag('s');
  put_str(&data, s, strnlen(s, (size_t)len));
}

static void put_value(mrb_state *m, mrb_value v, int depth) {
  if (mrb_nil_p(v)) put_tag('N');
  else if (mrb_type(v) == MRB_TT_FALSE) put_tag('F');
  else if (mrb_type(v) == MRB_TT_TRUE) put_tag('T');
  else if (mrb_float_p(v)) {
    double d = mrb_float(v);
    if (isfinite(d)) { put_tag('d'); put_f64(&data, d); }
    else put_tag('N');
  } else if (mrb_integer_p(v)) {
    mrb_int i = mrb_integer(v);
    if (i >= INT32_MIN && i <= INT32_MAX) { put_tag('i'); put_i32(&data, i); }
    else {
      uint64_t u = (uint64_t)(int64_t)i;
      put_tag('h');
      put_u32(&data, (uint32_t)(u >> 32));
      put_u32(&data, (uint32_t)u);
    }
  } else if (mrb_string_p(v)) {
    put_string_value(RSTRING_PTR(v), RSTRING_LEN(v));
  } else if (mrb_symbol_p(v)) {
    mrb_int len;
    const char *s = mrb_sym_name_len(m, mrb_symbol(v), &len);
    put_string_value(s, len);
  } else if (depth < 8 && mrb_array_p(v)) {
    put_tag('[');
    for (mrb_int i = 0; i < RARRAY_LEN(v); i++) put_value(m, RARRAY_PTR(v)[i], depth + 1);
    put_tag(']');
  } else if (depth < 8 && mrb_hash_p(v)) {
    int next = depth + 1;
    put_tag('[');
    mrb_hash_foreach(m, mrb_hash_ptr(v), put_pair, &next);
    put_tag(']');
  } else {
    mrb_value s = mrb_obj_as_string(m, v);
    put_string_value(RSTRING_PTR(s), RSTRING_LEN(s));
  }
}

// Native.gui(kind, uid, time, job, line, fields...): one record for the page.
static mrb_value native_gui(mrb_state *m, mrb_value self) {
  static const uint8_t nul[4];
  mrb_sym kind;
  mrb_int uid, job, n;
  mrb_float time;
  mrb_value line;
  const mrb_value *fields;
  mrb_get_args(m, "nifio*!", &kind, &uid, &time, &job, &line, &fields, &n);
  tags.len = 0;
  data.len = 0;
  put_tag(',');
  put_tag('i'); put_i32(&data, uid);
  put_tag('d'); put_f64(&data, time);
  put_tag('i'); put_i32(&data, job);
  if (mrb_integer_p(line)) { put_tag('i'); put_i32(&data, mrb_integer(line)); }
  else put_tag('N');
  for (mrb_int i = 0; i < n; i++) put_value(m, fields[i], 0);
  mrb_int len;
  const char *name = mrb_sym_name_len(m, kind, &len);
  packet.len = 0;
  put(&packet, "/sonic-pi/", 10);
  put(&packet, name, (size_t)len);
  put(&packet, nul, 4 - (10 + (size_t)len) % 4);
  put_str(&packet, (const char *)tags.p, tags.len);
  put(&packet, data.p, data.len);
  frame(3, &packet);
  return mrb_nil_value();
}

static mrb_value live_module(void) {
  struct RClass *sonic_pi = mrb_module_get(mrb, "SonicPi");
  return mrb_obj_value(mrb_module_get_under(mrb, sonic_pi, "Live"));
}

// Starts (or restarts) a live session. Returns 0 when ready.
SP_EXPORT int sp_live_boot(void) {
  outbox.len = 0;
  struct RClass *sonic_pi = mrb_module_get(mrb, "SonicPi");
  struct RClass *native = mrb_define_module_under(mrb, sonic_pi, "Native");
  mrb_define_module_function(mrb, native, "gui", native_gui, MRB_ARGS_ANY());
  mrb_define_module_function(mrb, native, "line", native_line, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, native, "s_new", native_s_new, MRB_ARGS_ARG(6, 2));
  mrb_define_module_function(mrb, native, "n_set", native_n_set, MRB_ARGS_REQ(4));
  mrb_define_module_function(mrb, native, "n_free", native_n_free, MRB_ARGS_REQ(2));
  mrb_define_module_function(mrb, native, "n_run", native_n_run, MRB_ARGS_REQ(3));
  mrb_define_module_function(mrb, native, "g_new", native_g_new, MRB_ARGS_REQ(4));
  mrb_define_module_function(mrb, native, "n_order", native_n_order, MRB_ARGS_REQ(4));
  mrb_define_module_function(mrb, native, "host", native_host, MRB_ARGS_REQ(3));
  int ai = mrb_gc_arena_save(mrb);
  mrb_funcall(mrb, live_module(), "boot_native", 0);
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) { report_exception("booting live"); return 1; }
  return 0;
}

// Runs a program as a new job starting at now (the host's clock, seconds).
// Its head runs at once, up to its first sleep. Returns the job id, or -1.
SP_EXPORT int sp_run(const char *code, double now) {
  outbox.len = 0;
  int ai = mrb_gc_arena_save(mrb);
  mrb_value id = mrb_funcall(mrb, live_module(), "run", 2, mrb_str_new_cstr(mrb, code), mrb_float_value(mrb, now));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) { report_exception("running"); return -1; }
  return (int)mrb_integer(id);
}

// A run in a group (Scheduler#stop_group stops a group as one): the job's id, or -1.
SP_EXPORT int sp_run_group(const char *code, double now, int group) {
  outbox.len = 0;
  int ai = mrb_gc_arena_save(mrb);
  mrb_value id = mrb_funcall(mrb, live_module(), "run", 3, mrb_str_new_cstr(mrb, code), mrb_float_value(mrb, now), mrb_int_value(mrb, group));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) { report_exception("running"); return -1; }
  return (int)mrb_integer(id);
}

// A group stops: its threads now, its sounds turned down over `fade` seconds and freed as the fade ends.
SP_EXPORT void sp_stop_group(int group, double fade, double now) {
  outbox.len = 0;
  int ai = mrb_gc_arena_save(mrb);
  mrb_funcall(mrb, live_module(), "stop_group", 3, mrb_int_value(mrb, group), mrb_float_value(mrb, fade), mrb_float_value(mrb, now));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) report_exception("stopping a group");
}

// A subtree stops: a thread (a run's, a live loop's) with everything under it, or an fx block's threads and sounds.
SP_EXPORT void sp_stop_subtree(int uid, double fade, double now) {
  outbox.len = 0;
  int ai = mrb_gc_arena_save(mrb);
  mrb_funcall(mrb, live_module(), "stop_subtree", 3, mrb_int_value(mrb, uid), mrb_float_value(mrb, fade), mrb_float_value(mrb, now));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) report_exception("stopping a subtree");
}

// A group sits under another: stopping the parent stops it too.
SP_EXPORT void sp_group_under(int group, int parent) {
  int ai = mrb_gc_arena_save(mrb);
  mrb_funcall(mrb, live_module(), "group_under", 2, mrb_int_value(mrb, group), mrb_int_value(mrb, parent));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) report_exception("nesting a group");
}

// Runs every thread due by now. Returns when to call again on the same
// clock, or -1 when nothing is waiting.
SP_EXPORT double sp_tick(double now) {
  outbox.len = 0;
  int ai = mrb_gc_arena_save(mrb);
  mrb_value next = mrb_funcall(mrb, live_module(), "tick", 1, mrb_float_value(mrb, now));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) { report_exception("ticking"); return -1; }
  if (mrb_nil_p(next)) return -1;
  return mrb_float_p(next) ? mrb_float(next) : (double)mrb_integer(next);
}

// A spec's horizon for a live session (runtime/bin/live-check.mjs): a thread
// whose sleep takes it past this many seconds of its run's logical time
// stops there, as in a trace. Negative clears it; the app never sets one.
SP_EXPORT void sp_live_stop_after(double seconds) {
  int ai = mrb_gc_arena_save(mrb);
  mrb_funcall(mrb, live_module(), "stop_after=", 1, seconds < 0 ? mrb_nil_value() : mrb_float_value(mrb, seconds));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) report_exception("setting the horizon");
}

// Link's tempo, from the page (its Link strip, tap tempo): it changes at `at`
// on the host's clock (as it sounds), as a program's set_link_bpm! does.
SP_EXPORT void sp_set_link_bpm(double bpm, double at) {
  int ai = mrb_gc_arena_save(mrb);
  mrb_funcall(mrb, live_module(), "set_link_bpm", 2, mrb_float_value(mrb, bpm), mrb_float_value(mrb, at));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) report_exception("setting Link's tempo");
}

// A cue from outside the program (MIDI in, a game controller, from the page): its address, its values each
// tagged with its kind ("i60\x1fi100", "sfake_pad"; adapter.rb host_values), at `now` on the session's clock
// (Scheduler#external_cue); the syncs waiting on it wake at the next tick.
SP_EXPORT void sp_cue(const char *address, const char *nums, double now) {
  outbox.len = 0;   // this call records the cue, so it empties the outbox first, as every filling call does
  int ai = mrb_gc_arena_save(mrb);
  mrb_funcall(mrb, live_module(), "external_cue", 3, mrb_str_new_cstr(mrb, address), mrb_str_new_cstr(mrb, nums), mrb_float_value(mrb, now));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) report_exception("an incoming cue");
}

// The global time warp, in seconds: every message to the engine that much
// later, or earlier when negative.
SP_EXPORT void sp_set_time_warp(double seconds) {
  int ai = mrb_gc_arena_save(mrb);
  mrb_funcall(mrb, live_module(), "time_warp=", 1, mrb_float_value(mrb, seconds));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) report_exception("setting the time warp");
}

// The session's schedule-ahead (SonicPi.sched_ahead: DEFAULT_SCHED_AHEAD until set_sched_ahead_time! sets it), for the
// host to time itself by (live-core.js) without a number of its own that could drift from it.
SP_EXPORT double sp_sched_ahead(void) {
  int ai = mrb_gc_arena_save(mrb);
  mrb_value v = mrb_funcall(mrb, mrb_obj_value(mrb_module_get(mrb, "SonicPi")), "sched_ahead", 0);
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) { report_exception("reading the schedule-ahead"); return 0.05; }
  return mrb_float_p(v) ? mrb_float(v) : mrb_integer_p(v) ? (double)mrb_integer(v) : 0.05;
}

// The host lost this many seconds (a held page, suspended audio, a clock jump):
// every pending event moves that much later, as a pause would.
SP_EXPORT void sp_hold(double seconds) {
  int ai = mrb_gc_arena_save(mrb);
  mrb_funcall(mrb, live_module(), "hold", 1, mrb_float_value(mrb, seconds));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) report_exception("holding the schedule");
}

// The process table: every live (or just finished) thread as a fixed row of
// doubles in one buffer on the wasm heap, which the page reads as a typed
// array. Row layout is web/sonic_pi.js's PROCESS_FIELDS. Returns the buffer;
// sp_process_table_len says how many doubles it holds. Valid until the next
// call.
static double *table_buf;
static mrb_int table_cap, table_len;
SP_EXPORT const double *sp_process_table(double now) {
  int ai = mrb_gc_arena_save(mrb);
  mrb_value rows = mrb_funcall(mrb, live_module(), "process_table", 1, mrb_float_value(mrb, now));
  table_len = 0;
  if (mrb->exc) {
    report_exception("process table");
  } else if (mrb_array_p(rows)) {
    mrb_int n = RARRAY_LEN(rows);
    if (n > table_cap) {
      table_cap = n * 2;
      table_buf = realloc(table_buf, sizeof(double) * (size_t)table_cap);
    }
    for (mrb_int i = 0; i < n; i++) {
      mrb_value v = RARRAY_PTR(rows)[i];
      table_buf[i] = mrb_float_p(v) ? mrb_float(v) : mrb_integer_p(v) ? (double)mrb_integer(v) : -1.0;
    }
    table_len = n;
  }
  mrb_gc_arena_restore(mrb, ai);
  return table_buf;
}

SP_EXPORT int sp_process_table_len(void) { return (int)table_len; }

// One job stops where it stands; the others carry on.
SP_EXPORT void sp_stop_job(int job) {
  outbox.len = 0;
  int ai = mrb_gc_arena_save(mrb);
  mrb_funcall(mrb, live_module(), "stop_job", 1, mrb_int_value(mrb, job));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) report_exception("stopping a job");
}

// Every job stops where it stands.
SP_EXPORT void sp_stop_all(void) {
  outbox.len = 0;
  int ai = mrb_gc_arena_save(mrb);
  mrb_funcall(mrb, live_module(), "stop_all", 0);
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) report_exception("stopping");
}

// The audio stream the last call left (see "The audio stream"): where it
// starts on the heap, and how many bytes it holds.
SP_EXPORT const uint8_t *sp_out_ptr(void) { return outbox.p; }
SP_EXPORT int sp_out_len(void) { return (int)outbox.len; }

// The synthdef a built-in synth (fx = 0) or fx (fx = 1) plays, by the name a program uses for it: what the runtime
// itself asks the engine for, so a host preloading a program's synths loads those and not a guess (:sine plays
// sonic-pi-beep). "" for a name that is not built in: a program's own synths are loaded from where it says.
SP_EXPORT const char *sp_synthdef_for(const char *name, int fx) {
  static char out[128];
  out[0] = 0;
  int ai = mrb_gc_arena_save(mrb);
  struct RClass *data = mrb_module_get_under(mrb, mrb_module_get(mrb, "SonicPi"), "Data");
  mrb_value table = mrb_const_get(mrb, mrb_obj_value(data), mrb_intern_cstr(mrb, fx ? "FX" : "SYNTHS"));
  mrb_value info = mrb_hash_get(mrb, table, mrb_symbol_value(mrb_intern_cstr(mrb, name)));
  if (mrb_hash_p(info)
      && !mrb_test(mrb_hash_get(mrb, info, mrb_symbol_value(mrb_intern_lit(mrb, "metadata"))))
      && !mrb_test(mrb_hash_get(mrb, info, mrb_symbol_value(mrb_intern_lit(mrb, "external"))))) {
    mrb_value n = mrb_hash_get(mrb, info, mrb_symbol_value(mrb_intern_lit(mrb, "scsynth_name")));
    if (mrb_string_p(n)) snprintf(out, sizeof out, "%s", mrb_str_to_cstr(mrb, n));
  }
  mrb_gc_arena_restore(mrb, ai);
  return out;
}

// A sample file's engine buffer number, numbering it if it has none: for a
// host that loads samples before a program asks for them. -1 on failure.
SP_EXPORT int sp_buffer_for(const char *file) {
  int ai = mrb_gc_arena_save(mrb);
  struct RClass *sonic_pi = mrb_module_get(mrb, "SonicPi");
  mrb_value ids = mrb_obj_value(mrb_module_get_under(mrb, sonic_pi, "EngineIds"));
  mrb_value n = mrb_funcall(mrb, ids, "claim_buffer", 1, mrb_str_new_cstr(mrb, file));
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) { report_exception("numbering a buffer"); return -1; }
  return (int)mrb_integer(n);
}
