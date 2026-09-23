// SPDX-License-Identifier: AGPL-3.0-or-later
// The runtime's native helpers, as an mrbgem so both the host and the wasm
// build carry them.
//
// SonicPi::Native.float_to_s(f): the shortest decimal that reads back as
// the same double, laid out the way CRuby's Float#to_s does. mruby 4.1 finds
// the same digits but switches to exponent form from 1e15 where CRuby waits
// for 1e16; this matches CRuby on 400,000 values. The round-trip search uses
// the C library's strtod and snprintf, which are correctly rounded.
#include <mruby.h>
#include <mruby/string.h>
#include <mruby/array.h>
#include <mruby/compile.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static mrb_value float_to_s(mrb_state *mrb, mrb_value self) {
  mrb_float f;
  mrb_get_args(mrb, "f", &f);
  char buf[64], out[64];
  if (isnan(f)) return mrb_str_new_cstr(mrb, "NaN");
  if (isinf(f)) return mrb_str_new_cstr(mrb, f > 0 ? "Infinity" : "-Infinity");
  if (f == 0.0) return mrb_str_new_cstr(mrb, signbit(f) ? "-0.0" : "0.0");
  int p;
  for (p = 1; p <= 17; p++) {
    snprintf(buf, sizeof buf, "%.*e", p - 1, f);
    if (strtod(buf, NULL) == f) break;
  }
  // buf is d.ddd…e±xx (or d e±xx); take the digits and the exponent
  char digits[32];
  int nd = 0, exp = 0;
  const char *s = buf;
  if (*s == '-') s++;
  for (; *s && *s != 'e'; s++) if (*s != '.') digits[nd++] = *s;
  digits[nd] = 0;
  if (*s == 'e') exp = atoi(s + 1);
  while (nd > 1 && digits[nd - 1] == '0') digits[--nd] = 0;
  const char *sign = f < 0 ? "-" : "";
  // CRuby: exponent form below 1e-4, from 1e16, and for a 16-digit integer part with nothing after the point
  if (exp < -4 || exp >= 16 || (exp == 15 && nd <= 16)) {
    if (nd > 1) snprintf(out, sizeof out, "%s%c.%se%c%02d", sign, digits[0], digits + 1, exp < 0 ? '-' : '+', abs(exp));
    else snprintf(out, sizeof out, "%s%c.0e%c%02d", sign, digits[0], exp < 0 ? '-' : '+', abs(exp));
  } else if (exp < 0) {
    snprintf(out, sizeof out, "%s0.", sign);
    for (int i = 0; i < -exp - 1; i++) strcat(out, "0");
    strcat(out, digits);
  } else if (nd <= exp + 1) {
    snprintf(out, sizeof out, "%s%s", sign, digits);
    for (int i = 0; i < exp + 1 - nd; i++) strcat(out, "0");
    strcat(out, ".0");
  } else {
    snprintf(out, sizeof out, "%s%.*s.%s", sign, exp + 1, digits, digits + exp + 1);
  }
  return mrb_str_new_cstr(mrb, out);
}

/* Where a syntax error is: [line, column] of the parser's first error, or nil. mruby's eval says only the line
   (SyntaxError "line 4: ..."); its parser keeps the column, which the error card's caret needs. */
static mrb_value syntax_at(mrb_state *mrb, mrb_value self) {
  const char *code; mrb_int len;
  mrb_get_args(mrb, "s", &code, &len);
  mrb_ccontext *cxt = mrb_ccontext_new(mrb);
  cxt->capture_errors = TRUE;
  cxt->no_exec = TRUE;
  struct mrb_parser_state *p = mrb_parse_nstring(mrb, code, (size_t)len, cxt);
  mrb_value at = mrb_nil_value();
  if (p && p->nerr > 0) at = mrb_assoc_new(mrb, mrb_fixnum_value(p->error_buffer[0].lineno), mrb_fixnum_value(p->error_buffer[0].column));
  if (p) mrb_parser_free(p);
  mrb_ccontext_free(mrb, cxt);
  return at;
}

void mrb_sonic_pi_core_gem_init(mrb_state *mrb) {
  struct RClass *sonic_pi = mrb_define_module(mrb, "SonicPi");
  struct RClass *native = mrb_define_module_under(mrb, sonic_pi, "Native");
  mrb_define_module_function(mrb, native, "float_to_s", float_to_s, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, native, "syntax_at", syntax_at, MRB_ARGS_REQ(1));
}

void mrb_sonic_pi_core_gem_final(mrb_state *mrb) {}
