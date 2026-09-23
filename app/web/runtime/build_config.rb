# SPDX-License-Identifier: AGPL-3.0-or-later
# The runtime's mruby: one gem set, built twice. `host` is the native
# interpreter and mrbc for tests and for compiling the runtime to bytecode;
# `emscripten` is the library the wasm links against.
#
#   MRUBY_CONFIG=runtime/build_config.rb MRUBY_BUILD_DIR=build/mruby rake -C runtime/mruby
#
# mruby 4.1's stdlib gembox brings the core Regexp gem. Rational is in for
# logical time, though the runtime may not keep it.
COMMON_GEMS = lambda do |conf|
  conf.gembox "stdlib"
  conf.gembox "stdlib-ext"
  conf.gembox "math"
  conf.gembox "metaprog"          # instance_eval with a string: how a program runs
  conf.gem core: "mruby-rational"
  conf.gem core: "mruby-bigint"
  conf.gem File.expand_path("mrbgems/sonic-pi-core", File.dirname(__FILE__))   # exact float printing
end

MRuby::Build.new("host") do |conf|
  conf.toolchain
  COMMON_GEMS.call(conf)
  conf.gembox "stdlib-io"         # File, for reading the random tables natively
  conf.gem core: "mruby-bin-mrbc"
  conf.gem core: "mruby-bin-mruby"
  conf.gem core: "mruby-bin-config"
end

# The wasm target needs Emscripten. A build that only wants the native runtime (scripts/build-runtime.sh --native,
# and the CI job that runs the specs through mruby on each operating system) says so, and asks for nothing it has
# no compiler for.
unless ENV["SP_HOST_ONLY"]
MRuby::CrossBuild.new("emscripten") do |conf|
  conf.toolchain :emscripten
  COMMON_GEMS.call(conf)
  conf.gem core: "mruby-bin-config"           # a host binary: the flags the wasm link must match
  # wasm32 is a 32-bit target; Sonic Pi needs 64-bit integers (sample
  # frames, NTP seconds), which on 32 bits means unboxed values.
  conf.cc.defines << "MRB_INT64" << "MRB_NO_BOXING"
  conf.cc.flags << "-O2"
end
end
