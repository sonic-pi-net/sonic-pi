# SPDX-License-Identifier: AGPL-3.0-or-later
# The runtime's mruby: one gem set, built twice. `host` is the native
# interpreter and mrbc for tests and for compiling the runtime to bytecode;
# `emscripten` is the library the wasm links against.
#
#   MRUBY_CONFIG=runtime/build_config.rb MRUBY_BUILD_DIR=build/mruby rake -C build/mruby-src
#
# build/mruby-src is the submodule with runtime/mruby-patches over it, as scripts/build-runtime.sh makes it.
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
  # macOS only: SP_MRUBY_ARCHS="arm64 x86_64" builds libmruby for both, which a universal plugin links
  # (app/plugin/scripts/build.sh --universal). Unset, the build is for this machine's architecture.
  ENV.fetch("SP_MRUBY_ARCHS", "").split.each do |arch|
    conf.cc.flags << "-arch" << arch
    conf.linker.flags << "-arch" << arch
  end
  # The gems a program gets are the web's and no more. This is the mruby a plugin and an app link, and whatever is
  # in it every program can use: with mruby's io gems (File, Dir, IO.popen, sockets, ENV) a stranger's code had the
  # run of the machine. The host hands the runtime its tables and sound files (sp_install_table, runtime_host.h);
  # runtime/host/runtime_host_check.cpp holds a program to finding none of them.
  COMMON_GEMS.call(conf)
  conf.gem core: "mruby-bin-mrbc"
  conf.gem core: "mruby-bin-config"
end

# The mruby the fuzzer runs programs on (scripts/build-fuzz.sh, runtime/fuzz): the gems a program gets in a plugin,
# under ASan and UBSan with libFuzzer's coverage, and with the VM's per-instruction hook so the harness can end a
# program that never would. Only built when asked for.
fuzz_build = lambda do |name, compile, link|
  MRuby::Build.new(name) do |conf|
    conf.toolchain :clang
    COMMON_GEMS.call(conf)
    conf.gem core: "mruby-bin-config"
    conf.cc.defines << "MRB_USE_DEBUG_HOOK"
    conf.cc.flags.concat(compile + %w[-fno-omit-frame-pointer -g -O1])
    conf.linker.flags.concat(link)
  end
end
if ENV["SP_FUZZ"]
  fuzz_build.call("fuzz", %w[-fsanitize=fuzzer-no-link,address,undefined -fno-sanitize-recover=undefined], %w[-fsanitize=address,undefined])
end
# The same without the sanitizers and with clang's line counters, to see what a corpus reaches (scripts/fuzz-coverage.sh).
if ENV["SP_FUZZ_COVERAGE"]
  fuzz_build.call("fuzz-coverage", %w[-fprofile-instr-generate -fcoverage-mapping], %w[-fprofile-instr-generate])
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
