# SPDX-License-Identifier: AGPL-3.0-or-later
# The runtime as a command: prints the trace of a program. Runs under mruby
# (an installed one, or our host build) and under ruby.
#
#   mruby runtime/bin/trace.rb specs/random/rrand.rb
#   ADAPTER="mruby runtime/bin/trace.rb" ruby scripts/check.rb specs/random
LIB = File.join(File.dirname(__FILE__), "../lib/sonic_pi")
SERVER_LIB = File.join(File.dirname(__FILE__), "../../../server/ruby/lib/sonicpi")   # the rules, where Sonic Pi keeps them
DATA = File.join(File.dirname(__FILE__), "../data")
[[LIB, "errors"], [LIB, "defaults"], [SERVER_LIB, "validation"], [LIB, "float_format"], [LIB, "ring"], [LIB, "rand"], [LIB, "rand_verbs"], [LIB, "note"], [DATA, "synths"], [DATA, "theory"],
 [DATA, "samples"], [LIB, "theory"], [LIB, "samples"], [LIB, "audio_meta"], [LIB, "cue_history"], [LIB, "scheduler"], [LIB, "lang"], [LIB, "lang_more"], [LIB, "adapter"]].each do |dir, f|
  path = File.join(dir, "#{f}.rb")
  eval(File.open(path, "rb") { |io| io.read }, nil, path, 1)
end
SonicPi::Rand.tables.load_dir(File.join(File.dirname(__FILE__), "../../../../etc/buffers"))
# The built-in samples: the oracle's folder, described by their headers.
samples_dir = File.expand_path(File.join(File.dirname(__FILE__), "../../../../etc/samples"))
SonicPi::Samples.builtin_dir = samples_dir
SonicPi::Data::ONSETS.each_key do |file|
  path = File.join(samples_dir, file)
  meta = SonicPi::AudioMeta.read(path)
  SonicPi::Samples.install(path, meta[:num_frames], meta[:num_chans], meta[:sample_rate])
  SonicPi::Samples.install_onsets(path, SonicPi::Data::ONSETS[file])
end
Object.const_set(:SAMPLES_DIR, samples_dir)
file = ARGV[0]
STDOUT.write(SonicPi::Adapter.trace(File.open(file, "rb") { |io| io.read }, file) + "\n")
