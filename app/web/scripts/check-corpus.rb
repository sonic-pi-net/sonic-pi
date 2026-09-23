#!/usr/bin/env ruby
# SPDX-License-Identifier: AGPL-3.0-or-later
# Every piece of Sonic Pi code this repository carries, run with Safe mode on.
#
#   ruby scripts/check-corpus.rb                                   # the runtime under MRI
#   ADAPTER="$PWD/build/runtime/sp-trace" ruby scripts/check-corpus.rb   # under mruby itself
#
# The specs say the runtime plays what Sonic Pi plays; this says the checks do not stand in the way of real music.
# Safe mode (native's use_arg_checks) refuses an opt whose value its synth will not take, and a rule that is wrong
# or too tight would show here first: as an example that no longer plays, or a tutorial snippet that stops on a
# value it has always had. The specs under specs/ that use a bad value on purpose are the only expected refusals,
# and they are named rather than counted, so a new one cannot hide among them.
#
# It exits non-zero on a refusal that is not one of those.
require "json"
require "open3"

ROOT = File.expand_path("..", __dir__)
ADAPTER = ENV["ADAPTER"] || "ruby #{File.join(ROOT, 'runtime/bin/trace.rb')}"

# the specs whose whole point is a value Sonic Pi refuses
EXPECTED = %w[
  play/note_extremes play/slide_opts play/use_arg_checks sample/rate_zero sample/start_finish_out_of_range
].flat_map { |s| ["specs/#{s}.rb", "specs/bpm/#{s}.rb"] }.to_set

def pieces
  out = []
  Dir[File.join(ROOT, "specs/**/*.rb")].sort.each { |f| out << [f.sub(ROOT + "/", ""), File.read(f)] }
  Dir[File.join(ROOT, "../../etc/examples/**/*.rb")].sort.each { |f| out << ["example #{File.basename(f)}", File.read(f)] }
  Dir[File.join(ROOT, "../../etc/doc/tutorial/*.md")].sort.each do |f|
    File.read(f).scan(/```\n(.*?)```/m).each_with_index { |(code), i| out << ["tutorial #{File.basename(f)} ##{i}", code] }
  end
  out
end

require "set"
work = File.join(ROOT, "build/corpus")
Dir.mkdir(File.join(ROOT, "build")) unless Dir.exist?(File.join(ROOT, "build"))
Dir.mkdir(work) unless Dir.exist?(work)
all = pieces
puts "#{all.length} pieces, Safe mode on, through #{ADAPTER}"

refused = []
all.each_with_index do |(name, code), i|
  path = File.join(work, "piece.rb")
  File.write(path, "use_arg_checks true\n" + code)
  out, _err, _status = Open3.capture3(*ADAPTER.split(" "), path)
  line = out.lines.find { |l| l.start_with?("{") }
  next unless line
  trace = JSON.parse(line) rescue next
  (trace["errors"] || []).each do |e|
    next unless e["class"] == "SonicPi::OptError"
    refused << [name, e["message"]] unless EXPECTED.include?(name)
  end
  $stderr.print "\r#{i + 1}/#{all.length}" if $stderr.tty?   # a log that is not a terminal gets the verdict, not a counter
end
$stderr.print "\r" if $stderr.tty?

if refused.empty?
  puts "nothing Safe mode would not play"
  exit 0
end
puts "#{refused.length} refused:"
refused.first(40).each { |n, m| puts format("  %-44s %s", n, m) }
puts "  … and #{refused.length - 40} more" if refused.length > 40
exit 1
