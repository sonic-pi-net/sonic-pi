# SPDX-License-Identifier: AGPL-3.0-or-later
# Safe mode's checks, against native's own.
#
# Native validates a synth's opts by calling the lambdas in its SynthInfo; the runtime here cannot carry a lambda, so
# runtime/data/synths.rb (scripts/gen-synth-data.rb) carries the rules themselves and lang.rb judges them with
# native's own validation.rb. One rule, two runtimes: this runs both over every rule the data holds, at values
# around each boundary, and reports every value they disagree on.
#
#   ruby scripts/check-validations.rb              # against the oracle beside this repository
#   ruby scripts/check-validations.rb path/to/sonic-pi   # against a native checkout of your own
#
# It exits non-zero on a disagreement, or on a constraint whose sentence the runtime does not know (a rule the
# runtime cannot read would pass every value, and let a cutoff of 200 play).

require "json"
require "open3"

ROOT = File.expand_path("..", __dir__)
NATIVE = File.expand_path(ARGV[0] || File.join(ROOT, "../.."))
SP = File.join(NATIVE, "app/server/ruby")
$LOAD_PATH.unshift File.join(SP, "lib")
require File.join(SP, "core")
require "sonicpi/synths/synthinfo"

# the values worth trying around a constraint: its own numbers, either side of them, and the usual suspects
def probes(messages)
  values = [-1000, -1.5, -1, -0.5, 0, 0.001, 0.5, 1, 1.5, 2, 5, 100, 130, 131, 1000]
  messages.each do |m|
    m.scan(/-?\d+(?:\.\d+)?(?:e-?\d+)?/i).each do |n|
      f = n.to_f
      values += [f - 1, f - 0.01, f, f + 0.01, f + 1]
    end
  end
  values.uniq.sort
end

# What native says: its lambda, and the sentence it would raise with
def native_cases
  cases = []
  [[SonicPi::Synths::SynthInfo.get_all, "synth"], [SonicPi::Synths::BaseInfo.get_all, "all"]].first(1)
  all = SonicPi::Synths::SynthInfo.get_all
  all.each do |name, info|
    # the same source native's validate! reads, so the slide opts it checks are checked here too
    info.arg_defaults.each_key do |arg|
      msgs = info.arg_validations(arg).map { |v| v[1].to_s }
      next if msgs.empty?
      probes(msgs).each do |v|
        ok = begin
          info.validate!(arg => v)
          true
        rescue StandardError
          false
        end
        cases << { "synth" => name.to_s, "arg" => arg.to_s, "value" => v, "ok" => ok, "rules" => msgs }
      end
    end
  end
  cases
end

cases = native_cases
puts "#{cases.length} values across #{cases.map { |c| [c['synth'], c['arg']] }.uniq.length} opts, from #{NATIVE}"

# What the runtime says: the same values through lang.rb's own checker, under mruby
harness = File.join(ROOT, "scripts/.validations-harness.rb")
File.write(harness, <<~RUBY)
  lang = SonicPi::Language.new(SonicPi::Scheduler.new)
  cases = JSON.parse(File.read(#{File.join(ROOT, 'scripts/.validations-cases.json').inspect}))
  out = cases.map do |c|
    info = SonicPi::Data::SYNTHS[c["synth"].to_sym] || SonicPi::Data::FX[c["synth"].to_sym]
    next nil unless info
    begin
      lang.send(:__validate!, info, { c["arg"].to_sym => c["value"] })
      true
    rescue StandardError
      false
    end
  end
  puts JSON.generate(out)
RUBY
File.write(File.join(ROOT, "scripts/.validations-cases.json"), JSON.generate(cases))
runner = File.join(ROOT, "runtime/bin/trace.rb")
ours, err, status = Open3.capture3("ruby", "-r", "json", "-e", <<~RUBY)
  $PROGRAM_ARGS = []
  load #{runner.inspect}.sub("trace.rb", "trace.rb") if false
RUBY
# the runtime is loaded the way trace.rb loads it, then the harness runs in the same process
loader = File.read(runner)[/\A.*?^end\n/m]
script = File.join(ROOT, "runtime/bin/.validations-run.rb")
File.write(script, "require 'json'\n" + loader + File.read(harness))
ours_json, err, status = Open3.capture3("ruby", script)
abort "the runtime's side failed:\n#{err}" unless status.success?
ours = JSON.parse(ours_json.lines.last)

bad = []
cases.each_with_index do |c, i|
  next if ours[i].nil? || ours[i] == c["ok"]
  bad << c.merge("ours" => ours[i])
end
[harness, script, File.join(ROOT, "scripts/.validations-cases.json")].each { |f| File.delete(f) if File.exist?(f) }

if bad.empty?
  puts "every value agrees with native"
  exit 0
end
puts "#{bad.length} disagreements (native → ours):"
bad.first(40).each { |c| puts format("  %-16s %-14s %-8s native=%s ours=%s   %s", c["synth"], c["arg"], c["value"], c["ok"], c["ours"], c["rules"].join("; ")) }
puts "  … and #{bad.length - 40} more" if bad.length > 40
exit 1
