#!/usr/bin/env ruby
# SPDX-License-Identifier: AGPL-3.0-or-later
# Runs every spec through an adapter and compares its trace with the oracle's
# expected trace. With no ADAPTER this re-runs the oracle, which checks the
# specs are deterministic.
#
#   scripts/check.rb                          # all specs, oracle adapter
#   ADAPTER="..." scripts/check.rb specs/play # a runtime under test
#   STRICT_LOG=1 scripts/check.rb             # also require the runtime log to match
#
# events, errors and output must match exactly; the runtime log (synth echoes,
# warnings) is reported but only fails the spec with STRICT_LOG=1.
require "json"
require "open3"
ROOT = File.expand_path("..", __dir__)
adapter = ENV["ADAPTER"] || "ruby #{File.join(ROOT, 'oracle/harness/oracle.rb')}"
targets = ARGV.empty? ? [File.join(ROOT, "specs")] : ARGV
specs = targets.flat_map { |t| File.directory?(t) ? Dir[File.join(t, "**/*.rb")] : [t] }.sort
# the oracle cannot pass what a spec pins as the runtime's own (`# expected: by hand`)
by_hand = ->(spec) { File.foreach(spec).take_while { |l| l.start_with?("#") }.any? { |l| l.start_with?("# expected: by hand") } }
# `# racy: <why>` — a spec whose trace the oracle does not always produce the same way (its own threads, not the
# program's). Named in the spec, with the reason, so that a retry here is a known quantity rather than a habit.
def racy(spec) = File.foreach(spec).take_while { |l| l.start_with?("#") }.any? { |l| l.start_with?("# racy:") }
specs = specs.reject(&by_hand) unless ENV["ADAPTER"]
jobs = (ENV["JOBS"] || 8).to_i
STRICT = %w[events errors output] + (ENV["STRICT_LOG"] ? %w[log] : [])

# An fx's free is timed by the oracle's wall clock, not by the logical time everything else here is measured in, so
# it lands a few milliseconds after the time meant and a spec allows for that. A machine with nothing spare (a CI
# runner, a laptop building something else) can be later still: re-running the oracle there wants FREE_TOLERANCE
# raised, while a runtime under test, whose own times are logical, is held to the few milliseconds.
FREE_TOLERANCE = (ENV["FREE_TOLERANCE"] || 0.03).to_f

def same_item(x, y)
  return x == y unless x.is_a?(Hash) && y.is_a?(Hash) && x["kind"] == "fx_free" && y["kind"] == "fx_free"
  (x["t"] - y["t"]).abs <= FREE_TOLERANCE && x.reject { |k, _| k == "t" } == y.reject { |k, _| k == "t" }
end

def diff(expected, actual)
  lines = []
  (expected.keys | actual.keys).each do |k|
    e, a = expected[k], actual[k]
    next if e == a
    missing = (e || []).reject { |x| (a || []).any? { |y| same_item(x, y) } }
    extra = (a || []).reject { |y| (e || []).any? { |x| same_item(x, y) } }
    next if missing.empty? && extra.empty?
    lines << "  #{k}:"
    missing.each { |x| lines << "    - #{x.to_json}" }
    extra.each { |x| lines << "    + #{x.to_json}" }
  end
  lines
end

queue = Queue.new; specs.each { |s| queue << s }
results = {}; mu = Mutex.new
Array.new(jobs) do
  Thread.new do
    loop do
      spec = (queue.pop(true) rescue break)
      name = spec.sub(ROOT + "/", "")
      exp_path = spec.sub(/\.rb\z/, ".expected.json")
      unless File.exist?(exp_path)
        mu.synchronize { results[name] = [:missing, ["  no expected trace; run scripts/gen-expected.rb"]] }
        next
      end
      # A spec the oracle races on (`# racy: <why>`) is run again before its failure is believed: native uses real
      # threads, so the same program can trace two ways there. A runtime under test has no such excuse, and gets
      # one attempt — a retry would only hide what the specs are for.
      tries = (racy(spec) && !ENV["ADAPTER"]) ? 3 : 1
      attempt = 0
      loop do
        attempt += 1
        out, err, status = Open3.capture3("#{adapter} #{spec}")
        actual = (JSON.parse(out) rescue nil)
        unless status.success? && actual
          mu.synchronize { results[name] = [:error, (err + out).lines.first(8).map { |l| "  " + l.chomp }] }
          break
        end
        expected = JSON.parse(File.read(exp_path))
        hard = diff(expected.slice(*STRICT), actual.slice(*STRICT))
        soft = STRICT.include?("log") ? [] : diff(expected.slice("log"), actual.slice("log"))
        state = hard.empty? ? (soft.empty? ? :pass : :pass_log_differs) : :fail
        if state == :fail && attempt < tries
          next
        end
        lines = hard + soft
        lines = ["  (the oracle raced: it agreed on attempt #{attempt} of #{tries})"] + lines if state != :fail && attempt > 1
        mu.synchronize { results[name] = [state, lines] }
        break
      end
    end
  end
end.each(&:join)

counts = Hash.new(0)
results.sort.each do |name, (status, lines)|
  counts[status] += 1
  label = { pass: "pass", pass_log_differs: "pass (log differs)", fail: "FAIL", error: "ERROR", missing: "MISSING" }[status]
  puts "#{label.ljust(18)} #{name}"
  puts lines unless status == :pass
end
puts
puts counts.map { |k, v| "#{v} #{k}" }.join(", ")
exit (counts[:fail] + counts[:error] + counts[:missing]).zero? ? 0 : 1
