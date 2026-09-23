#!/usr/bin/env ruby
# SPDX-License-Identifier: AGPL-3.0-or-later
# Generates <spec>.expected.json beside every spec by running it through the
# oracle, in parallel. Specs run in real time, so keep them short.
#
#   scripts/gen-expected.rb                 # every spec
#   scripts/gen-expected.rb specs/sample    # one directory, or files
#   JOBS=4 scripts/gen-expected.rb
#
# A spec whose leading comments say `# expected: by hand` pins where the
# runtime means to differ from Sonic Pi (specs/tau); its trace is written by
# hand and never by the oracle.
require "json"
require "open3"
ROOT = File.expand_path("..", __dir__)
HARNESS = File.join(ROOT, "oracle/harness/oracle.rb")
targets = ARGV.empty? ? [File.join(ROOT, "specs")] : ARGV
by_hand = ->(spec) { File.foreach(spec).take_while { |l| l.start_with?("#") }.any? { |l| l.start_with?("# expected: by hand") } }
specs = targets.flat_map { |t| File.directory?(t) ? Dir[File.join(t, "**/*.rb")] : [t] }.sort.reject(&by_hand)
jobs = (ENV["JOBS"] || 8).to_i
queue = Queue.new; specs.each { |s| queue << s }
failures = []; mu = Mutex.new
Array.new(jobs) do
  Thread.new do
    loop do
      spec = (queue.pop(true) rescue break)
      out, err, status = Open3.capture3("ruby", HARNESS, spec)
      if status.success? && (JSON.parse(out) rescue nil)
        File.write(spec.sub(/\.rb\z/, ".expected.json"), out)
        mu.synchronize { puts "ok   #{spec.sub(ROOT + '/', '')}" }
      else
        mu.synchronize { failures << spec; puts "FAIL #{spec}\n#{err}#{out}" }
      end
    end
  end
end.each(&:join)
puts "#{specs.size - failures.size}/#{specs.size} traces written"
exit failures.empty? ? 0 : 1
