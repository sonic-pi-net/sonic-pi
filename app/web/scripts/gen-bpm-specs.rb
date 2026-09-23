#!/usr/bin/env ruby
# SPDX-License-Identifier: AGPL-3.0-or-later
# Writes specs/bpm: every spec again at twice its tempo, so what Sonic Pi
# measures in beats (sleeps, envelopes, slides, fx phases and decays,
# durations, beat_stretch, at, time_warp, sample_duration, rt and bt) is
# checked to follow the tempo, and what it measures in seconds is checked
# not to. Each copy sets `use_bpm 120` after its leading comments and
# doubles every literal `use_bpm N` and `with_bpm N`; relative tempo
# (use_bpm_mul, density) and horizons (seconds) stay as they are. The spec's own
# comments are copied as they are, the tempo line saying where it came from. The oracle
# then says what the copy must do (scripts/gen-expected.rb specs/bpm).
#
#   scripts/gen-bpm-specs.rb            # write specs/bpm
#   scripts/gen-bpm-specs.rb --check    # fail when a copy has drifted from its spec
require "fileutils"
ROOT = File.expand_path("..", __dir__)
SPECS = File.join(ROOT, "specs")
OUT = File.join(SPECS, "bpm")
check = ARGV.include?("--check")

def double_tempo(src, rel)
  lines = src.lines
  head = lines.take_while { |l| l.start_with?("#") }
  body = lines.drop(head.size)
  return nil if head.any? { |l| l.start_with?("# expected: by hand") }
  body = body.map do |l|
    next l if l.lstrip.start_with?("#")
    l.gsub(/\b(use_bpm|with_bpm)(\s*\(?\s*)(\d+(?:\.\d+)?)/) do
      n = $3.include?(".") ? ($3.to_f * 2).to_s : ($3.to_i * 2).to_s
      "#{$1}#{$2}#{n}"
    end
  end
  # the spec's own comments as they are (its description, horizon, racy note: nothing moved, nothing joined), and the
  # tempo on one line of its own that says what it is, so the program keeps the same line numbers as it always has
  [*head, "use_bpm 120   # at twice the tempo: #{rel}\n", *body].join
end

specs = Dir[File.join(SPECS, "**/*.rb")].reject { |f| f.start_with?(OUT + "/") }.sort
drift = []
written = 0
specs.each do |spec|
  rel = spec.sub(SPECS + "/", "")
  copy = double_tempo(File.read(spec), "specs/#{rel}") or next
  dest = File.join(OUT, rel)
  if check
    drift << rel unless File.exist?(dest) && File.read(dest) == copy
  else
    FileUtils.mkdir_p(File.dirname(dest)) rescue (require "fileutils"; FileUtils.mkdir_p(File.dirname(dest)))
    File.write(dest, copy)
    written += 1
  end
end
if check
  puts drift.empty? ? "specs/bpm matches the specs" : "drifted from their specs: #{drift.join(', ')}"
  exit drift.empty? ? 0 : 1
end
puts "#{written} specs written to specs/bpm"
