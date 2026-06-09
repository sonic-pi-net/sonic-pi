#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

# End-to-end verification of `use_bpm :midi` against a REAL SuperSonic engine.
#
# Boots the native SuperSonic binary, runs a live-loop that fires `midi` calls
# at `sleep 0.25` under `use_bpm :midi, "test"`, then changes that midi
# timeline's tempo *inside SuperClock* (via OSC) mid-loop and asserts the
# scheduled message timestamps shift accordingly (0.25 beat: 125 ms @120 BPM →
# 62.5 ms @240 BPM). The clock is resolved entirely by the engine over the
# `/clock/midi:<port>/*` OSC contract — exactly the path the spider uses live.
#
# Not part of the unit suite (needs the built engine). Run directly:
#   ruby test/integration/test_use_bpm_midi_live.rb
# Override the engine binary with SUPERSONIC_BIN=/path/to/SuperSonic.

require "socket"

STDOUT.sync = true   # exit! below skips buffer flush; keep output live

RB = File.expand_path("../..", __dir__)            # app/server/ruby
$LOAD_PATH.unshift(File.join(RB, "lib"))
require File.join(RB, "test", "setup_test")

PORT = 57999

def find_engine
  return ENV["SUPERSONIC_BIN"] if ENV["SUPERSONIC_BIN"] && File.executable?(ENV["SUPERSONIC_BIN"])
  candidates = [
    File.expand_path("../../../external/supersonic/build/native/SuperSonic_artefacts/Release/SuperSonic", RB),
    File.expand_path("~/Development/supersonic/build/native/SuperSonic_artefacts/Release/SuperSonic")
  ]
  candidates.find { |c| File.executable?(c) }
end

# Records the scheduled timestamp of every MIDI message the spider emits.
class MidiRecorder
  attr_reader :sends
  def initialize; @sends = []; end
  def midi_send_at(t, path, *); @sends << [t, path]; end
  def method_missing(*); end
  def respond_to_missing?(*); true; end
end

bin = find_engine
abort "SKIP: SuperSonic engine binary not found (set SUPERSONIC_BIN)" unless bin

engine = spawn(bin, "-u", PORT.to_s, "-i", "0", out: "/tmp/ss_engine_test.log", err: "/tmp/ss_engine_test.log")
at_exit { Process.kill("TERM", engine) rescue nil }

# Wait for the engine to answer a clock RPC.
link = nil
60.times do
  sleep 0.25
  link ||= SonicPi::LinkAPI.new("127.0.0.1", PORT,
            internal_cue: ->(*){}, updated_link_num_peers: ->(*){}, updated_link_bpm: ->(*){})
  break if link.link_tempo(true) && link.link_current_time != 0
end

lang = SonicPi::MockLang.new
st = lang.mod_sound_studio
[:cent_tuning, :transpose].each { |m| st.define_singleton_method(m) { 0 } unless st.respond_to?(m) }
rec = MidiRecorder.new
lang.instance_variable_set(:@link_api, link)
lang.instance_variable_set(:@midi_api, rec)

comms = link.instance_variable_get(:@link_comms)
comms.send("/clock/midi:test/tempo/set", 120.0)   # claim the timeline @120
sleep 1.0                                          # let the link/clock time-delta settle

lang.run do
  use_bpm :midi, "test"
  8.times do |i|
    midi 60                                        # a real MIDI message each beat-fraction
    comms.send("/clock/midi:test/tempo/set", 240.0) if i == 3   # change tempo IN SuperClock
    sleep 0.25
  end
end

ons    = rec.sends.select { |_t, p| p.to_s.include?("note_on") }.map(&:first)
deltas = ons.each_cons(2).map { |a, b| ((b - a) * 1000).round(1) }   # ms between messages
before = deltas[0..1].sum / 2.0
after  = deltas[-2..-1].sum / 2.0

puts "note_on timestamps captured: #{ons.size}"
puts "inter-message gaps (ms):     #{deltas.inspect}"
puts "avg gap @120 BPM: #{before.round(1)} ms (expect ~125)"
puts "avg gap @240 BPM: #{after.round(1)} ms (expect ~62.5)"

ok = ons.size >= 6 &&
     (before - 125.0).abs < 25 &&
     (after  -  62.5).abs < 20 &&
     after < before * 0.7
puts ok ? "PASS: message timestamps tracked the SuperClock MIDI tempo change" \
        : "FAIL: timestamps did not track the tempo change"
Process.kill("TERM", engine) rescue nil
exit!(ok ? 0 : 1)
