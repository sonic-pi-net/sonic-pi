#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

# Measures the main mixer and prints a report. Run with:
#
#   ruby app/server/ruby/test/mix/mix_report.rb
#   ruby app/server/ruby/test/mix/mix_report.rb --mixer path/to/candidate.scsyndef
#
# Every number comes from a real render through the real mixer synthdef in a
# headless engine, so a proposed change can be A/B'd against the shipping
# chain instead of reasoned about.

require_relative "lib/mix_engine"
require_relative "lib/mix_signals"
require_relative "lib/mix_analysis"

A = SonicPi::MixAnalysis
S = SonicPi::MixSignals

mixer = nil
if (i = ARGV.index("--mixer"))
  mixer = ARGV[i + 1]
end

def fmt(v, unit = "dB")
  return "   -inf" unless v.finite?
  "%7.2f#{unit}" % v
end

engine = SonicPi::MixEngine.new
engine.start
engine.load_mixer(*[mixer].compact)
puts "mixer: #{mixer || 'etc/synthdefs/compiled/sonic-pi-mixer.scsyndef'}"
puts "engine: headless SuperSonic @ #{SonicPi::MixEngine::SAMPLE_RATE} Hz"
puts

begin
  # ---------------------------------------------------------------- transparency
  puts "== Path transparency (1 kHz sine, -6 dBFS in) =="
  puts "%-22s %10s %10s %10s" % %w[config gain THD+N DC]
  sine = S.write(S.sine(freq: 1000.0, dbfs: -6.0, duration: 4.0), "sine1k")
  [["limiter active", {}],
   ["limiter bypassed", { limiter_bypass: 1 }],
   ["filters bypassed", { hpf_bypass: 1, lpf_bypass: 1 }]].each do |name, args|
    cap = engine.render(sine, 4.0, args).steady
    gain = A.db(A.sample_peak(cap.l)) - (-6.0)
    puts "%-22s %10s %10s %10s" % [name, fmt(gain), fmt(A.thd_n_db(cap.l, 1000.0, cap.sample_rate)),
                                   fmt(A.db(A.dc_offset(cap.l).abs))]
  end
  puts

  # ------------------------------------------------------------------ amp ladder
  puts "== Main volume dial (amp = dial/100 * 2) =="
  puts "%-22s %10s %10s %10s %10s %10s" % %w[dial peak true-peak LUFS-I GR-peak THD+N]
  [[50, 1.0], [80, 1.6], [100, 2.0]].each do |dial, amp|
    cap = engine.render(sine, 4.0, { amp: amp }).steady
    ref = engine.render(sine, 4.0, { amp: amp, limiter_bypass: 1 }).steady
    gr = A.gain_reduction_db(cap.l, ref.l, cap.sample_rate)
    puts "%-22s %10s %10s %10s %10s %10s" % [
      "#{dial}#{dial == 80 ? ' (default)' : ''}",
      fmt(A.db(A.sample_peak(cap.l))), fmt(A.db(A.true_peak(cap.l))),
      fmt(A.lufs_integrated(cap.channels, cap.sample_rate), "LUFS"),
      fmt(gr[:peak]), fmt(A.thd_n_db(cap.l, 1000.0, cap.sample_rate))]
  end
  puts

  # ------------------------------------------------------------------- programme
  puts "== Drum & bass programme (174 bpm, -3 dBFS peak in) =="
  puts "%-22s %9s %9s %9s %9s %9s %9s" % %w[drive peak true-pk LUFS-I crest GR-pk link-err]
  dnb = S.write(S.dnb_burst(duration: 6.0), "dnb")
  [["pre_amp 1.0 (unity)", 1.0], ["pre_amp 1.6", 1.6], ["pre_amp 2.5", 2.5], ["pre_amp 4.0", 4.0]].each do |name, pre|
    cap = engine.render(dnb, 6.0, { pre_amp: pre }).steady
    ref = engine.render(dnb, 6.0, { pre_amp: pre, limiter_bypass: 1 }).steady
    gr = A.gain_reduction_db(cap.l, ref.l, cap.sample_rate)
    link = A.stereo_link_error_db(cap.l, cap.r, ref.l, ref.r, cap.sample_rate)
    puts "%-22s %9s %9s %9s %9s %9s %9s" % [
      name,
      fmt(A.db(A.sample_peak(cap.l))), fmt(A.db(A.true_peak(cap.l))),
      fmt(A.lufs_integrated(cap.channels, cap.sample_rate), "LUFS"),
      fmt(A.crest_factor_db(cap.l)), fmt(gr[:peak]), fmt(link)]
  end
  puts

  # -------------------------------------------------------------------- two-tone
  puts "== Intermodulation (220 + 247 Hz) =="
  puts "%-22s %10s %10s" % %w[drive IMD GR-peak]
  tt = S.write(S.two_tone(duration: 4.0), "twotone")
  [["pre_amp 1.0", 1.0], ["pre_amp 2.0", 2.0], ["pre_amp 4.0", 4.0]].each do |name, pre|
    cap = engine.render(tt, 4.0, { pre_amp: pre }).steady
    ref = engine.render(tt, 4.0, { pre_amp: pre, limiter_bypass: 1 }).steady
    gr = A.gain_reduction_db(cap.l, ref.l, cap.sample_rate)
    puts "%-22s %10s %10s" % [name, fmt(A.imd_db(cap.l, 220.0, 247.0, cap.sample_rate)), fmt(gr[:peak])]
  end
  puts

  # ------------------------------------------------------------------ true peak
  puts "== Inter-sample peaks (997 Hz square, -1 dBFS in) =="
  sq = S.write(S.square(duration: 4.0), "square")
  cap = engine.render(sq, 4.0, {}).steady
  sp = A.db(A.sample_peak(cap.l))
  tp = A.db(A.true_peak(cap.l))
  puts "sample peak %s   true peak %s   understated by %s" % [fmt(sp), fmt(tp), fmt(tp - sp)]
ensure
  engine.stop
end
