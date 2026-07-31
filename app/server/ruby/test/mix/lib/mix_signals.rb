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

require 'tmpdir'
require 'digest'

module SonicPi
  # Deterministic test signals, written to disk as float32 wavs for the
  # engine to play back. Everything here is seeded or analytic: the same
  # signal renders identically on every machine, so a measurement that moves
  # means the mixer moved.
  module MixSignals
    module_function

    SAMPLE_RATE = 48_000

    def db(v)
      10.0 ** (v / 20.0)
    end

    # A steady tone. The reference case: anything but a transparent path
    # shows up immediately as harmonics.
    def sine(freq: 1000.0, dbfs: -6.0, duration: 2.0)
      amp = db(dbfs)
      generate(duration) { |i| v = amp * Math.sin(2 * Math::PI * freq * i / SAMPLE_RATE); [v, v] }
    end

    # Two closely spaced tones. Non-linearity turns these into sum and
    # difference products, which is the measurable form of "mud".
    def two_tone(f1: 220.0, f2: 247.0, dbfs: -6.0, duration: 2.0)
      amp = db(dbfs) / 2
      generate(duration) do |i|
        v = amp * (Math.sin(2 * Math::PI * f1 * i / SAMPLE_RATE) +
                   Math.sin(2 * Math::PI * f2 * i / SAMPLE_RATE))
        [v, v]
      end
    end

    # Logarithmic sweep for frequency response and for catching anything the
    # safety filters do at the extremes.
    def sweep(from: 20.0, to: 20_000.0, dbfs: -12.0, duration: 4.0)
      amp = db(dbfs)
      n = (duration * SAMPLE_RATE).to_i
      k = Math.log(to / from)
      generate(duration) do |i|
        t = i.to_f / n
        phase = 2 * Math::PI * from * duration * (Math.exp(k * t) - 1) / k
        v = amp * Math.sin(phase)
        [v, v]
      end
    end

    # The case that actually matters here: a hard transient riding a
    # sustained bass note, at drum & bass tempo. High crest factor, most of
    # the energy low, which is exactly the material that makes a full-band
    # limiter duck the whole mix on every kick.
    def dnb_burst(bpm: 174.0, bass_freq: 55.0, dbfs: -3.0, duration: 4.0)
      amp = db(dbfs)
      beat = 60.0 / bpm * SAMPLE_RATE
      generate(duration) do |i|
        bass = 0.55 * Math.sin(2 * Math::PI * bass_freq * i / SAMPLE_RATE)
        pos = i % beat
        env = Math.exp(-pos / (0.012 * SAMPLE_RATE))
        # Alternate kick and snare-ish hits so the transients aren't all
        # identical and the meter ballistics get exercised.
        hit = if ((i / beat).to_i.even?)
                Math.sin(2 * Math::PI * 62 * pos / SAMPLE_RATE)
              else
                noise_at(i) * 0.8 + Math.sin(2 * Math::PI * 190 * pos / SAMPLE_RATE) * 0.4
              end
        v = amp * (bass + 0.9 * env * hit)
        # A touch of stereo difference so stereo-link behaviour is visible.
        [v, v * 0.92 + amp * 0.08 * bass]
      end
    end

    # Full-scale square: worst case for inter-sample peaks, and the signal
    # that separates a real true-peak reading from a sample-peak one.
    def square(freq: 997.0, dbfs: -1.0, duration: 2.0)
      amp = db(dbfs)
      generate(duration) do |i|
        v = amp * (Math.sin(2 * Math::PI * freq * i / SAMPLE_RATE) >= 0 ? 1.0 : -1.0)
        [v, v]
      end
    end

    def silence(duration: 1.0)
      generate(duration) { [0.0, 0.0] }
    end

    # Deterministic value noise: a hashed index rather than Random, so it is
    # identical across Ruby versions and platforms.
    def noise_at(i)
      h = Digest::MD5.digest("mix-noise-#{i}").unpack1("l>")
      h / 2_147_483_648.0
    end

    def generate(duration)
      n = (duration * SAMPLE_RATE).to_i
      (0...n).map { |i| yield(i) }
    end

    # Writes samples to a float32 wav and returns the path. Named after a
    # digest of the content so repeated renders of the same signal reuse the
    # file instead of rewriting it.
    def write(samples, name)
      require_wavefile
      path = File.join(Dir.tmpdir, "sonic-pi-mix-signal-#{name}.wav")
      format = WaveFile::Format.new(:stereo, :float_32, SAMPLE_RATE)
      WaveFile::Writer.new(path, format) do |writer|
        writer.write(WaveFile::Buffer.new(samples, format))
      end
      path
    end

    def require_wavefile
      return if defined?(WaveFile)
      root = File.expand_path("../../../../../..", __dir__)
      $LOAD_PATH.unshift File.join(root, "app/server/ruby/vendor/wavefile-0.8.1/lib")
      require 'wavefile'
    end
  end
end
