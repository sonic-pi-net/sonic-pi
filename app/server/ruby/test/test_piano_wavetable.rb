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

# The :piano wavetable ships as raw 16-bit integers and reaches the engine as
# an audio file. The header written around it has to be one the engine's
# codecs read as exactly the samples the asset holds.
require_relative "./setup_test"
require_relative "../lib/sonicpi/studio"
require 'tmpdir'

module SonicPi
  class PianoWavetableTester < Minitest::Test
    def with_asset
      Dir.mktmpdir do |dir|
        dat = File.join(dir, "piano_wavetable.dat")
        samples = (0...1000).map { |i| ((i * 37) % 65536) - 32768 }
        File.binwrite(dat, samples.pack("s<*"))
        yield dir, dat, samples
      end
    end

    def test_the_wav_wraps_the_samples_unchanged
      with_asset do |dir, dat, samples|
        wav = Studio.write_piano_wavetable_wav(dat, File.join(dir, "out.wav"))
        bytes = File.binread(wav)
        assert_equal "RIFF", bytes[0, 4]
        assert_equal "WAVE", bytes[8, 4]
        assert_equal "fmt ", bytes[12, 4]
        fmt = bytes[20, 16].unpack("vvVVvv")
        assert_equal [1, 1], fmt[0, 2], "PCM, mono"
        assert_equal 16, fmt[5], "16-bit"
        assert_equal "data", bytes[36, 4]
        assert_equal samples.size * 2, bytes[40, 4].unpack1("V")
        assert_equal samples, bytes[44..].unpack("s<*")
        assert_equal bytes.bytesize - 8, bytes[4, 4].unpack1("V")
      end
    end

    def test_a_current_wav_is_left_alone_and_a_stale_one_rewritten
      with_asset do |dir, dat, samples|
        wav = File.join(dir, "out.wav")
        Studio.write_piano_wavetable_wav(dat, wav)
        first = File.mtime(wav)
        sleep 0.01
        Studio.write_piano_wavetable_wav(dat, wav)
        assert_equal first, File.mtime(wav), "rewritten though nothing changed"

        File.binwrite(dat, (samples + [7]).pack("s<*"))
        File.utime(Time.now + 5, Time.now + 5, dat)
        Studio.write_piano_wavetable_wav(dat, wav)
        assert_equal samples.size + 1, (File.binread(wav).bytesize - 44) / 2
      end
    end
  end
end
