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

module SonicPi
  module MixAnalysis
    module_function

    NEG_INF = -Float::INFINITY

    def db(v)
      v <= 0.0 ? NEG_INF : 20.0 * Math.log10(v)
    end

    def undb(d)
      10.0 ** (d / 20.0)
    end

    # ---------------------------------------------------------------- levels

    def sample_peak(ch)
      ch.map(&:abs).max || 0.0
    end

    def rms(ch)
      return 0.0 if ch.empty?
      Math.sqrt(ch.sum { |v| v * v } / ch.size)
    end

    def dc_offset(ch)
      return 0.0 if ch.empty?
      ch.sum / ch.size
    end

    # Peak-to-RMS in dB. Low crest means a dense, squashed mix; high crest
    # means the peaks are running far ahead of the level you actually hear,
    # which is why a peak meter alone can't tell you how loud something is.
    def crest_factor_db(ch)
      p = sample_peak(ch)
      r = rms(ch)
      return 0.0 if p <= 0.0 || r <= 0.0
      db(p) - db(r)
    end

    # Inter-sample peak, per ITU-R BS.1770-4: the reconstructed analogue
    # waveform between samples can exceed every sample, so a signal that a
    # sample-peak meter calls legal can still overload a converter. 4x
    # oversampled with a windowed-sinc polyphase FIR.
    OVERSAMPLE = 4
    TAPS_PER_PHASE = 32

    def polyphase_kernel
      @polyphase_kernel ||= begin
        len = OVERSAMPLE * TAPS_PER_PHASE
        centre = (len - 1) / 2.0
        taps = (0...len).map do |i|
          x = i - centre
          sinc = x.abs < 1e-9 ? 1.0 : Math.sin(Math::PI * x / OVERSAMPLE) / (Math::PI * x / OVERSAMPLE)
          # Blackman window keeps the stopband down so the interpolation
          # doesn't invent peaks of its own.
          w = 0.42 - 0.5 * Math.cos(2 * Math::PI * i / (len - 1)) +
              0.08 * Math.cos(4 * Math::PI * i / (len - 1))
          sinc * w
        end
        (0...OVERSAMPLE).map { |p| (0...TAPS_PER_PHASE).map { |t| taps[t * OVERSAMPLE + p] } }
      end
    end

    # Interpolating every sample is needlessly slow: an inter-sample peak can
    # only appear next to a sample that is already close to the maximum, so
    # only those neighbourhoods are oversampled.
    def true_peak(ch)
      return 0.0 if ch.empty?
      phases = polyphase_kernel
      sp = sample_peak(ch)
      return 0.0 if sp <= 0.0
      threshold = sp * undb(-3.0)
      candidates = []
      ch.each_with_index { |v, i| candidates << i if v.abs >= threshold }
      peak = sp
      candidates.each do |i|
        (i...(i + TAPS_PER_PHASE)).each do |j|
          next if j >= ch.size + TAPS_PER_PHASE
          phases.each do |taps|
            acc = 0.0
            taps.each_with_index do |c, t|
              idx = j - t
              acc += c * ch[idx] if idx >= 0 && idx < ch.size
            end
            a = acc.abs
            peak = a if a > peak
          end
        end
      end
      peak
    end

    # --------------------------------------------------------------- loudness

    # ITU-R BS.1770-4 K-weighting. The coefficients are defined at 48 kHz;
    # anything else needs re-derivation, so the harness renders at 48k and
    # this refuses to guess.
    PRE_B = [1.53512485958697, -2.69169618940638, 1.19839281085285].freeze
    PRE_A = [1.0, -1.69065929318241, 0.73248077421585].freeze
    RLB_B = [1.0, -2.0, 1.0].freeze
    RLB_A = [1.0, -1.99004745483398, 0.99007225036621].freeze

    def biquad(ch, b, a)
      x1 = x2 = y1 = y2 = 0.0
      ch.map do |x0|
        y0 = b[0] * x0 + b[1] * x1 + b[2] * x2 - a[1] * y1 - a[2] * y2
        x2, x1 = x1, x0
        y2, y1 = y1, y0
        y0
      end
    end

    def k_weight(ch, sample_rate)
      unless sample_rate == 48_000
        raise ArgumentError, "K-weighting coefficients are 48 kHz only (got #{sample_rate})"
      end
      biquad(biquad(ch, PRE_B, PRE_A), RLB_B, RLB_A)
    end

    # Mean square of each gating block, per channel, summed with the BS.1770
    # channel weights (1.0 for L and R). Blocks are 400 ms with 75% overlap.
    def gating_blocks(channels, sample_rate)
      weighted = channels.map { |ch| k_weight(ch, sample_rate) }
      block = (0.4 * sample_rate).to_i
      hop = block / 4
      n = weighted.first.size
      return [] if n < block
      starts = (0..(n - block)).step(hop).to_a
      starts.map do |s|
        weighted.sum do |ch|
          sum = 0.0
          (s...(s + block)).each { |i| sum += ch[i] * ch[i] }
          sum / block
        end
      end
    end

    def block_loudness(z)
      z <= 0.0 ? NEG_INF : -0.691 + 10.0 * Math.log10(z)
    end

    # Integrated loudness with both R128 gates: the absolute gate drops
    # silence, then the relative gate drops anything more than 10 LU below
    # what's left, so a quiet intro can't drag the number down.
    def lufs_integrated(channels, sample_rate)
      zs = gating_blocks(channels, sample_rate)
      return NEG_INF if zs.empty?
      above_abs = zs.select { |z| block_loudness(z) > -70.0 }
      return NEG_INF if above_abs.empty?
      mean = above_abs.sum / above_abs.size
      relative = block_loudness(mean) - 10.0
      gated = above_abs.select { |z| block_loudness(z) > relative }
      return NEG_INF if gated.empty?
      block_loudness(gated.sum / gated.size)
    end

    # Loudest 3 s window (LUFS-S max): the number that tracks "how loud does
    # this actually feel at its peak", which is what a performer cares about.
    def lufs_short_term_max(channels, sample_rate)
      weighted = channels.map { |ch| k_weight(ch, sample_rate) }
      block = (3.0 * sample_rate).to_i
      hop = (0.1 * sample_rate).to_i
      n = weighted.first.size
      block = n if block > n
      best = NEG_INF
      (0..(n - block)).step(hop) do |s|
        z = weighted.sum do |ch|
          sum = 0.0
          (s...(s + block)).each { |i| sum += ch[i] * ch[i] }
          sum / block
        end
        l = block_loudness(z)
        best = l if l > best
      end
      best
    end

    # ------------------------------------------------------------ distortion

    # Coherent fit at a known frequency: subtract the best-fit sinusoid and
    # what remains is harmonics plus noise. Exact for a stationary tone, and
    # it needs no windowing, so the floor is the float32 noise floor rather
    # than spectral leakage.
    def thd_n_db(ch, freq, sample_rate)
      ch = whole_seconds(ch, sample_rate)
      return NEG_INF if ch.empty?
      w = 2 * Math::PI * freq / sample_rate
      cs = sn = 0.0
      ch.each_with_index do |v, n|
        cs += v * Math.cos(w * n)
        sn += v * Math.sin(w * n)
      end
      cs = cs * 2 / ch.size
      sn = sn * 2 / ch.size
      residual = 0.0
      ch.each_with_index do |v, n|
        r = v - (cs * Math.cos(w * n) + sn * Math.sin(w * n))
        residual += r * r
      end
      fundamental = Math.sqrt(cs * cs + sn * sn) / Math.sqrt(2)
      return NEG_INF if fundamental <= 0.0
      db(Math.sqrt(residual / ch.size) / fundamental)
    end

    # Single-bin DFT has no window, so it only reads a true amplitude when the
    # analysed span holds a whole number of cycles of everything present.
    # Trimming to whole seconds does that for any integer-Hz test signal;
    # without it, leakage from the test tones sits about 42 dB down and masks
    # every distortion product underneath it.
    def whole_seconds(ch, sample_rate)
      n = (ch.size / sample_rate) * sample_rate
      n.zero? ? ch : ch[0, n]
    end

    # Amplitude of one frequency component, via a single-bin DFT.
    def component(ch, freq, sample_rate)
      w = 2 * Math::PI * freq / sample_rate
      cs = sn = 0.0
      ch.each_with_index do |v, n|
        cs += v * Math.cos(w * n)
        sn += v * Math.sin(w * n)
      end
      Math.sqrt((cs * 2 / ch.size) ** 2 + (sn * 2 / ch.size) ** 2)
    end

    # Intermodulation products around a two-tone pair. Non-linearity that
    # sounds like mud rather than like harmonics shows up here first.
    def imd_db(ch, f1, f2, sample_rate)
      ch = whole_seconds(ch, sample_rate)
      ref = [component(ch, f1, sample_rate), component(ch, f2, sample_rate)].max
      return NEG_INF if ref <= 0.0
      diff = (f2 - f1).abs
      products = [diff, f1 + f2, 2 * f1 - f2, 2 * f2 - f1, 3 * f1 - 2 * f2, 3 * f2 - 2 * f1]
      power = products.select { |f| f > 0 && f < sample_rate / 2.0 }
                      .sum { |f| component(ch, f, sample_rate) ** 2 }
      db(Math.sqrt(power) / ref)
    end

    # ---------------------------------------------------------------- stereo

    # Block-by-block gain applied to each channel relative to a reference
    # render. An unlinked stereo limiter gives the two channels different
    # gains on the same hit, which is what drags the image sideways.
    # How far apart the gains applied to the two channels drift, in dB. A
    # stereo-linked limiter derives one gain curve and applies it to both, so
    # this is zero by construction; an unlinked pair reduces each channel on
    # its own peaks, and the difference is the stereo image moving under the
    # listener whenever the limiter works.
    #
    # The gain is read sample by sample against the reference rather than
    # from block RMS: over a block the gain is changing, and L and R have
    # different waveforms inside it, so block ratios differ even when the
    # applied gain is identical. Instants where either channel is near a zero
    # crossing are skipped, since the ratio there is numerical noise.
    LINK_SAMPLE_FLOOR_DB = -30.0
    LINK_PERCENTILE = 0.99

    def stereo_link_error_db(out_l, out_r, ref_l, ref_r, sample_rate, lag: nil)
      lag ||= best_lag(out_l, ref_l)
      n = [out_l.size, ref_l.size - lag].min
      threshold = [sample_peak(ref_l), sample_peak(ref_r)].max * undb(LINK_SAMPLE_FLOOR_DB)
      return 0.0 if threshold <= 0.0
      errors = []
      i = lag
      while i < n
        rl = ref_l[i - lag]
        rr = ref_r[i - lag]
        if rl.abs > threshold && rr.abs > threshold
          gl = out_l[i] / rl
          gr = out_r[i] / rr
          errors << (db(gl.abs) - db(gr.abs)).abs if gl.abs > 0 && gr.abs > 0
        end
        i += 1
      end
      return 0.0 if errors.empty?
      # Not Comparable#clamp: Sonic Pi's core.rb redefines clamp with a
      # different arity, and this file is loaded alongside it under test.
      index = (errors.size * LINK_PERCENTILE).floor
      index = errors.size - 1 if index > errors.size - 1
      index = 0 if index < 0
      errors.sort[index]
    end

    # Blocks far below the loudest part of the reference are excluded from
    # any gain-ratio measurement. A beat null or a decay tail divides two
    # near-zero numbers and reports tens of dB of gain change that nothing
    # audible corresponds to.
    QUIET_BLOCK_FLOOR_DB = -40.0

    def block_floor(*channels, block, n)
      loudest = 0.0
      (0...(n - block)).step(block) do |s|
        channels.each do |ch|
          r = rms(ch[s, block])
          loudest = r if r > loudest
        end
      end
      [loudest * undb(QUIET_BLOCK_FLOOR_DB), 1e-6].max
    end

    # Lag, in samples, that best aligns `ref` to `out`. Needed because
    # bypassing the limiter also removes its lookahead delay, so a bypassed
    # reference arrives early and every block-by-block comparison against it
    # measures the offset rather than the gain.
    #
    # The lag is found by correlation rather than assumed, which matters:
    # SC's Limiter delays by *twice* its `dur` argument (960 samples at 48 k
    # for the 10 ms the mixer asks for), where SPLimiter delays by exactly
    # its lookahead (72 samples for 1.5 ms). Assuming either number is how
    # a limiter variant gets built against a mismatched reference and
    # measures as applying gain above unity.
    # Reference samples pinned at full scale are excluded from the
    # correlation. A bypassed reference is heavily clipped at high drive
    # (nearly 40% of samples at pre_amp 4.0), and clipping distorts it worst
    # exactly where it is loudest. Correlating a window centred on the peak
    # against that, with an unnormalised dot product that rewards
    # high-energy lags, returns confidently wrong answers: 1814 samples
    # where the true delay is 72. Every gain ratio downstream is then
    # comparing mismatched samples.
    #
    # So: probe several windows spread through the signal rather than one at
    # the peak, skip clipped reference samples, and normalise by reference
    # energy so the score is a correlation rather than a magnitude.
    CLIPPED_SAMPLE = 0.999

    def best_lag(out, ref, max_lag: 2048, window: 4096, probes: 3)
      n = [out.size, ref.size].min
      span = n - window - max_lag
      return 0 if span <= 0
      starts = (1..probes).map { |k| max_lag + span * k / (probes + 1) }

      best = 0
      best_score = -Float::INFINITY
      (0..max_lag).each do |lag|
        num = 0.0
        den = 0.0
        starts.each do |start|
          i = 0
          while i < window
            r = ref[start - lag + i]
            if r.abs < CLIPPED_SAMPLE
              num += out[start + i] * r
              den += r * r
            end
            i += 16
          end
        end
        next if den <= 0.0
        score = num / Math.sqrt(den)
        if score > best_score
          best_score = score
          best = lag
        end
      end
      best
    end

    # Gain reduction against a reference render of the same signal with the
    # limiter bypassed: the honest measure of how hard the limiter is working,
    # rather than inferring it from how far the input went over.
    def gain_reduction_db(out_ch, ref_ch, sample_rate, block_ms: 10, lag: nil)
      lag ||= best_lag(out_ch, ref_ch)
      block = (sample_rate * block_ms / 1000.0).to_i
      n = [out_ch.size, ref_ch.size - lag].min
      floor = block_floor(ref_ch, block, n)
      worst = 0.0
      total = 0.0
      count = 0
      (lag...(n - block)).step(block) do |s|
        r = rms(ref_ch[s - lag, block])
        next if r < floor
        g = db(rms(out_ch[s, block]) / r)
        next unless g.finite?
        reduction = -g
        worst = reduction if reduction > worst
        total += [reduction, 0.0].max
        count += 1
      end
      { peak: worst, mean: count.zero? ? 0.0 : total / count }
    end
  end
end
