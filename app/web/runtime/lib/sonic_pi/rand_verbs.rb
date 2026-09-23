# SPDX-License-Identifier: AGPL-3.0-or-later
# The language's random verbs over the current stream.
module SonicPi
  module RandVerbs
    NOISE_ERROR = "please use one of :white, :pink, :light_pink, :dark_pink or :perlin instead"

    def __rand = SonicPi::Rand.current

    def rand(max = 1)
      return 0.0 if max == 0
      return rrand(note(max.min), note(max.max)) if max.is_a?(Range)
      __rand.rand!(max)
    end

    def rand_i(max = 2)
      return 0 if max == 0
      return rrand_i(max.min, max.max) if max.is_a?(Range)
      __rand.rand_i!(note(max))
    end

    def rrand(min, max, *opts)
      min = note(min)
      max = note(max)
      step = resolve_synth_opts_hash_or_array(opts)[:step]   # step: 2, or :step, 2, as native reads it
      if min == max
        return step ? quantise(min, step) : min
      end
      r = __rand.rand!((min - max).abs)
      smallest = [min, max].min
      step ? quantise(r + smallest, step) : r + smallest
    end

    def rrand_i(min, max)
      min = note(min)
      max = note(max)
      return min if min == max
      r = __rand.rand_i!((min - max).abs.to_i + 1)
      r + [min, max].min
    end

    def rdist(width, centre = 0, *opts) = rrand(centre - width, centre + width, *opts)
    def dice(num_sides = 6) = rrand_i(1, num_sides)
    def one_in(num)
      num = num.to_i
      num < 1 ? false : rrand_i(1, num) == 1
    end

    def choose(args = nil) = args ? args.to_a.choose : lambda { |col| col.choose }
    def pick(*args)
      if args[0].respond_to?(:to_a) && !args[0].is_a?(Numeric)
        items = args[0]
        n = args[1].is_a?(Numeric) ? args.shift(2)[1] : (args.shift(1); 1)
      else
        items = nil
        n = args[0].is_a?(Numeric) ? args.shift(1)[0] : 1
      end
      return lambda { |col| col.pick(n, *args) } unless items
      items.pick(n, *args)
    end
    def shuffle(list) = list.respond_to?(:shuffle) ? list.shuffle : list.to_a.shuffle

    def rand_look(*args)
      res = rand(*args)
      rand_back
      res
    end

    def rand_i_look(*args)
      res = rand_i(*args)
      rand_back
      res
    end

    def rand_back(amount = 1)
      __rand.dec_idx!(amount)
      __rand.rand_peek
    end

    def rand_skip(amount = 1)
      __rand.inc_idx!(amount)
      __rand.rand_peek
    end

    def rand_reset = (__rand.idx = 0)

    def use_random_seed(seed, &block)
      raise ArgumentError, "use_random_seed does not work with a block. Perhaps you meant with_random_seed" if block
      __rand.new_thread_idx = 0
      __rand.set_seed!(seed)
    end

    def with_random_seed(seed, &block)
      raise ArgumentError, "with_random_seed requires a block. Perhaps you meant use_random_seed" unless block
      r = __rand
      saved = [r.seed, r.idx, r.new_thread_idx]
      r.set_seed!(seed)
      r.new_thread_idx = 0
      res = block.call
      r.set_seed!(saved[0], saved[1])
      r.new_thread_idx = saved[2]
      res
    end

    def use_random_source(noise_type, &block)
      raise ArgumentError, "use_random_source does not work with a block. Perhaps you meant with_random_source" if block
      raise ArgumentError, "invalid noise type '#{noise_type}' - #{NOISE_ERROR}" unless SonicPi::Rand::SOURCES.include?(noise_type.to_s.to_sym) && SonicPi::Rand::SOURCES.map(&:to_s).include?(noise_type.to_s)
      __rand.source = noise_type.to_s.to_sym
    end

    def with_random_source(noise_type, &block)
      raise ArgumentError, "with_random_source requires a block. Perhaps you meant use_random_source" unless block
      raise ArgumentError, "invalid noise type '#{noise_type}' - #{NOISE_ERROR}" unless SonicPi::Rand::SOURCES.map(&:to_s).include?(noise_type.to_s)
      saved = __rand.source
      __rand.source = noise_type.to_s.to_sym
      res = block.call
      __rand.source = saved
      res
    end

    def quantise(n, step)
      raise ArgumentError, "quantisation step resolution should be positive" if step <= 0
      (n.to_f / step).round * step
    end

    # Notes are the western theory module's; here only numbers pass.
    def note(n)
      return n if n.is_a?(Numeric)
      raise ArgumentError, "note names are not in this runtime yet: #{n.inspect}"
    end

    def ring(*a) = SonicPi::Ring.new(a)
  end
end
