module SonicPi
  class NormalDistribution
    def initialize(mean=0, stddev=1)
      @mean = mean
      @stddev = stddev
      @cached_result = nil
    end

    def sample
      if @cached_result
        result = @cached_result
        @cached_result = nil
      else
        theta = 2 * Math::PI * SonicPi::Core::SPRand.rand!(1)
        scale = @stddev * Math.sqrt(-2 * Math.log(1 - SonicPi::Core::SPRand.rand!(1)))
        @cached_result = @mean + scale * Math.sin(theta)
        result = @mean + scale * Math.cos(theta)
      end
      result
    end
  end
end
