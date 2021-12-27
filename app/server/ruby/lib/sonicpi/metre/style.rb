require_relative "distribution"

module SonicPi
  class Style

    STYLE_LOOKUP = {
      triplet_swing: {
        :beat_divisions => [2,2,2,2],
        :distributions => {
          -1 => [NormalDistribution.new, NormalDistribution.new(0.33333),
            NormalDistribution.new, NormalDistribution.new(0.33333),
            NormalDistribution.new, NormalDistribution.new(0.33333),
            NormalDistribution.new, NormalDistribution.new(0.33333)]
        }
      },

      viennese_waltz: {
        :beat_divisions => [2,2,2],
        :distributions => {
          0 => [NormalDistribution.new, NormalDistribution.new(-0.6114, 0.02036), NormalDistribution.new(-0.07811, 0.2109)]
        }
      },

      jembe_suku: {
        :beat_divisions => [3,3,3,3],
        :distributions => {
          -1 => [NormalDistribution.new(0, 0.0589), NormalDistribution.new(-0.2331, 0.0885), NormalDistribution.new(-0.2191, 0.0731),
            NormalDistribution.new(0.0145, 0.0680), NormalDistribution.new(-0.2108, 0.0860), NormalDistribution.new(-0.1998, 0.0950),
            NormalDistribution.new(0.0391, 0.0703), NormalDistribution.new(-0.2214, 0.0849), NormalDistribution.new(-0.2175, 0.0772),
            NormalDistribution.new(-0.0005, 0.0709), NormalDistribution.new(-0.2659, 0.0761), NormalDistribution.new(-0.2168, 0.0794)]
        }
      },

      jembe_manjanin: {
        :beat_divisions => [3,3,3,3],
        :distributions => {
          -1 => [NormalDistribution.new(0, 0.0737), NormalDistribution.new(-0.2630, 0.1158), NormalDistribution.new(-0.1885, 0.0832),
            NormalDistribution.new(0.0113, 0.0827), NormalDistribution.new(-0.2582, 0.0897), NormalDistribution.new(-0.1858, 0.09586),
            NormalDistribution.new(0.0103, 0.0822), NormalDistribution.new(-0.2390, 0.1096), NormalDistribution.new(-0.2125, 0.0924),
            NormalDistribution.new(0.0091, 0.0877), NormalDistribution.new(-0.3156, 0.0885), NormalDistribution.new(-0.2111, 0.1115)]
        }
      },

      jembe_maraka: {
        :beat_divisions => [3,3,3,3],
        :distributions => {
          -1 => [NormalDistribution.new(0, 0.0651), NormalDistribution.new(0.0481, 0.0850), NormalDistribution.new(-0.0404, 0.0726),
            NormalDistribution.new(0.0009, 0.0758), NormalDistribution.new(0.0799, 0.0877), NormalDistribution.new(-0.0354, 0.1039),
            NormalDistribution.new(-0.0268, 0.0768), NormalDistribution.new(0.0797, 0.0813), NormalDistribution.new(-0.0538, 0.0771),
            NormalDistribution.new(-0.0266, 0.0751), NormalDistribution.new(0.0558, 0.0785), NormalDistribution.new(-0.0978, 0.0892)]
        }
      },

      jembe_woloso: {
        :beat_divisions => [3,3,3,3],
        :distributions => {
          -1 => [NormalDistribution.new(0, 0.0677), NormalDistribution.new(-0.3152, 0.1094), NormalDistribution.new(-0.2588, 0.0745),
            NormalDistribution.new(0.0090, 0.0802), NormalDistribution.new(-0.2883, 0.0920), NormalDistribution.new(-0.2496, 0.0829),
            NormalDistribution.new(0.0105, 0.0818), NormalDistribution.new(-0.2865, 0.0900), NormalDistribution.new(-0.2545, 0.0804),
            NormalDistribution.new(0.0091, 0.0818), NormalDistribution.new(-0.3084, 0.0835), NormalDistribution.new(-0.3001, 0.0893)]
        }
      }
    }

    attr_reader :name, :beat_divisions, :lowest_metrical_level

    # Beat divisions is a list of integers representing the number of pulse units each beat is divided into
    # Distributions is a hash of metrical levels (e.g. 0, -1, etc.) to a list containing a distribution for each metrical location in that level
    # Any probability distribution can be used as long as its class has a sample() method
    def initialize(name, beat_divisions, distributions)
      @name = name
      @beat_divisions = beat_divisions
      @distributions = distributions
      @lowest_metrical_level = @distributions.keys.min
    end

    # Generate samples from each distribution for each metrical level
    # Returns a hash of metrical levels to lists of samples
    def sample_distributions
      samples = {}
      @distributions.each do |metrical_level, dist_array|
        samples[metrical_level] = dist_array.map { |dist| dist.sample }
      end
      return samples
    end

    # Static method to lookup a style preset from its symbol
    # Creates and returns the corresponding Style object
    def self.lookup(style_name)
      s = STYLE_LOOKUP[style_name]
      raise "Unknown style #{style_name}" unless s
      return Style.new(style_name, s[:beat_divisions], s[:distributions])
    end
  end
end
