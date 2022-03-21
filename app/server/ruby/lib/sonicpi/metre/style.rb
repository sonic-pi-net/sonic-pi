require_relative "distribution"

module SonicPi
  class Style

    STYLE_LOOKUP = {
      triplet_swing: {
        1 => [NormalDistribution.new, NormalDistribution.new(0.166667),
          NormalDistribution.new, NormalDistribution.new(0.166667),
          NormalDistribution.new, NormalDistribution.new(0.166667),
          NormalDistribution.new, NormalDistribution.new(0.166667)]
      },

      viennese_waltz: {
        0 => [NormalDistribution.new, NormalDistribution.new(-0.3057, 0.0051), NormalDistribution.new(-0.0391, 0.1054)]
      },

      jembe: {
        1 => [NormalDistribution.new(0, 0.0334), NormalDistribution.new(-0.1352, 0.0523), NormalDistribution.new(-0.1111, 0.0385),
          NormalDistribution.new(0.0058, 0.0385), NormalDistribution.new(-0.1262, 0.0446), NormalDistribution.new(-0.1059, 0.0456),
          NormalDistribution.new(0.0100, 0.0390), NormalDistribution.new(-0.1245, 0.0474), NormalDistribution.new(-0.1141, 0.0417),
          NormalDistribution.new(0.0029, 0.0401), NormalDistribution.new(-0.1483, 0.0413), NormalDistribution.new(-0.1213, 0.0467)]
      }
    }

    attr_reader :name, :deepest_metrical_level

    # Distributions is a hash of metrical levels (e.g. 0, 1, etc.) to a list containing a distribution for each metrical location in that level
    # Any probability distribution can be used as long as its class has a sample() method
    def initialize(name, distributions)
      @name = name
      @distributions = distributions
      @deepest_metrical_level = @distributions.keys.max
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

    # Tests to see if this style is compatible with a given metre object
    # Returns true if the style has the same number of distributions as the metre has events at each level
    def compatible_with?(metre)
      (0..@deepest_metrical_level).each do |level|
        metre_at_level = metre.get_level(level)
        return false if @distributions[level] and @distributions[level].length != metre_at_level.length
      end
      return true
    end

    # Static method to lookup a style preset from its symbol
    # Creates and returns the corresponding Style object
    def self.lookup(style_name)
      s = STYLE_LOOKUP[style_name]
      raise "Unknown style #{style_name}" unless s
      return Style.new(style_name, s)
    end
  end
end
