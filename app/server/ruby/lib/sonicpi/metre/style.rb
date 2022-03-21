require_relative "distribution"

module SonicPi
  class Style

    STYLE_LOOKUP = {
      triplet_swing: {
        1 => [NormalDistribution.new, NormalDistribution.new(0.33333),
          NormalDistribution.new, NormalDistribution.new(0.33333),
          NormalDistribution.new, NormalDistribution.new(0.33333),
          NormalDistribution.new, NormalDistribution.new(0.33333)]
      },

      viennese_waltz: {
        0 => [NormalDistribution.new, NormalDistribution.new(-0.3057, 0.0051), NormalDistribution.new(-0.0391, 0.1054)]
      },

      jembe_suku: {
        1 => [NormalDistribution.new(0, 0.0295), NormalDistribution.new(-0.0665, 0.0442), NormalDistribution.new(-0.0095, 0.0365),
          NormalDistribution.new(0.0072, 0.0340), NormalDistribution.new(-0.0554, 0.0430), NormalDistribution.new(0.0001, 0.0475),
          NormalDistribution.new(0.0195, 0.0351), NormalDistribution.new(-0.0607, 0.0424), NormalDistribution.new(-0.0088, 0.0386),
          NormalDistribution.new(-0.0003, 0.0354), NormalDistribution.new(-0.0830, 0.0381), NormalDistribution.new(-0.0084, 0.0397)]
      },

      jembe_manjanin: {
        1 => [NormalDistribution.new(0, 0.0369), NormalDistribution.new(-0.0815, 0.0579), NormalDistribution.new(0.0057, 0.0416),
          NormalDistribution.new(0.0056, 0.0413), NormalDistribution.new(-0.0791, 0.0448), NormalDistribution.new(0.0071, 0.0479),
          NormalDistribution.new(0.0052, 0.0411), NormalDistribution.new(-0.0695, 0.0548), NormalDistribution.new(-0.0063, 0.0462),
          NormalDistribution.new(0.0046, 0.0438), NormalDistribution.new(-0.1078, 0.0442), NormalDistribution.new(-0.0056, 0.0558)]
      },

      jembe_maraka: {
        1 => [NormalDistribution.new(0, 0.0325), NormalDistribution.new(0.0740, 0.0425), NormalDistribution.new(0.0798, 0.0363),
          NormalDistribution.new(0.0004, 0.0379), NormalDistribution.new(0.0900, 0.0439), NormalDistribution.new(0.0823, 0.0519),
          NormalDistribution.new(-0.0134, 0.0384), NormalDistribution.new(0.0899, 0.0407), NormalDistribution.new(0.0731, 0.0386),
          NormalDistribution.new(-0.0133, 0.0375), NormalDistribution.new(0.0779, 0.0393), NormalDistribution.new(0.0511, 0.0446)]
      },

      jembe_woloso: {
        1 => [NormalDistribution.new(0, 0.0339), NormalDistribution.new(-0.1076, 0.0547), NormalDistribution.new(-0.0294, 0.0373),
          NormalDistribution.new(0.0045, 0.0401), NormalDistribution.new(-0.0942, 0.0460), NormalDistribution.new(-0.0248, 0.0415),
          NormalDistribution.new(0.0053, 0.0409), NormalDistribution.new(-0.0932, 0.0450), NormalDistribution.new(-0.0273, 0.0402),
          NormalDistribution.new(0.0045, 0.0409), NormalDistribution.new(-0.1042, 0.0418), NormalDistribution.new(-0.0501, 0.0446)]
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
