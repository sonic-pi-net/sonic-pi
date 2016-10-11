# A pile of methods that only get tested in their frameworks
# and thus make this file only 100% covered when all framework test
# results are merged
module FrameworkSpecific
  class << self
    def cucumber
      "Only tested in Cucumber"
    end

    def rspec
      "Only tested in RSpec"
    end

    def test_unit
      "Only tested in Test/Unit"
    end
  end
end
