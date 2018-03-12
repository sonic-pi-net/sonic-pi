module Mocha
  module Integration
    module TestUnit
      module Nothing
        def self.applicable_to?(test_unit_version, ruby_version = nil)
          true
        end

        def self.description
          "nothing (no Test::Unit integration available)"
        end

        def self.included(mod)
          raise "No Test::Unit integration available"
        end
      end
    end
  end
end
