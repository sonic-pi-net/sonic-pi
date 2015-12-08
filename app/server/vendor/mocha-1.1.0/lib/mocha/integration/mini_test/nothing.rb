module Mocha
  module Integration
    module MiniTest
      module Nothing
        def self.applicable_to?(test_unit_version, ruby_version = nil)
          true
        end

        def self.description
          "nothing (no MiniTest integration available)"
        end

        def self.included(mod)
          raise "No MiniTest integration available"
        end
      end
    end
  end
end
