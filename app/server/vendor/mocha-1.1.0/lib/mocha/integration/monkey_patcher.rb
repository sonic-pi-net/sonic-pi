require 'mocha/api'

module Mocha
  module Integration
    module MonkeyPatcher
      def self.apply(mod, run_method_patch)
        unless mod < Mocha::API
          mod.send(:include, Mocha::API)
        end
        unless mod.method_defined?(:run_before_mocha)
          mod.send(:alias_method, :run_before_mocha, :run)
          mod.send(:remove_method, :run)
          mod.send(:include, run_method_patch)
        end
      end
    end
  end
end
