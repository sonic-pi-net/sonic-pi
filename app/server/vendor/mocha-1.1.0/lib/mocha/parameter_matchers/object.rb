require 'mocha/parameter_matchers/equals'

module Mocha

  module ObjectMethods
    # @private
    def to_matcher
      Mocha::ParameterMatchers::Equals.new(self)
    end
  end

end

# @private
class Object
  include Mocha::ObjectMethods
end
