require "hamster/list"

module Hamster
  module CoreExt
    module Enumerator
      # @deprecated Please use Hamster#enumerate instead
      def to_list
        Hamster.enumerate(self)
      end
    end
  end
end

class Enumerator
  include Hamster::CoreExt::Enumerator
end
