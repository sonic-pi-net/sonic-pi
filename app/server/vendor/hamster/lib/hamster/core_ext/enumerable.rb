require "hamster/list"

module Hamster
  module CoreExt
    module Enumerable
      def self.included(base)
        base.class_eval do
          def to_list
            list = EmptyList
            reverse_each { |item| list = list.cons(item) }
            list
          end
        end
      end
    end
  end
end

module Enumerable
  include Hamster::CoreExt::Enumerable
end
