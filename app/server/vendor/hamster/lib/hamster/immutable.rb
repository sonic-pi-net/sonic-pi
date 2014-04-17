module Hamster
  module Immutable
    def self.included(klass)
      klass.extend(ClassMethods)
      klass.instance_eval do
        include InstanceMethods
      end
    end

    module ClassMethods
      def new(*args)
        super.immutable!
      end

      def memoize(*names)
        include MemoizeMethods unless include?(MemoizeMethods)
        names.each do |name|
          original_method = "__hamster_immutable_#{name}__"
          alias_method original_method, name
          class_eval <<-METHOD, __FILE__, __LINE__
            def #{name}
              if @__hamster_immutable_memory__.instance_variable_defined?(:@#{name})
                @__hamster_immutable_memory__.instance_variable_get(:@#{name})
              else
                @__hamster_immutable_memory__.instance_variable_set(:@#{name}, #{original_method})
              end
            end
          METHOD
        end
      end
    end

    module MemoizeMethods
      def immutable!
        @__hamster_immutable_memory__ = Object.new
        freeze
      end
    end

    module InstanceMethods
      def immutable!
        freeze
      end

      def immutable?
        frozen?
      end

      alias_method :__hamster_immutable_dup__, :dup
      private :__hamster_immutable_dup__

      def dup
        self
      end

      def clone
        self
      end

      protected

      def transform_unless(condition, &block)
        condition ? self : transform(&block)
      end

      def transform(&block)
        __hamster_immutable_dup__.tap { |copy| copy.instance_eval(&block) }.immutable!
      end
    end
  end
end
