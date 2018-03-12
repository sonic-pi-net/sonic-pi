require 'mocha/class_method'

module Mocha

  class AnyInstanceMethod < ClassMethod

    def mock
      stubbee.any_instance.mocha
    end

    def reset_mocha
      stubbee.any_instance.reset_mocha
    end

    def hide_original_method
      if method_exists?(method)
        begin
          @original_method = stubbee.instance_method(method)
          if @original_method && @original_method.owner == stubbee
            @original_visibility = :public
            if stubbee.protected_instance_methods.include?(method)
              @original_visibility = :protected
            elsif stubbee.private_instance_methods.include?(method)
              @original_visibility = :private
            end
            stubbee.send(:remove_method, method)
          end

          include_prepended_module if RUBY_VERSION >= '2.0'
        rescue NameError
          # deal with nasties like ActiveRecord::Associations::AssociationProxy
        end
      end
    end

    def define_new_method
      definition_target.class_eval(<<-CODE, __FILE__, __LINE__ + 1)
        def #{method}(*args, &block)
          self.class.any_instance.mocha.method_missing(:#{method}, *args, &block)
        end
      CODE
      if @original_visibility
        Module.instance_method(@original_visibility).bind(definition_target).call(method)
      end
    end

    def remove_new_method
      definition_target.send(:remove_method, method)
    end

    def restore_original_method
      if @original_method && @original_method.owner == stubbee
        stubbee.send(:define_method, method, @original_method)
        Module.instance_method(@original_visibility).bind(stubbee).call(method)
      end
    end

    def method_exists?(method)
      return true if stubbee.public_instance_methods(false).include?(method)
      return true if stubbee.protected_instance_methods(false).include?(method)
      return true if stubbee.private_instance_methods(false).include?(method)
      return false
    end

    private

    def include_prepended_module
      possible_prepended_modules = stubbee.ancestors.take_while do |mod|
        !(Class === mod)
      end

      if possible_prepended_modules.any?
        @definition_target = PrependedModule.new
        stubbee.__send__ :prepend, @definition_target
      end
    end

    def definition_target
      @definition_target ||= stubbee
    end

  end

end
