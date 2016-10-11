module MetaMagic
  module ClassMethods
    def a_class_method
      "this is a mixed-in class method"
    end
  end

  module InstanceMethods
    def an_instance_method
      "this is a mixed-in instance method"
    end
  end

  def self.included(base)
    base.send :extend, ClassMethods
    base.send :include, InstanceMethods

    base.class_eval do
      define_method :dynamic do
        "A dynamically defined instance method"
      end
    end
  end
end
