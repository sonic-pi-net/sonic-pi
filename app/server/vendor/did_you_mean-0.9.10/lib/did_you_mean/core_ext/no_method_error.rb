case RUBY_ENGINE
when 'ruby'
  require 'did_you_mean/method_missing'

when 'jruby'
  NoMethodError.class_eval do
    def to_s
      receiver unless defined?(@receiver)
      super
    end

    def receiver
      @receiver ||= begin
        field = JRuby.reference(__message__).java_class.getDeclaredField("object")
        field.setAccessible(true)
        field.get(__message__)
      rescue
        super
      end
    end

    private

    if JRUBY_VERSION >= '9.0.0.0'
      def __message__
        JRuby.reference(self).getMessage
      end
    else
      def __message__
        error = JRuby.reference(self)
        error.java_class.getField("message").get(error)
      end
    end
  end
end
