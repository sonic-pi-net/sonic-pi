module DidYouMean
  module Correctable
    attr_reader :frame_binding

    IGNORED_CALLERS = [
      /( |`)missing_name'/,
      /( |`)safe_constantize'/
    ].freeze
    private_constant :IGNORED_CALLERS

    def self.included(klass)
      klass.class_eval do
        __to_s__ = klass.instance_method(:to_s)
        define_method(:original_message){ __to_s__.bind(self).call }

        def to_s
          msg = original_message.dup
          bt  = caller.first(6)

          msg << Formatter.new(suggestions).to_s if IGNORED_CALLERS.all? {|ignored| bt.grep(ignored).empty? }
          msg
        rescue
          original_message
        end
      end
    end

    def suggestions
      finder.suggestions
    end

    def finder
      @finder ||= DidYouMean.finders[self.class.to_s].new(self)
    end
  end
end

NameError.send(:include, DidYouMean::Correctable)
