module DidYouMean
  module NameErrorFinders
    def self.included(*)
      raise "Do not include this module since it overrides Class.new method."
    end

    def self.new(exception)
      case exception.original_message
      when /uninitialized constant/
        ClassFinder
      when /undefined local variable or method/, /undefined method/, /uninitialized class variable/
        NameFinder
      else
        NullFinder
      end.new(exception)
    end
  end
end

require 'did_you_mean/finders/name_error_finders/name_finder'
require 'did_you_mean/finders/name_error_finders/class_finder'
