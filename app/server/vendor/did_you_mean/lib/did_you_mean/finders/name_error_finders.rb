module DidYouMean
  module NameErrorFinders
    def self.included(*)
      raise "Do not include this module since it overrides Class.new method."
    end

    def self.new(exception)
      klass = if /uninitialized constant/ =~ exception.original_message
        SimilarClassFinder
      elsif /undefined local variable or method/ =~ exception.original_message
        SimilarNameFinder
      else
        NullFinder
      end

      klass.new(exception)
    end
  end

  finders["NameError"] = NameErrorFinders
end

require 'did_you_mean/finders/name_error_finders/similar_name_finder'
require 'did_you_mean/finders/name_error_finders/similar_class_finder'
