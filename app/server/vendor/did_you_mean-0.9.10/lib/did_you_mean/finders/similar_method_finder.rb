module DidYouMean
  class SimilarMethodFinder
    include BaseFinder
    attr_reader :method_name, :receiver

    def initialize(exception)
      @method_name, @receiver = exception.name, exception.receiver
    end

    def words
      method_names = receiver.methods + receiver.singleton_methods
      method_names.delete(@method_name)
      method_names.uniq
    end

    alias target_word method_name

    def format(word)
      "#{separator}#{word}"
    end

    def class_method?
      receiver.is_a? Class
    end

    def separator
      class_method? ? "." : "#"
    end
  end

  if RUBY_ENGINE == 'ruby' || RUBY_ENGINE == 'jruby'
    finders["NoMethodError"] = SimilarMethodFinder
  end
end
