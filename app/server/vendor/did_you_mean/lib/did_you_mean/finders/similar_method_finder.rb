module DidYouMean
  class SimilarMethodFinder
    include BaseFinder
    attr_reader :method_name, :receiver

    def initialize(exception)
      @method_name, @receiver = exception.name, exception.args.first
    end

    def words
      receiver.methods + receiver.singleton_methods
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

  finders["NoMethodError"] = SimilarMethodFinder
end
