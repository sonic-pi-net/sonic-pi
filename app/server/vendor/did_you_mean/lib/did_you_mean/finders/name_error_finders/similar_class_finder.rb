module DidYouMean
  class SimilarClassFinder
    include BaseFinder
    attr_reader :class_name, :original_message

    def initialize(exception)
      @class_name, @original_message = exception.name, exception.original_message
    end

    def words
      scopes.flat_map do |scope|
        scope.constants.map {|name| ConstantName.new(name, scope) }
      end
    end

    def name_from_message
      class_name || /([A-Z]\w*$)/.match(original_message)[0]
    end
    alias target_word name_from_message

    def similar_words
      super.map(&:full_name)
    end

    def scopes
      @scopes ||= scope_base.size.times.map do |count|
        eval(scope_base[0..(- count)].join("::"))
      end.reverse << Object
    end

    private

    def scope_base
      @scope_base ||= (/(([A-Z]\w*::)*)([A-Z]\w*)$/ =~ original_message ? $1 : "").split("::")
    end

    class ConstantName < String
      attr_reader :scope

      def initialize(str, scope)
        super(str.to_s)
        @scope = scope.to_s
      end

      def full_name
        scope == "Object" ? to_s : "#{scope}::#{to_s}"
      end
    end
  end
end
