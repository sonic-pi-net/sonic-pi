module DidYouMean
  class SimilarAttributeFinder
    include BaseFinder
    attr_reader :columns, :attribute_name

    def initialize(exception)
      @columns        = exception.frame_binding.eval("self.class").columns
      @attribute_name = (/unknown attribute(: | ')(\w+)/ =~ exception.original_message) && $2
    end

    def words
      columns.map(&:name)
    end

    alias target_word attribute_name

    def format(column_name)
      "%{column}: %{type}" % {
        column: column_name,
        type:   columns.detect{|c| c.name == column_name }.type
      }
    end
  end

  finders["ActiveRecord::UnknownAttributeError"] = SimilarAttributeFinder
end
