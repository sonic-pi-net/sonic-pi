module DidYouMean
  class SimilarNameFinder
    include BaseFinder
    attr_reader :name, :_methods, :_local_variables, :_instance_variables

    def initialize(exception)
      @name             = exception.name
      @_methods         = exception.frame_binding.eval("methods")
      @_local_variables = exception.frame_binding.eval("local_variables")
      @_instance_variables = exception.frame_binding.eval("instance_variables").map do |name|
        name.to_s.tr("@", "")
      end
    end

    def words
      local_variable_names + method_names + instance_variable_names
    end

    alias target_word name

    def local_variable_names
      _local_variables.map {|word| LocalVariableName.new(word.to_s) }
    end

    def similar_local_variables
      similar_words.select{|word| word.is_a?(LocalVariableName) }
    end

    def method_names
      _methods.map {|word| MethodName.new(word.to_s) }
    end

    def similar_methods
      similar_words.select{|word| word.is_a?(MethodName) }
    end

    def instance_variable_names
      _instance_variables.map {|word| InstanceVariableName.new(word.to_s) }
    end

    def similar_instance_variables
      similar_words.select {|word| word.is_a?(InstanceVariableName) }
    end

    def format(word)
      "#{word.prefix}#{word}"
    end

    class MethodName < String
      def prefix; "#"; end
    end

    class LocalVariableName < String
      def prefix; ""; end
    end

    class InstanceVariableName < String
      def prefix; "@"; end
    end
  end
end
