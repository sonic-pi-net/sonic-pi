module MultiJson
  module Options

    def load_options=(options)
      MultiJson.reset_cached_options!
      @load_options = options
    end

    def dump_options=(options)
      MultiJson.reset_cached_options!
      @dump_options = options
    end

    def load_options(*args)
      get_options :load_options, *args
    end

    def dump_options(*args)
      get_options :dump_options, *args
    end

    def default_load_options
      @default_load_options ||= {}
    end

    def default_dump_options
      @default_dump_options ||= {}
    end

    private

    def get_options(ivar, *args)
      defaults = send("default_#{ivar}")

      return defaults unless instance_variable_defined?("@#{ivar}")

      value = instance_variable_get("@#{ivar}")

      if value.respond_to?(:call) and value.arity
        value.arity == 0 ? value[] : value[*args]
      elsif Hash === value or value.respond_to?(:to_hash)
        value.to_hash
      else
        defaults
      end
    end
  end
end
