module DidYouMean
  class NameFinder
    include BaseFinder
    attr_reader :name, :method_names, :lvar_names, :ivar_names, :cvar_names

    def initialize(exception)
      @name         = exception.name.to_s.tr(AT, EMPTY)
      @lvar_names   = exception.frame_binding.eval("local_variables")
      @method_names = exception.frame_binding.eval("methods + private_methods")
      @cvar_names   = exception.frame_binding.eval("self.class.class_variables")
      @ivar_names   = exception.frame_binding.eval("instance_variables")
    end

    def searches
      {name => (lvar_names + method_names + ivar_names + cvar_names)}
    end
  end
end
