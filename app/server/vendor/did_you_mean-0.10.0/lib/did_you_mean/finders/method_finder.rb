module DidYouMean
  class MethodFinder
    include BaseFinder
    attr_reader :method_name, :receiver

    def initialize(exception)
      @method_name = exception.name
      @receiver    = exception.receiver
      @binding     = exception.frame_binding
      @location    = exception.backtrace.first
      @ivar_names  = NameFinder.new(exception).ivar_names
    end

    def searches
      {
        method_name        => method_names,
        receiver_name.to_s => @ivar_names
      }
    end

    def method_names
      method_names = receiver.methods + receiver.singleton_methods
      method_names += receiver.private_methods if receiver.equal?(@binding.eval("self"))
      method_names.delete(method_name)
      method_names.uniq!
      method_names
    end

    def receiver_name
      return unless @receiver.nil?

      abs_path, lineno, label =
        /(.*):(.*):in `(.*)'/ =~ @location && [$1, $2.to_i, $3]

      line =
        case abs_path
        when "(irb)"
          Readline::HISTORY.to_a.last
        when "(pry)"
          ::Pry.history.to_a.last
        else
          File.open(abs_path) do |file|
            file.detect { file.lineno == lineno }
          end if File.exist?(abs_path)
        end

      /@(\w+)*\.#{@method_name}/ =~ line.to_s && $1
    end
  end

  if RUBY_ENGINE == 'rbx'
    module MethodFinder::RubiniusSupport
      def self.new(exception)
        if exception.receiver === exception.frame_binding.eval("self")
          NameErrorFinders.new(exception)
        else
          MethodFinder.new(exception)
        end
      end
    end
  end
end
