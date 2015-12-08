require 'delegate'

module DidYouMean
  class ClassFinder
    include BaseFinder
    attr_reader :class_name, :original_message

    def initialize(exception)
      @class_name, @original_message = exception.name, exception.original_message
    end

    def searches
      {name_from_message => class_names}
    end

    def class_names
      scopes.flat_map do |scope|
        scope.constants.map do |c|
          ClassName.new(c, scope == Object ? EMPTY : "#{scope}::")
        end
      end
    end

    if RUBY_ENGINE == 'jruby'
      # Always use the original error message to retrieve the user
      # input since JRuby 1.7 behaves differently from MRI/Rubinius.
      #
      #   class Name; end
      #   error = (Name::DoesNotExist rescue $!)
      #
      #   # on MRI/Rubinius
      #   error.name #=> :DoesNotExist
      #
      #   # on JRuby <= 1.7
      #   error.name #=> :'Name::DoesNotExist'
      #
      def name_from_message
        /([A-Z]\w*$)/.match(original_message)[0]
      end
    else
      def name_from_message
        class_name || /([A-Z]\w*$)/.match(original_message)[0]
      end
    end

    def suggestions
      super.map(&:full_name)
    end

    def scopes
      @scopes ||= scope_base.inject([Object]) do |_scopes, scope|
        _scopes << _scopes.last.const_get(scope)
      end
    end

    private

    def scope_base
      @scope_base ||= (/(([A-Z]\w*::)*)([A-Z]\w*)$/ =~ original_message ? $1 : "").split("::")
    end

    class ClassName < SimpleDelegator
      attr :namespace

      def initialize(name, namespace = '')
        super(name)
        @namespace = namespace
      end

      def full_name
        self.class.new("#{namespace}#{__getobj__}")
      end
    end

    private_constant :ClassName
  end
end
