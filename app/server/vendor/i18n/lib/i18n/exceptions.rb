require 'cgi'

module I18n
  # Handles exceptions raised in the backend. All exceptions except for
  # MissingTranslationData exceptions are re-thrown. When a MissingTranslationData
  # was caught the handler returns an error message string containing the key/scope.
  # Note that the exception handler is not called when the option :throw was given.
  class ExceptionHandler
    include Module.new {
      def call(exception, locale, key, options)
        if exception.is_a?(MissingTranslation)
          #
          # TODO: this block is to be replaced by `exception.message` when
          # rescue_format is removed
          if options[:rescue_format] == :html
            if !defined?(@rescue_format_deprecation)
              $stderr.puts "[DEPRECATED] I18n's :recue_format option will be removed from a future release. All exception messages will be plain text. If you need the exception handler to return an html format please set or pass a custom exception handler."
              @rescue_format_deprecation = true
            end
            exception.html_message
          else
            exception.message
          end

        elsif exception.is_a?(Exception)
          raise exception
        else
          throw :exception, exception
        end
      end
    }
  end

  class ArgumentError < ::ArgumentError; end

  class InvalidLocale < ArgumentError
    attr_reader :locale
    def initialize(locale)
      @locale = locale
      super "#{locale.inspect} is not a valid locale"
    end
  end

  class InvalidLocaleData < ArgumentError
    attr_reader :filename
    def initialize(filename, exception_message)
      @filename, @exception_message = filename, exception_message
      super "can not load translations from #{filename}: #{exception_message}"
    end
  end

  class MissingTranslation
    module Base
      attr_reader :locale, :key, :options

      def initialize(locale, key, options = nil)
        @key, @locale, @options = key, locale, options.dup || {}
        options.each { |k, v| self.options[k] = v.inspect if v.is_a?(Proc) }
      end

      def html_message
        key  = CGI.escapeHTML titleize(keys.last)
        path = CGI.escapeHTML keys.join('.')
        %(<span class="translation_missing" title="translation missing: #{path}">#{key}</span>)
      end

      def keys
        @keys ||= I18n.normalize_keys(locale, key, options[:scope]).tap do |keys|
          keys << 'no key' if keys.size < 2
        end
      end

      def message
        "translation missing: #{keys.join('.')}"
      end
      alias :to_s :message

      def to_exception
        MissingTranslationData.new(locale, key, options)
      end

      protected

      # TODO : remove when #html_message is removed
      def titleize(key)
        key.to_s.gsub('_', ' ').gsub(/\b('?[a-z])/) { $1.capitalize }
      end
    end

    include Base
  end

  class MissingTranslationData < ArgumentError
    include MissingTranslation::Base
  end

  class InvalidPluralizationData < ArgumentError
    attr_reader :entry, :count
    def initialize(entry, count)
      @entry, @count = entry, count
      super "translation data #{entry.inspect} can not be used with :count => #{count}"
    end
  end

  class MissingInterpolationArgument < ArgumentError
    attr_reader :key, :values, :string
    def initialize(key, values, string)
      @key, @values, @string = key, values, string
      super "missing interpolation argument #{key.inspect} in #{string.inspect} (#{values.inspect} given)"
    end
  end

  class ReservedInterpolationKey < ArgumentError
    attr_reader :key, :string
    def initialize(key, string)
      @key, @string = key, string
      super "reserved key #{key.inspect} used in #{string.inspect}"
    end
  end

  class UnknownFileType < ArgumentError
    attr_reader :type, :filename
    def initialize(type, filename)
      @type, @filename = type, filename
      super "can not load translations from #{filename}, the file type #{type} is not known"
    end
  end
end
