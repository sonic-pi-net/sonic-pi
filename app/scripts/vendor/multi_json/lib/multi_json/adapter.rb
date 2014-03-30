require 'singleton'
require 'multi_json/options'

module MultiJson
  class Adapter
    extend Options
    include Singleton
    class << self

      def defaults(action, value)
        metaclass = class << self; self; end

        metaclass.instance_eval do
          define_method("default_#{action}_options"){ value }
        end
      end

      def load(string, options={})
        raise self::ParseError if blank?(string)
        instance.load(string, collect_load_options(options).clone)
      end

      def dump(object, options={})
        instance.dump(object, collect_dump_options(options).clone)
      end

    protected

      def collect_load_options(options)
        cache('load', options){ collect_options(:load_options, options).merge(options) }
      end

      def collect_dump_options(options)
        cache('dump', options){ collect_options(:dump_options, options).merge(options) }
      end

      def collect_options(method, *args)
        global, local = *[MultiJson, self].map{ |r| r.send(method, *args) }
        local.merge(global)
      end

      def cache(method, options)
        cache_key = [self, options].map(&:hash).join + method
        MultiJson.cached_options[cache_key] ||= yield
      end

    private

      def blank?(input)
        input.nil? || /\A\s*\z/ === input
      rescue ArgumentError # invalid byte sequence in UTF-8
        false
      end

    end
  end
end
