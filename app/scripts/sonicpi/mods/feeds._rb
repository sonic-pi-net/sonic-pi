require 'simple-rss'
require 'open-uri'

module SonicPi
  module Mods
    module Feeds

       # def self.included(base)
       #   base.instance_exec {alias_method :sonic_pi_mods_feeds_initialize_old, :initialize}

       #   base.instance_exec do
       #     define_method(:initialize) do |*splat, &block|
       #       sonic_pi_mods_feeds_initialize_old *splat, &block


       #     end
       #   end
       # end

      def feed_entries(feed)
        f = SimpleRSS.parse open(feed)
        f.items.inject([]) do |s, el|
          if el.content
            s << el.content
          elsif el.description
            s << el.description
          elsif el.content_encoded
            s << el.content_encoded
          else
            s
          end
        end
      end

      def feed_text(feed)
        entries = feed_entries(feed)
        entries.inject("") {|s, el| s << " " << (el || "")}
      end

      def feed_images(feed)
        f = SimpleRSS.parse open(feed)
        content = f.items.inject("") do |s, el|
          begin
            s << el.content
            s << el.content_encoded
            s << el.link
          rescue
            s
          end
        end
        (content.scan /<img +src="(.*?)"/).flatten
      end

    end
  end
end
