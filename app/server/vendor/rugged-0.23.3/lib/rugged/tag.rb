module Rugged
  class Tag < Rugged::Reference
    def name
      canonical_name.sub(%r{^refs/tags/}, "")
    end

    class Annotation
      def self.prettify_message(msg, strip_comments = true)
        Rugged::prettify_message(msg, strip_comments)
      end

      def inspect
        "#<Rugged::Tag::Annotation:#{object_id} {name: #{name.inspect}, message: #{message.inspect}, target: #{target.inspect}>"
      end

      def to_hash
        {
          :message => message,
          :name => name,
          :target => target,
          :tagger => tagger,
        }
      end

      def modify(new_args, force=True)
        args = self.to_hash.merge(new_args)
        Tag.create(args, force)
      end
    end
  end
end
