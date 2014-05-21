module Rugged
  class Tag < Rugged::Reference
    def self.create(repo, name, target, *args)
      warn "DEPRECATION WARNING: Rugged::Tag.create is deprecated and will be removed."
      repo.tags.create(name, target, *args)
    end

    def self.lookup(repo, name)
      warn "DEPRECATION WARNING: Rugged::Tag.lookup is deprecated and will be removed."
      repo.tags[name]
    end

    def self.each(repo, glob = nil, &block)
      warn "DEPRECATION WARNING: Rugged::Tag.each is deprecated and will be removed."
      repo.tags.each(glob, &block)
    end

    def self.each_name(repo, glob = nil, &block)
      warn "DEPRECATION WARNING: Rugged::Tag.each_name is deprecated and will be removed."
      repo.tags.each_name(glob, &block)
    end

    def delete
      warn "DEPRECATION WARNING: Rugged::Tag#delete is deprecated and will be removed."
      @owner.tags.delete(self.name)
    end

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
