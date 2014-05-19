module Rugged
  class Reference
    def self.create(repo, name, target, options = {})
      warn "DEPRECATION WARNING: Rugged::Reference.create is deprecated and will be removed."
      repo.references.create(name, target, options)
    end

    def self.lookup(repo, ref_name)
      warn "DEPRECATION WARNING: Rugged::Reference.lookup is deprecated and will be removed."
      repo.references[ref_name]
    end

    def self.each(repo, glob = nil, &block)
      warn "DEPRECATION WARNING: Rugged::Reference.each is deprecated and will be removed."
      repo.references.each(glob, &block)
    end

    def self.each_name(repo, glob = nil, &block)
      warn "DEPRECATION WARNING: Rugged::Reference.each_name is deprecated and will be removed."
      repo.references.each_name(glob, &block)
    end

    def self.exist?(repo, ref_name)
      warn "DEPRECATION WARNING: Rugged::Reference.exist? is deprecated and will be removed."
      repo.references.exist?(ref_name)
    end

    def self.exists?(repo, ref_name)
      warn "DEPRECATION WARNING: Rugged::Reference.exists? is deprecated and will be removed."
      repo.references.exists?(ref_name)
    end

    def rename(new_name, force = false)
      warn "DEPRECATION WARNING: Rugged::Reference#rename is deprecated and will be removed."
      @owner.references.rename(self, new_name, force)
    end

    def set_target(target, options = {})
      warn "DEPRECATION WARNING: Rugged::Reference#set_target is deprecated and will be removed."
      @owner.references.update(self, target, options)
    end

    def delete!
      warn "DEPRECATION WARNING: Rugged::Reference#delete! is deprecated and will be removed."
      @owner.references.delete(self)
    end

    def inspect
      "#<#{self.class}:#{object_id} {name: #{name.inspect}, target: #{target.inspect}}>"
    end

  end
end
