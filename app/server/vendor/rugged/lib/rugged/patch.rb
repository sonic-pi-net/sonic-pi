module Rugged
  class Patch
    include Enumerable
    alias each each_hunk

    alias size hunk_count
    alias count hunk_count

    attr_accessor :owner
    alias diff owner

    def inspect
      "#<#{self.class.name}:#{object_id}>"
    end

    # Returns the number of changes in the patch.
    def changes
      stat.reduce { |t,v| t + v }
    end

    # Returns an Array containing all hunks of the patch.
    def hunks
      each_hunk.to_a
    end
  end
end
