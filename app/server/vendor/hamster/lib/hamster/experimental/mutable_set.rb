require "forwardable"
require "hamster/set"
require "hamster/read_copy_update"

module Hamster
  def self.mutable_set(*items)
    MutableSet.new(set(*items))
  end

  class MutableSet
    extend Forwardable
    include ReadCopyUpdate

    def add(item)
      transform { |set| set.add(item) }
    end
    def_delegator :self, :add, :<<

    def add?(item)
      added = false
      transform do |set|
        added = !set.include?(item)
        set.add(item)
      end
      added
    end

    def delete(item)
      transform { |set| set.delete(item) }
    end

    def delete?(item)
      deleted = false
      transform do |set|
        deleted = set.include?(item)
        set.delete(item)
      end
      deleted
    end
  end
end
