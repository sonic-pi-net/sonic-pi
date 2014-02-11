require "forwardable"
require "hamster/hash"
require "hamster/read_copy_update"

module Hamster
  def self.mutable_hash(pairs = {}, &block)
    MutableHash.new(hash(pairs, &block))
  end

  class MutableHash
    extend Forwardable
    include ReadCopyUpdate

    def put(key, value = Undefined, &block)
      transform { |hash| hash.put(key, value, &block) }
    end

    def store(key, value)
      put(key, value)
      value
    end
    def_delegator :self, :store, :[]=

    def delete(key)
      old_value = nil
      transform do |hash|
        old_value = hash.get(key)
        hash.delete(key)
      end
      old_value
    end
  end
end
