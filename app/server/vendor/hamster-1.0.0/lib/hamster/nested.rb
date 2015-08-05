require "set"
require "hamster/hash"
require "hamster/set"
require "hamster/vector"

module Hamster
  class << self

    # Create a Hamster immutable data structure with nested Hamster data
    # structure from a nested Ruby object `obj`.  This method recursively
    # "walks" the Ruby object, converting Ruby `Hash` to `Hamster::Hash`, Ruby
    # `Array` to `Hamster::Vector` and Ruby `Set` to `Hamster::Set`.  Other
    # Ruby objects are left as-is.
    #
    # @example
    #   h = Hamster.from({ "a" => [1, 2], "b" => "c" })
    #   # => Hamster::Hash["a" => Hamster::Vector[1, 2], "b" => "c"]
    #
    # @return [Hamster::Hash, Hamster::Vector, Hamster::Set, Object]
    def from(obj)
      case obj
      when ::Hash
        res = obj.map { |key, value| [from(key), from(value)] }
        Hamster::Hash.new(res)
      when ::Array
        res = obj.map { |element| from(element) }
        Hamster::Vector.new(res)
      when ::Set
        res = obj.map { |element| from(element) }
        Hamster::Set.new(res)
      else
        obj
      end
    end
  end
end
