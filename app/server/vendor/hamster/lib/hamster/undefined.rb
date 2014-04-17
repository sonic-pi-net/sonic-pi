module Hamster
  module Undefined
    def self.erase(value)
      value unless value.equal?(self)
    end
  end
end
