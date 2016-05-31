module Rugged
  class Object
    def <=>(other)
      self.oid <=> other.oid
    end
  end
end
