module Rugged
  class Reference
    def inspect
      "#<#{self.class}:#{object_id} {name: #{name.inspect}, target: #{target.inspect}}>"
    end
  end
end
