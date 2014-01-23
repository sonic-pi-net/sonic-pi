require_relative "node"

module SonicPi
  class Group < Node

    def initialize(id, comms)
      super(id, comms)
    end

    def clear
      @comms.group_clear @id
      self
    end

    def deep_clear
      @comms.group_deep_clear @id
      self
    end

    def to_s
      "#<SonicPi::Group @id=#{@id}>"
    end
  end

end
