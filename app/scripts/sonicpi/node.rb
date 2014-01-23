module SonicPi
  class Node
    attr_reader :id, :comms

    def initialize(id, comms)
      @id = id
      @comms = comms
    end

    def kill
      @comms.kill_node @id
      self
    end

    def ctl(*args)
      @comms.node_ctl @id, *args
    end

    def to_i
      @id
    end

    def to_f
      @id.to_f
    end

    def to_s
      "#<SonicPi::Node @id=#{@id}>"
    end

    def inspect
      to_s
    end
  end
end
