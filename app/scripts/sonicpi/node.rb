module SonicPi
  class Node
    attr_reader :id, :comms

    def initialize(id, comms)
      @id = id
      @comms = comms
      @state = :pending

      killed_event_id = @comms.event_gensym("/sonicpi/node/killed#{id}")
      paused_event_id = @comms.event_gensym("/sonicpi/node/paused#{id}")
      started_event_id = @comms.event_gensym("/sonicpi/node/started#{id}")
      created_event_id = @comms.event_gensym("/sonicpi/node/created#{id}")

      @comms.add_event_handler("/n_end", killed_event_id) do |payload|
        if(id.to_i == payload[0].to_i)
          @state = :destroyed
          [:remove_handlers,
            [ ["/n_go", created_event_id],
              ["/n_off", paused_event_id],
              ["/n_on", started_event_id]]]
        end
      end

      @comms.add_event_handler("/n_off", paused_event_id) do |payload|
        if(id.to_i == payload[0].to_i)
          @state = :paused
        end
      end

      @comms.add_event_handler("/n_on", started_event_id) do |payload|
        if(id.to_i == payload[0].to_i)
          @state = :running
        end
      end

      @comms.add_event_handler("/n_go", created_event_id) do |payload|
        if(id.to_i == payload[0].to_i)
          @state = :running
        end
      end
    end

    def kill
      @comms.kill_node @id
      self
    end

    def ctl(*args)
      @comms.node_ctl @id, *args
      self
    end

    def live?
      !(@state == :destroyed)
    end

    def destroyed?
      @state == :destroyed
    end

    def paused?
      @state == :paused
    end

    def running?
      @state == :running
    end

    def state
      @state
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
