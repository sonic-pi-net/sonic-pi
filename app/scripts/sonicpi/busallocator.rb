require_relative "bus"

module SonicPi
  class BusAllocator
    def initialize(max_bus_id, idx_offset)
      @idx_offset = idx_offset
      @max_bus_id = max_bus_id
      @bus_ids = []
      @bus_ids[max_bus_id] = nil
      @busses = []
      @sem = Mutex.new
    end

    def allocate(num_adj_busses)
      @sem.synchronize do
        idx = find_gap(0, num_adj_busses)
        (idx...idx+num_adj_busses).each {|i| @bus_ids[i] = true}
        offsetted = idx + @idx_offset
        bus = bus_class.new(offsetted, num_adj_busses, self){|id, size| release!(id-@idx_offset, size)}
        @busses << bus
        bus
      end
    end

    def release(bus)
      @bus.free
    end

    def reset!
      @sem.synchronize do
        @busses.each {|b| b.free}
        @busses = []
        @bus_ids = []
      end
    end

    private

    def bus_class
      raise "Implement me!"
    end

    def release!(idx, num_adj_busses)
      @sem.synchronize do
        (idx...idx+num_adj_busses).each {|i| @bus_ids[i] = false}
      end
    end

    def valid_gap?(idx, gap_size)
      bus_seg = @bus_ids[idx...idx+gap_size]
      bus_seg.all?{|el| !el}
    end

    def find_gap(idx, gap_size)
      raise "Unable to allocate bus" if (idx > @max_bus_id)

      if valid_gap?(idx, gap_size)
        idx
      else
        find_gap idx+1, gap_size
      end
    end
  end
end
