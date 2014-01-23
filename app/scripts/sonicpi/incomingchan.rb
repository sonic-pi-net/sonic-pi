require 'thread'
require_relative "util"

module SonicPi
  class IncomingChan

    attr_reader :chan
    POP_SYNC_SEM = Mutex.new
    VALID_MESSAGES = ["/done", "/status.reply", "/synced",
      "/status.reply", "/b_info", "/n_set", "/n_setn", "/c_set",
      "/c_setn", "/n_info", "g_queryTree.reply"]

    def initialize
      @chan = VALID_MESSAGES.inject({}) do |s, el|
        s[el] = Queue.new
        s
      end

      @sync_replies = {}
    end

    def push(msg)
      addr = msg.address
      if("/synced" == addr)
        sync_id = msg.to_a[0]
        POP_SYNC_SEM.synchronize do
          @sync_replies[sync_id] ||= new_sync_queue
        end
      else
        if VALID_MESSAGES.include? addr
          @chan[addr].push msg
        elsif addr == "/fail"
          puts "Server warning:  #{msg.to_a[0].inspect} - #{msg.to_a[1].inspect}"
        else
          puts "IncomingChan - Ignoring unknown incoming addr: #{addr}" if debug_mode
        end
      end
    end

    def pop(osc_path)
      raise 'use pop_sync to pop for /synced messages' if osc_path == "/synced"
      @chan[osc_path].pop
    end

    def pop_done
      pop("/done")
    end

    def pop_status
      pop("/status.reply")
    end

    def pop_buffer_info
      pop("/b_info")
    end

    def pop_buffer_get
      pop("/n_set")
    end

    def pop_buffer_getn
      pop("/n_setn")
    end

    def pop_bus_get
      pop("/c_set")
    end

    def pop_bus_getn
      pop("/c_setn")
    end

    def pop_node_info
      pop("/n_info")
    end

    def pop_tree
      pop("/g_queryTree.reply")
    end

    def pop_sync(sync_id)
      @sync_replies[sync_id].pop
      POP_SYNC_SEM.synchronize do
        @sync_replies.delete sync_id
      end
    end

    private

    def new_sync_queue
      q = Queue.new
      q.push true
      q
    end

  end
end
