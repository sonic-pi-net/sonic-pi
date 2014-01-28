require_relative "util"
require_relative "studio"
require_relative "incomingevents"
require_relative "counter"
require_relative "promise"
require_relative "mods/spmidi"
#require_relative "mods/graphics"
require_relative "mods/sound"
#require_relative "mods/feeds"
#require_relative "mods/globalkeys"

require 'thread'
require 'fileutils'

module SonicPi
  class Spider

    attr_reader :event_queue

    def initialize(hostname, port, msg_queue, max_concurrent_synths)
      @msg_queue = msg_queue
      @event_queue = Queue.new
      @keypress_handlers = {}
      message "Starting..."
      @events = IncomingEvents.new
      @sync_counter = Counter.new
      Thread.new do
        loop do
          event = @event_queue.pop
          handle_event event
        end
      end
    end

    #These includes must happen after the initialize method
    #as they may potentially redefine it to extend behaviour
    include SonicPi::Mods::SPMIDI
#    include SonicPi::Mods::Graphics
    include SonicPi::Mods::Sound
#    include SonicPi::Mods::Feeds
#    include SonicPi::Mods::GlobalKeys

    def sync(id, res)
      @events.event("/sync", {:id => id, :result => res})
    end

    def handle_event(e)
      case e[:type]
      when :keypress
        @keypress_handlers.values.each{|h| h.call(e[:val])}
        else
          puts "Unknown event: #{e}"
        end
    end

    def on_keypress(&block)
      @keypress_handlers[:foo] = block
    end

    def message(s)
      @msg_queue.push({:type => :message, :val => s.to_s})
    end

    def sync_msg_command(msg)
      id = @sync_counter.next
      prom = Promise.new
      @events.add_handler("/sync", @events.gensym("/spider")) do |payload|
        if payload[:id] == id
          prom.deliver! payload[:result]
          :remove_handler
        end
      end
      msg[:sync] = id
      @msg_queue.push msg
      prom.get
    end

    def spider_eval(code)
      eval(code)
      STDOUT.flush
    end

    def print(output)
      message output
    end

    def puts(output)
      message output
    end

  end
end
