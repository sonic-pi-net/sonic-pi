require "forwardable"
require "hamster/queue"
require "hamster/read_copy_update"

module Hamster
  def self.mutable_queue(*items)
    MutableQueue.new(queue(*items))
  end

  class MutableQueue
    extend Forwardable
    include ReadCopyUpdate

    def enqueue(item)
      transform { |queue| queue.enqueue(item) }
    end
    def_delegate :self, :enqueue, :<<
    def_delegate :self, :enqueue, :add

    def dequeue
      head = nil
      transform do |queue|
        head = queue.head
        queue.dequeue
      end
      head
    end
  end
end
