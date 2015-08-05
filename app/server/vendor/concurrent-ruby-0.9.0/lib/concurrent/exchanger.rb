module Concurrent

  # A synchronization point at which threads can pair and swap elements within
  # pairs. Each thread presents some object on entry to the exchange method,
  # matches with a partner thread, and receives its partner's object on return.
  #
  # Uses `MVar` to manage synchronization of the individual elements.
  # Since `MVar` is also a `Dereferenceable`, the exchanged values support all
  # dereferenceable options. The constructor options hash will be passed to
  # the `MVar` constructors.
  # 
  # @see Concurrent::MVar
  # @see Concurrent::Concern::Dereferenceable
  # @see http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/Exchanger.html java.util.concurrent.Exchanger
  #
  # @!macro edge_warning
  class Exchanger

    EMPTY = Object.new

    # Create a new `Exchanger` object.
    #
    # @param [Hash] opts the options controlling how the managed references
    #   will be processed
    def initialize(opts = {})
      @first = MVar.new(EMPTY, opts)
      @second = MVar.new(MVar::EMPTY, opts)
    end

    # Waits for another thread to arrive at this exchange point (unless the
    # current thread is interrupted), and then transfers the given object to
    # it, receiving its object in return.
    #
    # @param [Object] value the value to exchange with an other thread
    # @param [Numeric] timeout the maximum time in second to wait for one other
    #   thread. nil (default value) means no timeout
    # @return [Object] the value exchanged by the other thread; nil if timed out
    def exchange(value, timeout = nil)
      first = @first.take(timeout)
      if first == MVar::TIMEOUT
        nil
      elsif first == EMPTY
        @first.put value
        second = @second.take timeout
        if second == MVar::TIMEOUT
          nil
        else
          second
        end
      else
        @first.put EMPTY
        @second.put value
        first
      end
    end

  end
end
