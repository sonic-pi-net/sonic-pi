require 'thread'
require 'concurrent/collection/copy_on_write_observer_set'
require 'concurrent/concern/dereferenceable'
require 'concurrent/concern/observable'
require 'concurrent/concern/logging'
require 'concurrent/executor/executor'
require 'concurrent/concern/deprecation'

module Concurrent

  # `Agent`s are inspired by [Clojure's](http://clojure.org/) [agent](http://clojure.org/agents) function. An `Agent` is a single atomic value that represents an identity. The current value of the `Agent` can be requested at any time (`deref`). Each `Agent` has a work queue and operates on the global thread pool (see below). Consumers can `post` code blocks to the `Agent`. The code block (function) will receive the current value of the `Agent` as its sole parameter. The return value of the block will become the new value of the `Agent`. `Agent`s support two error handling modes: fail and continue. A good example of an `Agent` is a shared incrementing counter, such as the score in a video game. 
  # 
  # An `Agent` must be initialize with an initial value. This value is always accessible via the `value` (or `deref`) methods. Code blocks sent to the `Agent` will be processed in the order received. As each block is processed the current value is updated with the result from the block. This update is an atomic operation so a `deref` will never block and will always return the current value. 
  # 
  # When an `Agent` is created it may be given an optional `validate` block and zero or more `rescue` blocks. When a new value is calculated the value will be checked against the validator, if present. If the validator returns `true` the new value will be accepted. If it returns `false` it will be rejected. If a block raises an exception during execution the list of `rescue` blocks will be seacrhed in order until one matching the current exception is found. That `rescue` block will then be called an passed the exception object. If no matching `rescue` block is found, or none were configured, then the exception will be suppressed. 
  # 
  # `Agent`s also implement Ruby's [Observable](http://ruby-doc.org/stdlib-2.0/libdoc/observer/rdoc/Observable.html). Code that observes an `Agent` will receive a callback with the new value any time the value is changed. 
  #
  # @!macro copy_options
  # 
  # @example Simple Example
  # 
  #   require 'concurrent'
  #   
  #   score = Concurrent::Agent.new(10)
  #   score.value #=> 10
  #   
  #   score << proc{|current| current + 100 }
  #   sleep(0.1)
  #   score.value #=> 110
  #   
  #   score << proc{|current| current * 2 }
  #   sleep(0.1)
  #   score.value #=> 220
  #   
  #   score << proc{|current| current - 50 }
  #   sleep(0.1)
  #   score.value #=> 170
  # 
  # @example With Validation and Error Handling
  # 
  #   score = Concurrent::Agent.new(0).validate{|value| value <= 1024 }.
  #             rescue(NoMethodError){|ex| puts "Bam!" }.
  #             rescue(ArgumentError){|ex| puts "Pow!" }.
  #             rescue{|ex| puts "Boom!" }
  #   score.value #=> 0
  #   
  #   score << proc{|current| current + 2048 }
  #   sleep(0.1)
  #   score.value #=> 0
  #   
  #   score << proc{|current| raise ArgumentError }
  #   sleep(0.1)
  #   #=> puts "Pow!"
  #   score.value #=> 0
  #   
  #   score << proc{|current| current + 100 }
  #   sleep(0.1)
  #   score.value #=> 100
  # 
  # @example With Observation
  # 
  #   bingo = Class.new{
  #     def update(time, score)
  #       puts "Bingo! [score: #{score}, time: #{time}]" if score >= 100
  #     end
  #   }.new
  #   
  #   score = Concurrent::Agent.new(0)
  #   score.add_observer(bingo)
  #   
  #   score << proc{|current| sleep(0.1); current += 30 }
  #   score << proc{|current| sleep(0.1); current += 30 }
  #   score << proc{|current| sleep(0.1); current += 30 }
  #   score << proc{|current| sleep(0.1); current += 30 }
  #   
  #   sleep(1)
  #   #=> Bingo! [score: 120, time: 2013-07-22 21:26:08 -0400]
  #
  # @!attribute [r] timeout
  #   @return [Fixnum] the maximum number of seconds before an update is cancelled
  #
  # @!macro edge_warning
  class Agent
    include Concern::Dereferenceable
    include Concern::Observable
    include Concern::Logging
    include Concern::Deprecation

    attr_reader :timeout, :io_executor, :fast_executor

    # Initialize a new Agent with the given initial value and provided options.
    #
    # @param [Object] initial the initial value
    #
    # @!macro executor_and_deref_options
    def initialize(initial, opts = {})
      @value                = initial
      @rescuers             = []
      @validator            = Proc.new { |result| true }
      self.observers        = Collection::CopyOnWriteObserverSet.new
      @serialized_execution = SerializedExecution.new
      @io_executor          = Executor.executor_from_options(opts) || Concurrent.global_io_executor
      @fast_executor        = Executor.executor_from_options(opts) || Concurrent.global_fast_executor
      init_mutex
      set_deref_options(opts)
    end

    # Specifies a block fast to be performed when an update fast raises
    # an exception. Rescue blocks will be checked in order they were added. The first
    # block for which the raised exception "is-a" subclass of the given `clazz` will
    # be called. If no `clazz` is given the block will match any caught exception.
    # This behavior is intended to be identical to Ruby's `begin/rescue/end` behavior.
    # Any number of rescue handlers can be added. If no rescue handlers are added then
    # caught exceptions will be suppressed.
    #
    # @param [Exception] clazz the class of exception to catch
    # @yield the block to be called when a matching exception is caught
    # @yieldparam [StandardError] ex the caught exception
    #
    # @example
    #   score = Concurrent::Agent.new(0).
    #             rescue(NoMethodError){|ex| puts "Bam!" }.
    #             rescue(ArgumentError){|ex| puts "Pow!" }.
    #             rescue{|ex| puts "Boom!" }
    #
    #   score << proc{|current| raise ArgumentError }
    #   sleep(0.1)
    #   #=> puts "Pow!"
    def rescue(clazz = StandardError, &block)
      unless block.nil?
        mutex.synchronize do
          @rescuers << Rescuer.new(clazz, block)
        end
      end
      self
    end

    alias_method :catch, :rescue
    alias_method :on_error, :rescue

    # A block fast to be performed after every update to validate if the new
    # value is valid. If the new value is not valid then the current value is not
    # updated. If no validator is provided then all updates are considered valid.
    #
    # @yield the block to be called after every update fast to determine if
    #   the result is valid
    # @yieldparam [Object] value the result of the last update fast
    # @yieldreturn [Boolean] true if the value is valid else false
    def validate(&block)

      unless block.nil?
        begin
          mutex.lock
          @validator = block
        ensure
          mutex.unlock
        end
      end
      self
    end

    alias_method :validates, :validate
    alias_method :validate_with, :validate
    alias_method :validates_with, :validate

    # Update the current value with the result of the given block fast,
    # block should not do blocking calls, use #post_off for blocking calls
    #
    # @yield the fast to be performed with the current value in order to calculate
    #   the new value
    # @yieldparam [Object] value the current value
    # @yieldreturn [Object] the new value
    # @return [true, nil] nil when no block is given
    def post(&block)
      post_on(@fast_executor, &block)
    end

    # Update the current value with the result of the given block fast,
    # block can do blocking calls
    #
    # @param [Fixnum, nil] timeout [DEPRECATED] maximum number of seconds before an update is cancelled
    #
    # @yield the fast to be performed with the current value in order to calculate
    #   the new value
    # @yieldparam [Object] value the current value
    # @yieldreturn [Object] the new value
    # @return [true, nil] nil when no block is given
    def post_off(timeout = nil, &block)
      task = if timeout
               deprecated 'post_off with option timeout options is deprecated and will be removed'
               lambda do |value|
                 future = Future.execute do
                   block.call(value)
                 end
                 if future.wait(timeout)
                   future.value!
                 else
                   raise Concurrent::TimeoutError
                 end
               end
             else
               block
             end
      post_on(@io_executor, &task)
    end

    # Update the current value with the result of the given block fast,
    # block should not do blocking calls, use #post_off for blocking calls
    #
    # @yield the fast to be performed with the current value in order to calculate
    #   the new value
    # @yieldparam [Object] value the current value
    # @yieldreturn [Object] the new value
    def <<(block)
      post(&block)
      self
    end

    # Waits/blocks until all the updates sent before this call are done.
    #
    # @param [Numeric] timeout the maximum time in second to wait.
    # @return [Boolean] false on timeout, true otherwise
    def await(timeout = nil)
      done = Event.new
      post { |val| done.set; val }
      done.wait timeout
    end

    private

    def post_on(executor, &block)
      return nil if block.nil?
      @serialized_execution.post(executor) { work(&block) }
      true
    end

    # @!visibility private
    Rescuer = Struct.new(:clazz, :block) # :nodoc:

    # @!visibility private
    def try_rescue(ex) # :nodoc:
      rescuer = mutex.synchronize do
        @rescuers.find { |r| ex.is_a?(r.clazz) }
      end
      rescuer.block.call(ex) if rescuer
    rescue Exception => ex
      # suppress
      log DEBUG, ex
    end

    # @!visibility private
    def work(&handler) # :nodoc:
      validator, value = mutex.synchronize { [@validator, @value] }

      begin
        result = handler.call(value)
        valid  = validator.call(result)
      rescue Exception => ex
        exception = ex
      end

      begin
        mutex.lock
        should_notify = if !exception && valid
                          @value = result
                          true
                        end
      ensure
        mutex.unlock
      end

      if should_notify
        time = Time.now
        observers.notify_observers { [time, self.value] }
      end

      try_rescue(exception)
    end
  end
end
