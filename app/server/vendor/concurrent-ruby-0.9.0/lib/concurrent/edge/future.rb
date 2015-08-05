require 'concurrent' # TODO do not require whole concurrent gem
require 'concurrent/edge/lock_free_stack'


# @note different name just not to collide for now
module Concurrent
  module Edge

    # Provides edge features, which will be added to or replace features in main gem.
    #
    # Contains new unified implementation of Futures and Promises which combines Features of previous `Future`,
    # `Promise`, `IVar`, `Event`, `Probe`, `dataflow`, `Delay`, `TimerTask` into single framework. It uses extensively
    # new synchronization layer to make all the paths lock-free with exception of blocking threads on `#wait`.
    # It offers better performance and does not block threads (exception being #wait and similar methods where it's
    # intended).
    #
    # ## Examples
    # {include:file:examples/edge_futures.out.rb}
    #
    # @!macro edge_warning
    module FutureShortcuts
      # User is responsible for completing the event once by {Edge::CompletableEvent#complete}
      # @return [CompletableEvent]
      def event(default_executor = :io)
        CompletableEventPromise.new(default_executor).future
      end

      # @overload future(default_executor = :io, &task)
      #   Constructs new Future which will be completed after block is evaluated on executor. Evaluation begins immediately.
      #   @return [Future]
      # @overload future(default_executor = :io)
      #   User is responsible for completing the future once by {Edge::CompletableFuture#success} or {Edge::CompletableFuture#fail}
      #   @return [CompletableFuture]
      def future(default_executor = :io, &task)
        if task
          ImmediateEventPromise.new(default_executor).future.then(&task)
        else
          CompletableFuturePromise.new(default_executor).future
        end
      end

      # @return [Future] which is already completed
      def completed_future(success, value, reason, default_executor = :io)
        ImmediateFuturePromise.new(default_executor, success, value, reason).future
      end

      # @return [Future] which is already completed in success state with value
      def succeeded_future(value, default_executor = :io)
        completed_future true, value, nil, default_executor
      end

      # @return [Future] which is already completed in failed state with reason
      def failed_future(reason, default_executor = :io)
        completed_future false, nil, reason, default_executor
      end

      # @return [Event] which is already completed
      def completed_event(default_executor = :io)
        ImmediateEventPromise.new(default_executor).event
      end

      alias_method :async, :future

      # Constructs new Future which will evaluate to the block after
      # requested by calling `#wait`, `#value`, `#value!`, etc. on it or on any of the chained futures.
      # @return [Future]
      def delay(default_executor = :io, &task)
        Delay.new(default_executor).future.then(&task)
      end

      # Schedules the block to be executed on executor in given intended_time.
      # @param [Numeric, Time] intended_time Numeric => run in `intended_time` seconds. Time => eun on time.
      # @return [Future]
      def schedule(intended_time, default_executor = :io, &task)
        ScheduledPromise.new(default_executor, intended_time).future.then(&task)
      end

      # Constructs new {Future} which is completed after all futures are complete. Its value is array
      # of dependent future values. If there is an error it fails with the first one.
      # @param [Event] futures
      # @return [Future]
      def zip(*futures)
        ZipPromise.new(futures, :io).future
      end

      # Constructs new {Future} which is completed after first of the futures is complete.
      # @param [Event] futures
      # @return [Future]
      def any(*futures)
        AnyPromise.new(futures, :io).future
      end

      # only proof of concept
      # @return [Future]
      def select(*channels)
        probe = future
        channels.each { |ch| ch.select probe }
        probe
      end

      # post job on :fast executor
      # @return [true, false]
      def post!(*args, &job)
        post_on(:fast, *args, &job)
      end

      # post job on :io executor
      # @return [true, false]
      def post(*args, &job)
        post_on(:io, *args, &job)
      end

      # post job on executor
      # @return [true, false]
      def post_on(executor, *args, &job)
        Concurrent.executor(executor).post *args, &job
      end

      # TODO add first(futures, count=count)
      # TODO allow to to have a zip point for many futures and process them in batches by 10
    end

    extend FutureShortcuts
    include FutureShortcuts

    # Represents an event which will happen in future (will be completed). It has to always happen.
    class Event < Synchronization::Object
      include Concern::Deprecation

      # @!visibility private
      class State
        def completed?
          raise NotImplementedError
        end

        def to_sym
          raise NotImplementedError
        end
      end

      # @!visibility private
      class Pending < State
        def completed?
          false
        end

        def to_sym
          :pending
        end
      end

      # @!visibility private
      class Completed < State
        def completed?
          true
        end

        def to_sym
          :completed
        end
      end

      # @!visibility private
      PENDING   = Pending.new
      # @!visibility private
      COMPLETED = Completed.new

      def initialize(promise, default_executor)
        @Promise         = promise
        @DefaultExecutor = default_executor
        @Touched         = AtomicBoolean.new(false)
        @Callbacks       = LockFreeStack.new
        @Waiters         = LockFreeStack.new # TODO replace with AtomicFixnum, avoid aba problem
        @State           = AtomicReference.new PENDING
        super()
        ensure_ivar_visibility!
      end

      # @return [:pending, :completed]
      def state
        @State.get.to_sym
      end

      # Is Event/Future pending?
      # @return [Boolean]
      def pending?(state = @State.get)
        !state.completed?
      end

      def unscheduled?
        raise 'unsupported'
      end

      alias_method :incomplete?, :pending?

      # Has the Event been completed?
      # @return [Boolean]
      def completed?(state = @State.get)
        state.completed?
      end

      alias_method :complete?, :completed?

      # Wait until Event is #complete?
      # @param [Numeric] timeout the maximum time in second to wait.
      # @return [Event, true, false] self or true/false if timeout is used
      # @!macro [attach] edge.periodical_wait
      #   @note a thread should wait only once! For repeated checking use faster `completed?` check.
      #     If thread waits periodically it will dangerously grow the waiters stack.
      def wait(timeout = nil)
        touch
        result = wait_until_complete(timeout)
        timeout ? result : self
      end

      # @!visibility private
      def touch
        # distribute touch to promise only once
        @Promise.touch if @Touched.make_true
        self
      end

      # @return [Executor] current default executor
      # @see #with_default_executor
      def default_executor
        @DefaultExecutor
      end

      # @yield [success, value, reason] of the parent
      def chain(executor = nil, &callback)
        ChainPromise.new(self, @DefaultExecutor, executor || @DefaultExecutor, &callback).future
      end

      alias_method :then, :chain

      def chain_completable(completable_event)
        on_completion! { completable_event.complete_with COMPLETED }
      end

      alias_method :tangle, :chain_completable

      # Zip with future producing new Future
      # @return [Event]
      def zip(other)
        if other.is?(Future)
          ZipFutureEventPromise.new(other, self, @DefaultExecutor).future
        else
          ZipEventEventPromise.new(self, other, @DefaultExecutor).future
        end
      end

      alias_method :&, :zip

      # Inserts delay into the chain of Futures making rest of it lazy evaluated.
      # @return [Event]
      def delay
        ZipEventEventPromise.new(self, Delay.new(@DefaultExecutor).event, @DefaultExecutor).event
      end

      # # Schedules rest of the chain for execution with specified time or on specified time
      # # @return [Event]
      # def schedule(intended_time)
      #   chain do
      #     ZipEventEventPromise.new(self,
      #                              ScheduledPromise.new(@DefaultExecutor, intended_time).event,
      #                              @DefaultExecutor).event
      #   end.flat
      # end

      # Zips with selected value form the suplied channels
      # @return [Future]
      def then_select(*channels)
        ZipFutureEventPromise(Concurrent.select(*channels), self, @DefaultExecutor).future
      end

      # @yield [success, value, reason] executed async on `executor` when completed
      # @return self
      def on_completion(executor = nil, &callback)
        add_callback :pr_async_callback_on_completion, executor || @DefaultExecutor, callback
      end

      # @yield [success, value, reason] executed sync when completed
      # @return self
      def on_completion!(&callback)
        add_callback :pr_callback_on_completion, callback
      end

      # Changes default executor for rest of the chain
      # @return [Event]
      def with_default_executor(executor)
        EventWrapperPromise.new(self, executor).future
      end

      def to_s
        "<##{self.class}:0x#{'%x' % (object_id << 1)} #{state.to_sym}>"
      end

      def inspect
        "#{to_s[0..-2]} blocks:[#{blocks.map(&:to_s).join(', ')}]>"
      end

      def set(*args, &block)
        raise 'Use CompletableEvent#complete or CompletableFuture#complete instead, ' +
                  'constructed by Concurrent.event or Concurrent.future respectively.'
      end

      # @!visibility private
      def complete_with(state, raise_on_reassign = true)
        if @State.compare_and_set(PENDING, state)
          (state)
          # go to synchronized block only if there were waiting threads
          synchronize { ns_broadcast } if @Waiters.clear
          call_callbacks
        else
          Concurrent::MultipleAssignmentError.new('Event can be completed only once') if raise_on_reassign
          return false
        end
        self
      end

      # @!visibility private
      # just for inspection
      # @return [Array<AbstractPromise>]
      def blocks
        @Callbacks.each_with_object([]) do |callback, promises|
          promises.push *callback.select { |v| v.is_a? AbstractPromise }
        end
      end

      # @!visibility private
      # just for inspection
      def callbacks
        @Callbacks.each.to_a
      end

      # @!visibility private
      def add_callback(method, *args)
        if completed?
          call_callback method, *args
        else
          @Callbacks.push [method, *args]
          call_callbacks if completed?
        end
        self
      end

      # @!visibility private
      # only for inspection
      def promise
        @Promise
      end

      # @!visibility private
      # only for inspection
      def touched
        @Touched.value
      end

      # @!visibility private
      # only for debugging inspection
      def waiting_threads
        @Waiters.each.to_a
      end

      # @!visibility private
      def internal_state
        @State.get
      end

      private

      # @return [true, false]
      def wait_until_complete(timeout)
        while true
          last_waiter = @Waiters.peek # waiters' state before completion
          return true if completed?

          # synchronize so it cannot be signaled before it waits
          synchronize do
            # ok only if completing thread did not start signaling
            next unless @Waiters.compare_and_push last_waiter, Thread.current
            return ns_wait_until(timeout) { completed? }
          end
        end
      end

      def pr_with_async(executor, *args, &block)
        Concurrent.post_on(executor, *args, &block)
      end

      def pr_async_callback_on_completion(executor, callback)
        pr_with_async(executor) { pr_callback_on_completion callback }
      end

      def pr_callback_on_completion(callback)
        callback.call
      end

      def pr_callback_notify_blocked(promise)
        promise.on_done self
      end

      def call_callback(method, *args)
        self.send method, *args
      end

      def call_callbacks
        method, *args = @Callbacks.pop
        while method
          call_callback method, *args
          method, *args = @Callbacks.pop
        end
      end
    end

    # Represents a value which will become available in future. May fail with a reason instead.
    class Future < Event
      # @!visibility private
      class CompletedWithResult < Completed
        def result
          [success?, value, reason]
        end

        def success?
          raise NotImplementedError
        end

        def value
          raise NotImplementedError
        end

        def reason
          raise NotImplementedError
        end
      end

      # @!visibility private
      class Success < CompletedWithResult
        def initialize(value)
          @Value = value
        end

        def success?
          true
        end

        def apply(block)
          block.call value
        end

        def value
          @Value
        end

        def reason
          nil
        end

        def to_sym
          :success
        end
      end

      # @!visibility private
      class SuccessArray < Success
        def apply(block)
          block.call *value
        end
      end

      # @!visibility private
      class Failed < CompletedWithResult
        def initialize(reason)
          @Reason = reason
        end

        def success?
          false
        end

        def value
          nil
        end

        def reason
          @Reason
        end

        def to_sym
          :failed
        end

        def apply(block)
          block.call reason
        end
      end

      # @!visibility private
      class PartiallyFailed < CompletedWithResult
        def initialize(value, reason)
          @Value  = value
          @Reason = reason
          super()
        end

        def success?
          false
        end

        def to_sym
          :failed
        end

        def value
          @Value
        end

        def reason
          @Reason
        end

        def apply(block)
          block.call *reason
        end
      end

      # @!method state
      #   @return [:pending, :success, :failed]

      # Has Future been success?
      # @return [Boolean]
      def success?(state = @State.get)
        state.completed? && state.success?
      end

      def fulfilled?
        deprecated_method 'fulfilled?', 'success?'
        success?
      end

      # Has Future been failed?
      # @return [Boolean]
      def failed?(state = @State.get)
        state.completed? && !state.success?
      end

      def rejected?
        deprecated_method 'rejected?', 'failed?'
        failed?
      end

      # @return [Object, nil] the value of the Future when success, nil on timeout
      # @!macro [attach] edge.timeout_nil
      #   @note If the Future can have value `nil` then it cannot be distinquished from `nil` returned on timeout.
      #     In this case is better to use first `wait` then `value` (or similar).
      # @!macro edge.periodical_wait
      def value(timeout = nil)
        touch
        @State.get.value if wait_until_complete timeout
      end

      # @return [Exception, nil] the reason of the Future's failure
      # @!macro edge.timeout_nil
      # @!macro edge.periodical_wait
      def reason(timeout = nil)
        touch
        @State.get.reason if wait_until_complete timeout
      end

      # @return [Array(Boolean, Object, Exception), nil] triplet of success, value, reason
      # @!macro edge.timeout_nil
      # @!macro edge.periodical_wait
      def result(timeout = nil)
        touch
        @State.get.result if wait_until_complete timeout
      end

      # Wait until Future is #complete?
      # @param [Numeric] timeout the maximum time in second to wait.
      # @raise reason on failure
      # @return [Event, true, false] self or true/false if timeout is used
      # @!macro edge.periodical_wait
      def wait!(timeout = nil)
        touch
        result = wait_until_complete!(timeout)
        timeout ? result : self
      end

      # Wait until Future is #complete?
      # @param [Numeric] timeout the maximum time in second to wait.
      # @raise reason on failure
      # @return [Object, nil]
      # @!macro edge.timeout_nil
      # @!macro edge.periodical_wait
      def value!(timeout = nil)
        touch
        @State.get.value if wait_until_complete! timeout
      end

      # @example allows failed Future to be risen
      #   raise Concurrent.future.fail
      def exception(*args)
        raise 'obligation is not failed' unless failed?
        reason = @State.get.reason
        if reason.is_a?(Array)
          reason.each { |e| log Error, 'Edge::Future', e }
          Concurrent::Error.new 'multiple exceptions, inspect log'
        else
          reason.exception(*args)
        end
      end

      # @yield [value] executed only on parent success
      # @return [Future]
      def then(executor = nil, &callback)
        ThenPromise.new(self, @DefaultExecutor, executor || @DefaultExecutor, &callback).future
      end

      # Asks the actor with its value.
      # @return [Future] new future with the response form the actor
      def then_ask(actor)
        self.then { |v| actor.ask(v) }.flat
      end

      def chain_completable(completable_future)
        on_completion! { completable_future.complete_with internal_state }
      end

      alias_method :tangle, :chain_completable

      # @yield [reason] executed only on parent failure
      # @return [Future]
      def rescue(executor = nil, &callback)
        RescuePromise.new(self, @DefaultExecutor, executor || @DefaultExecutor, &callback).future
      end

      # zips with the Future in the value
      # @example
      #   Concurrent.future { Concurrent.future { 1 } }.flat.vale # => 1
      def flat(level = 1)
        FlatPromise.new(self, level, @DefaultExecutor).future
      end

      # @return [Future] which has first completed value from futures
      def any(*futures)
        AnyPromise.new([self, *futures], @DefaultExecutor).future
      end

      # Inserts delay into the chain of Futures making rest of it lazy evaluated.
      # @return [Future]
      def delay
        ZipFutureEventPromise.new(self, Delay.new(@DefaultExecutor).future, @DefaultExecutor).future
      end

      # Schedules rest of the chain for execution with specified time or on specified time
      # @return [Future]
      def schedule(intended_time)
        chain do
          ZipFutureEventPromise.new(self,
                                    ScheduledPromise.new(@DefaultExecutor, intended_time).event,
                                    @DefaultExecutor).future
        end.flat
      end

      # Zips with selected value form the suplied channels
      # @return [Future]
      def then_select(*channels)
        ZipPromise.new([self, Concurrent.select(*channels)], @DefaultExecutor).future
      end

      # Changes default executor for rest of the chain
      # @return [Future]
      def with_default_executor(executor)
        FutureWrapperPromise.new(self, executor).future
      end

      # Zip with future producing new Future
      # @return [Future]
      def zip(other)
        if other.is_a?(Future)
          ZipFutureFuturePromise.new(self, other, @DefaultExecutor).future
        else
          ZipFutureEventPromise.new(self, other, @DefaultExecutor).future
        end
      end

      alias_method :&, :zip

      alias_method :|, :any

      # @note may block
      # @note only proof of concept
      def then_push(channel)
        on_success(:io) { |value| channel.push value }
      end

      # @yield [value] executed async on `executor` when success
      # @return self
      def on_success(executor = nil, &callback)
        add_callback :pr_async_callback_on_success, executor || @DefaultExecutor, callback
      end

      # @yield [reason] executed async on `executor` when failed?
      # @return self
      def on_failure(executor = nil, &callback)
        add_callback :pr_async_callback_on_failure, executor || @DefaultExecutor, callback
      end

      # @yield [value] executed sync when success
      # @return self
      def on_success!(&callback)
        add_callback :pr_callback_on_success, callback
      end

      # @yield [reason] executed sync when failed?
      # @return self
      def on_failure!(&callback)
        add_callback :pr_callback_on_failure, callback
      end

      # @!visibility private
      def complete_with(state, raise_on_reassign = true)
        if @State.compare_and_set(PENDING, state)
          @Waiters.clear
          synchronize { ns_broadcast }
          call_callbacks state
        else
          if raise_on_reassign
            log ERROR, 'Edge::Future', reason if reason # print otherwise hidden error
            raise(Concurrent::MultipleAssignmentError.new(
                      "Future can be completed only once. Current result is #{result}, " +
                          "trying to set #{state.result}"))
          end
          return false
        end
        self
      end

      # @!visibility private
      def add_callback(method, *args)
        state = @State.get
        if completed?(state)
          call_callback method, state, *args
        else
          @Callbacks.push [method, *args]
          state = @State.get
          # take back if it was completed in the meanwhile
          call_callbacks state if completed?(state)
        end
        self
      end

      # @!visibility private
      def apply(block)
        @State.get.apply block
      end

      private

      def wait_until_complete!(timeout = nil)
        result = wait_until_complete(timeout)
        raise self if failed?
        result
      end

      def call_callbacks(state)
        method, *args = @Callbacks.pop
        while method
          call_callback method, state, *args
          method, *args = @Callbacks.pop
        end
      end

      def call_callback(method, state, *args)
        self.send method, state, *args
      end

      def pr_async_callback_on_success(state, executor, callback)
        pr_with_async(executor, state, callback) do |state, callback|
          pr_callback_on_success state, callback
        end
      end

      def pr_async_callback_on_failure(state, executor, callback)
        pr_with_async(executor, state, callback) do |state, callback|
          pr_callback_on_failure state, callback
        end
      end

      def pr_callback_on_success(state, callback)
        state.apply callback if state.success?
      end

      def pr_callback_on_failure(state, callback)
        state.apply callback unless state.success?
      end

      def pr_callback_on_completion(state, callback)
        callback.call state.result
      end

      def pr_callback_notify_blocked(state, promise)
        super(promise)
      end

      def pr_async_callback_on_completion(state, executor, callback)
        pr_with_async(executor, state, callback) do |state, callback|
          pr_callback_on_completion state, callback
        end
      end

    end

    # A Event which can be completed by user.
    class CompletableEvent < Event
      # Complete the Event, `raise` if already completed
      def complete(raise_on_reassign = true)
        complete_with COMPLETED, raise_on_reassign
      end

      def hide_completable
        EventWrapperPromise.new(self, @DefaultExecutor).event
      end
    end

    # A Future which can be completed by user.
    class CompletableFuture < Future
      # Complete the future with triplet od `success`, `value`, `reason`
      # `raise` if already completed
      # return [self]
      def complete(success, value, reason, raise_on_reassign = true)
        complete_with(success ? Success.new(value) : Failed.new(reason), raise_on_reassign)
      end

      # Complete the future with value
      # return [self]
      def success(value)
        promise.success(value)
      end

      # Try to complete the future with value
      # return [self]
      def try_success(value)
        promise.try_success(value)
      end

      # Fail the future with reason
      # return [self]
      def fail(reason = StandardError.new)
        promise.fail(reason)
      end

      # Try to fail the future with reason
      # return [self]
      def try_fail(reason = StandardError.new)
        promise.try_fail(reason)
      end

      # Evaluate the future to value if there is an exception the future fails with it
      # return [self]
      def evaluate_to(*args, &block)
        promise.evaluate_to(*args, block)
      end

      # Evaluate the future to value if there is an exception the future fails with it
      # @raise the exception
      # return [self]
      def evaluate_to!(*args, &block)
        promise.evaluate_to!(*args, block)
      end

      def hide_completable
        FutureWrapperPromise.new(self, @DefaultExecutor).future
      end
    end

    # @abstract
    # @!visibility private
    class AbstractPromise < Synchronization::Object
      def initialize(future)
        @Future = future
        ensure_ivar_visibility!
      end

      def future
        @Future
      end

      alias_method :event, :future

      def default_executor
        future.default_executor
      end

      def state
        future.state
      end

      def touch
      end

      def to_s
        "<##{self.class}:0x#{'%x' % (object_id << 1)} #{state}>"
      end

      def inspect
        to_s
      end

      private

      def complete_with(new_state, raise_on_reassign = true)
        @Future.complete_with(new_state, raise_on_reassign)
      end

      # @return [Future]
      def evaluate_to(*args, block)
        complete_with Future::Success.new(block.call(*args))
      rescue => error
        complete_with Future::Failed.new(error)
      end
    end

    # @!visibility private
    class CompletableEventPromise < AbstractPromise
      def initialize(default_executor)
        super CompletableEvent.new(self, default_executor)
      end
    end

    # @!visibility private
    class CompletableFuturePromise < AbstractPromise
      def initialize(default_executor)
        super CompletableFuture.new(self, default_executor)
      end

      # Set the `Future` to a value and wake or notify all threads waiting on it.
      #
      # @param [Object] value the value to store in the `Future`
      # @raise [Concurrent::MultipleAssignmentError] if the `Future` has already been set or otherwise completed
      # @return [Future]
      def success(value)
        complete_with Future::Success.new(value)
      end

      def try_success(value)
        !!complete_with(Future::Success.new(value), false)
      end

      # Set the `Future` to failed due to some error and wake or notify all threads waiting on it.
      #
      # @param [Object] reason for the failure
      # @raise [Concurrent::MultipleAssignmentError] if the `Future` has already been set or otherwise completed
      # @return [Future]
      def fail(reason = StandardError.new)
        complete_with Future::Failed.new(reason)
      end

      def try_fail(reason = StandardError.new)
        !!complete_with(Future::Failed.new(reason), false)
      end

      public :evaluate_to

      # @return [Future]
      def evaluate_to!(*args, block)
        evaluate_to(*args, block).wait!
      end
    end

    # @abstract
    # @!visibility private
    class InnerPromise < AbstractPromise
    end

    # @abstract
    # @!visibility private
    class BlockedPromise < InnerPromise
      def initialize(future, blocked_by_futures, countdown)
        initialize_blocked_by(blocked_by_futures)
        @Countdown = AtomicFixnum.new countdown

        super(future)
        @BlockedBy.each { |future| future.add_callback :pr_callback_notify_blocked, self }
      end

      # @api private
      def on_done(future)
        countdown   = process_on_done(future)
        completable = completable?(countdown)

        if completable
          on_completable(future)
          # futures could be deleted from blocked_by one by one here, but that would be too expensive,
          # it's done once when all are done to free the reference
          clear_blocked_by!
        end
      end

      def touch
        blocked_by.each(&:touch)
      end

      # !visibility private
      # for inspection only
      def blocked_by
        @BlockedBy
      end

      def inspect
        "#{to_s[0..-2]} blocked_by:[#{ blocked_by.map(&:to_s).join(', ')}]>"
      end

      private

      def initialize_blocked_by(blocked_by_futures)
        @BlockedBy = Array(blocked_by_futures)
      end

      def clear_blocked_by!
        # not synchronized because we do not care when this change propagates
        @BlockedBy = []
        nil
      end

      # @return [true,false] if completable
      def completable?(countdown)
        countdown.zero?
      end

      def process_on_done(future)
        @Countdown.decrement
      end

      def on_completable(done_future)
        raise NotImplementedError
      end
    end

    # @abstract
    # @!visibility private
    class BlockedTaskPromise < BlockedPromise
      def initialize(blocked_by_future, default_executor, executor, &task)
        raise ArgumentError, 'no block given' unless block_given?
        @Executor = executor
        @Task     = task
        super Future.new(self, default_executor), blocked_by_future, 1
      end

      def executor
        @Executor
      end
    end

    # @!visibility private
    class ThenPromise < BlockedTaskPromise
      private

      def initialize(blocked_by_future, default_executor, executor, &task)
        raise ArgumentError, 'only Future can be appended with then' unless blocked_by_future.is_a? Future
        super blocked_by_future, default_executor, executor, &task
      end

      def on_completable(done_future)
        if done_future.success?
          Concurrent.post_on(@Executor, done_future, @Task) do |done_future, task|
            evaluate_to lambda { done_future.apply task }
          end
        else
          complete_with done_future.internal_state
        end
      end
    end

    # @!visibility private
    class RescuePromise < BlockedTaskPromise
      private

      def initialize(blocked_by_future, default_executor, executor, &task)
        super blocked_by_future, default_executor, executor, &task
      end

      def on_completable(done_future)
        if done_future.failed?
          Concurrent.post_on(@Executor, done_future, @Task) do |done_future, task|
            evaluate_to lambda { done_future.apply task }
          end
        else
          complete_with done_future.internal_state
        end
      end
    end

    # @!visibility private
    class ChainPromise < BlockedTaskPromise
      private

      def on_completable(done_future)
        if Future === done_future
          Concurrent.post_on(@Executor, done_future, @Task) { |future, task| evaluate_to *future.result, task }
        else
          Concurrent.post_on(@Executor, @Task) { |task| evaluate_to task }
        end
      end
    end

    # will be immediately completed
    # @!visibility private
    class ImmediateEventPromise < InnerPromise
      def initialize(default_executor)
        super Event.new(self, default_executor).complete_with(Event::COMPLETED)
      end
    end

    # @!visibility private
    class ImmediateFuturePromise < InnerPromise
      def initialize(default_executor, success, value, reason)
        super Future.new(self, default_executor).
                  complete_with(success ? Future::Success.new(value) : Future::Failed.new(reason))
      end
    end

    # @!visibility private
    class FlatPromise < BlockedPromise

      # !visibility private
      def blocked_by
        @BlockedBy.each.to_a
      end

      private

      def process_on_done(future)
        countdown = super(future)
        value     = future.value!
        if countdown.nonzero?
          case value
          when Future
            @BlockedBy.push value
            value.add_callback :pr_callback_notify_blocked, self
            @Countdown.value
          when Event
            raise TypeError, 'cannot flatten to Event'
          else
            raise TypeError, "returned value #{value.inspect} is not a Future"
          end
        end
        countdown
      end

      def initialize(blocked_by_future, levels, default_executor)
        raise ArgumentError, 'levels has to be higher than 0' if levels < 1
        super Future.new(self, default_executor), blocked_by_future, 1 + levels
      end

      def initialize_blocked_by(blocked_by_future)
        @BlockedBy = LockFreeStack.new.push(blocked_by_future)
      end

      def on_completable(done_future)
        complete_with done_future.internal_state
      end

      def clear_blocked_by!
        @BlockedBy.clear
        nil
      end
    end

    # @!visibility private
    class ZipEventEventPromise < BlockedPromise
      def initialize(event1, event2, default_executor)
        super Event.new(self, default_executor), [event1, event2], 2
      end

      def on_completable(done_future)
        complete_with Event::COMPLETED
      end
    end

    # @!visibility private
    class ZipFutureEventPromise < BlockedPromise
      def initialize(future, event, default_executor)
        @FutureResult = future
        super Future.new(self, default_executor), [future, event], 2
      end

      def on_completable(done_future)
        complete_with @FutureResult.internal_state
      end
    end

    # @!visibility private
    class ZipFutureFuturePromise < BlockedPromise
      def initialize(future1, future2, default_executor)
        @Future1Result = future1
        @Future2Result = future2
        super Future.new(self, default_executor), [future1, future2], 2
      end

      def on_completable(done_future)
        success1, value1, reason1 = @Future1Result.result
        success2, value2, reason2 = @Future2Result.result
        success                   = success1 && success2
        new_state                 = if success
                                      Future::SuccessArray.new([value1, value2])
                                    else
                                      Future::PartiallyFailed.new([value1, value2], [reason1, reason2])
                                    end
        complete_with new_state
      end
    end

    # @!visibility private
    class EventWrapperPromise < BlockedPromise
      def initialize(event, default_executor)
        super Event.new(self, default_executor), event, 1
      end

      def on_completable(done_future)
        complete_with Event::COMPLETED
      end
    end

    # @!visibility private
    class FutureWrapperPromise < BlockedPromise
      def initialize(future, default_executor)
        super Future.new(self, default_executor), future, 1
      end

      def on_completable(done_future)
        complete_with done_future.internal_state
      end
    end

    # @!visibility private
    class ZipPromise < BlockedPromise

      private

      def initialize(blocked_by_futures, default_executor)
        klass = Event
        blocked_by_futures.each do |f|
          if f.is_a?(Future)
            if klass == Event
              klass = Future
              break
            end
          end
        end

        # noinspection RubyArgCount
        super(klass.new(self, default_executor), blocked_by_futures, blocked_by_futures.size)

        if blocked_by_futures.empty?
          on_completable nil
        end
      end

      def on_completable(done_future)
        all_success = true
        values      = []
        reasons     = []

        blocked_by.each do |future|
          next unless future.is_a?(Future)
          success, value, reason = future.result

          unless success
            all_success = false
          end

          values << value
          reasons << reason
        end

        if all_success
          if values.empty?
            complete_with Event::COMPLETED
          else
            if values.size == 1
              complete_with Future::Success.new(values.first)
            else
              complete_with Future::SuccessArray.new(values)
            end
          end
        else
          complete_with Future::PartiallyFailed.new(values, reasons)
        end
      end
    end

    # @!visibility private
    class AnyPromise < BlockedPromise

      private

      def initialize(blocked_by_futures, default_executor)
        blocked_by_futures.all? { |f| f.is_a? Future } or
            raise ArgumentError, 'accepts only Futures not Events'
        super(Future.new(self, default_executor), blocked_by_futures, blocked_by_futures.size)
      end

      def completable?(countdown)
        true
      end

      def on_completable(done_future)
        complete_with done_future.internal_state, false
      end
    end

    # @!visibility private
    class Delay < InnerPromise
      def touch
        @Future.complete_with Event::COMPLETED
      end

      private

      def initialize(default_executor)
        super Event.new(self, default_executor)
      end
    end

    # @!visibility private
    class DelayValue < InnerPromise
      def touch
        @Future.complete_with Future::Success.new(@Value)
      end

      private

      def initialize(default_executor, value)
        @Value = value
        super Future.new(self, default_executor)
      end
    end

    # will be evaluated to task in intended_time
    # @!visibility private
    class ScheduledPromise < InnerPromise
      def intended_time
        @IntendedTime
      end

      def inspect
        "#{to_s[0..-2]} intended_time:[#{@IntendedTime}}>"
      end

      private

      def initialize(default_executor, intended_time)
        @IntendedTime = intended_time

        in_seconds = begin
          now           = Time.now
          schedule_time = if @IntendedTime.is_a? Time
                            @IntendedTime
                          else
                            now + @IntendedTime
                          end
          [0, schedule_time.to_f - now.to_f].max
        end

        super Event.new(self, default_executor)

        Concurrent.global_timer_set.post(in_seconds) do
          @Future.complete_with Event::COMPLETED
        end
      end
    end

    # @note proof of concept
    class Channel < Synchronization::Object
      # TODO make lock free
      def initialize
        super
        @ProbeSet = Concurrent::Channel::WaitableList.new
        ensure_ivar_visibility!
      end

      def probe_set_size
        @ProbeSet.size
      end

      def push(value)
        until @ProbeSet.take.try_success([value, self])
        end
      end

      def pop
        select(Concurrent.future)
      end

      def select(probe)
        @ProbeSet.put(probe)
        probe
      end

      def inspect
        to_s
      end
    end
  end

  extend Edge::FutureShortcuts
  include Edge::FutureShortcuts
end
