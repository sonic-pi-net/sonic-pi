require 'concurrent/collection/copy_on_notify_observer_set'
require 'concurrent/collection/copy_on_write_observer_set'

module Concurrent
  module Concern

    # The [observer pattern](http://en.wikipedia.org/wiki/Observer_pattern) is one
    # of the most useful design patterns.
    #
    # The workflow is very simple:
    # - an `observer` can register itself to a `subject` via a callback
    # - many `observers` can be registered to the same `subject`
    # - the `subject` notifies all registered observers when its status changes
    # - an `observer` can deregister itself when is no more interested to receive
    #     event notifications
    #
    # In a single threaded environment the whole pattern is very easy: the
    # `subject` can use a simple data structure to manage all its subscribed
    # `observer`s and every `observer` can react directly to every event without
    # caring about synchronization.
    #
    # In a multi threaded environment things are more complex. The `subject` must
    # synchronize the access to its data structure and to do so currently we're
    # using two specialized ObserverSet: CopyOnWriteObserverSet and
    # CopyOnNotifyObserverSet.
    #
    # When implementing and `observer` there's a very important rule to remember:
    # **there are no guarantees about the thread that will execute the callback**
    #
    # Let's take this example
    # ```
    # class Observer
    #   def initialize
    #     @count = 0
    #   end
    #
    #   def update
    #     @count += 1
    #   end
    # end
    #
    # obs = Observer.new
    # [obj1, obj2, obj3, obj4].each { |o| o.add_observer(obs) }
    # # execute [obj1, obj2, obj3, obj4]
    # ```
    #
    # `obs` is wrong because the variable `@count` can be accessed by different
    # threads at the same time, so it should be synchronized (using either a Mutex
    # or an AtomicFixum)
    module Observable

      # @return [Object] the added observer
      def add_observer(*args, &block)
        observers.add_observer(*args, &block)
      end

      # as #add_observer but it can be used for chaining
      # @return [Observable] self
      def with_observer(*args, &block)
        add_observer(*args, &block)
        self
      end

      # @return [Object] the deleted observer
      def delete_observer(*args)
        observers.delete_observer(*args)
      end

      # @return [Observable] self
      def delete_observers
        observers.delete_observers
        self
      end

      # @return [Integer] the observers count
      def count_observers
        observers.count_observers
      end

      protected

      attr_accessor :observers
    end
  end
end
