require 'concurrent/synchronization'

module Concurrent
  module Collection

    # A thread safe observer set implemented using copy-on-read approach:
    # observers are added and removed from a thread safe collection; every time
    # a notification is required the internal data structure is copied to
    # prevent concurrency issues
    # 
    # @api private
    class CopyOnNotifyObserverSet < Synchronization::Object

      def initialize
        super()
        synchronize { ns_initialize }
      end

      # Adds an observer to this set. If a block is passed, the observer will be
      # created by this method and no other params should be passed
      #
      # @param [Object] observer the observer to add
      # @param [Symbol] func the function to call on the observer during notification.
      #   Default is :update
      # @return [Object] the added observer
      def add_observer(observer=nil, func=:update, &block)
        if observer.nil? && block.nil?
          raise ArgumentError, 'should pass observer as a first argument or block'
        elsif observer && block
          raise ArgumentError.new('cannot provide both an observer and a block')
        end

        if block
          observer = block
          func     = :call
        end

        synchronize do
          @observers[observer] = func
          observer
        end
      end

      # @param [Object] observer the observer to remove
      # @return [Object] the deleted observer
      def delete_observer(observer)
        synchronize do
          @observers.delete(observer)
          observer
        end
      end

      # Deletes all observers
      # @return [CopyOnWriteObserverSet] self
      def delete_observers
        synchronize do
          @observers.clear
          self
        end
      end

      # @return [Integer] the observers count
      def count_observers
        synchronize { @observers.count }
      end

      # Notifies all registered observers with optional args
      # @param [Object] args arguments to be passed to each observer
      # @return [CopyOnWriteObserverSet] self
      def notify_observers(*args, &block)
        observers = duplicate_observers
        notify_to(observers, *args, &block)
        self
      end

      # Notifies all registered observers with optional args and deletes them.
      #
      # @param [Object] args arguments to be passed to each observer
      # @return [CopyOnWriteObserverSet] self
      def notify_and_delete_observers(*args, &block)
        observers = duplicate_and_clear_observers
        notify_to(observers, *args, &block)
        self
      end

      protected

      def ns_initialize
        @observers = {}
      end

      private

      def duplicate_and_clear_observers
        synchronize do
          observers = @observers.dup
          @observers.clear
          observers
        end
      end

      def duplicate_observers
        synchronize { observers = @observers.dup }
      end

      def notify_to(observers, *args)
        raise ArgumentError.new('cannot give arguments and a block') if block_given? && !args.empty?
        observers.each do |observer, function|
          args = yield if block_given?
          observer.send(function, *args)
        end
      end
    end
  end
end
