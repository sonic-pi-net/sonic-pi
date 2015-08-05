module Concurrent
  module Collection

    # @!macro [attach] priority_queue
    #
    #   A queue collection in which the elements are sorted based on their
    #   comparison (spaceship) operator `<=>`. Items are added to the queue
    #   at a position relative to their priority. On removal the element
    #   with the "highest" priority is removed. By default the sort order is
    #   from highest to lowest, but a lowest-to-highest sort order can be
    #   set on construction.
    #
    #   The API is based on the `Queue` class from the Ruby standard library.
    #
    #   The pure Ruby implementation, `MutexPriorityQueue` uses a heap algorithm
    #   stored in an array. The algorithm is based on the work of Robert Sedgewick
    #   and Kevin Wayne.
    #
    #   The JRuby native implementation is a thin wrapper around the standard
    #   library `java.util.PriorityQueue`.
    #
    #   When running under JRuby the class `PriorityQueue` extends `JavaPriorityQueue`.
    #   When running under all other interpreters it extends `MutexPriorityQueue`.
    #
    #   @note This implementation is *not* thread safe.
    #
    #   @see http://en.wikipedia.org/wiki/Priority_queue
    #   @see http://ruby-doc.org/stdlib-2.0.0/libdoc/thread/rdoc/Queue.html
    #
    #   @see http://algs4.cs.princeton.edu/24pq/index.php#2.6
    #   @see http://algs4.cs.princeton.edu/24pq/MaxPQ.java.html
    #
    #   @see http://docs.oracle.com/javase/7/docs/api/java/util/PriorityQueue.html
    # 
    # @!visibility private
    # @!macro internal_implementation_note
    class MutexPriorityQueue

      # @!macro [attach] priority_queue_method_initialize
      #
      #   Create a new priority queue with no items.
      #  
      #   @param [Hash] opts the options for creating the queue
      #   @option opts [Symbol] :order (:max) dictates the order in which items are
      #     stored: from highest to lowest when `:max` or `:high`; from lowest to
      #     highest when `:min` or `:low`
      def initialize(opts = {})
        order = opts.fetch(:order, :max)
        @comparator = [:min, :low].include?(order) ? -1 : 1
        clear
      end

      # @!macro [attach] priority_queue_method_clear
      #
      #   Removes all of the elements from this priority queue.
      def clear
        @queue = [nil]
        @length = 0
        true
      end

      # @!macro [attach] priority_queue_method_delete
      #
      #   Deletes all items from `self` that are equal to `item`.
      #  
      #   @param [Object] item the item to be removed from the queue
      #   @return [Object] true if the item is found else false
      def delete(item)
        original_length = @length
        k = 1
        while k <= @length
          if @queue[k] == item
            swap(k, @length)
            @length -= 1
            sink(k)
            @queue.pop
          else
            k += 1
          end
        end
        @length != original_length
      end

      # @!macro [attach] priority_queue_method_empty
      #  
      #   Returns `true` if `self` contains no elements.
      #  
      #   @return [Boolean] true if there are no items in the queue else false
      def empty?
        size == 0
      end

      # @!macro [attach] priority_queue_method_include
      #
      #   Returns `true` if the given item is present in `self` (that is, if any
      #   element == `item`), otherwise returns false.
      #  
      #   @param [Object] item the item to search for
      #  
      #   @return [Boolean] true if the item is found else false
      def include?(item)
        @queue.include?(item)
      end
      alias_method :has_priority?, :include?

      # @!macro [attach] priority_queue_method_length
      #  
      #   The current length of the queue.
      #  
      #   @return [Fixnum] the number of items in the queue
      def length
        @length
      end
      alias_method :size, :length

      # @!macro [attach] priority_queue_method_peek
      #  
      #   Retrieves, but does not remove, the head of this queue, or returns `nil`
      #   if this queue is empty.
      #   
      #   @return [Object] the head of the queue or `nil` when empty
      def peek
        @queue[1]
      end

      # @!macro [attach] priority_queue_method_pop
      #  
      #   Retrieves and removes the head of this queue, or returns `nil` if this
      #   queue is empty.
      #   
      #   @return [Object] the head of the queue or `nil` when empty
      def pop
        max = @queue[1]
        swap(1, @length)
        @length -= 1
        sink(1)
        @queue.pop
        max
      end
      alias_method :deq, :pop
      alias_method :shift, :pop

      # @!macro [attach] priority_queue_method_push
      #  
      #   Inserts the specified element into this priority queue.
      #  
      #   @param [Object] item the item to insert onto the queue
      def push(item)
        @length += 1
        @queue << item
        swim(@length)
        true
      end
      alias_method :<<, :push
      alias_method :enq, :push

      # @!macro [attach] priority_queue_method_from_list
      #  
      #   Create a new priority queue from the given list.
      #  
      #   @param [Enumerable] list the list to build the queue from
      #   @param [Hash] opts the options for creating the queue
      #  
      #   @return [PriorityQueue] the newly created and populated queue
      def self.from_list(list, opts = {})
        queue = new(opts)
        list.each{|item| queue << item }
        queue
      end

      protected

      # Exchange the values at the given indexes within the internal array.
      # 
      # @param [Integer] x the first index to swap
      # @param [Integer] y the second index to swap
      # 
      # @!visibility private
      def swap(x, y)
        temp = @queue[x]
        @queue[x] = @queue[y]
        @queue[y] = temp
      end

      # Are the items at the given indexes ordered based on the priority
      # order specified at construction?
      #
      # @param [Integer] x the first index from which to retrieve a comparable value
      # @param [Integer] y the second index from which to retrieve a comparable value
      #
      # @return [Boolean] true if the two elements are in the correct priority order
      #   else false
      # 
      # @!visibility private
      def ordered?(x, y)
        (@queue[x] <=> @queue[y]) == @comparator
      end

      # Percolate down to maintain heap invariant.
      # 
      # @param [Integer] k the index at which to start the percolation
      # 
      # @!visibility private
      def sink(k)
        while (j = (2 * k)) <= @length do
          j += 1 if j < @length && ! ordered?(j, j+1)
          break if ordered?(k, j)
          swap(k, j)
          k = j
        end
      end

      # Percolate up to maintain heap invariant.
      # 
      # @param [Integer] k the index at which to start the percolation
      # 
      # @!visibility private
      def swim(k)
        while k > 1 && ! ordered?(k/2, k) do
          swap(k, k/2)
          k = k/2
        end
      end
    end

    if Concurrent.on_jruby?

      # @!macro priority_queue
      # 
      # @!visibility private
      # @!macro internal_implementation_note
      class JavaPriorityQueue

        # @!macro priority_queue_method_initialize
        def initialize(opts = {})
          order = opts.fetch(:order, :max)
          if [:min, :low].include?(order)
            @queue = java.util.PriorityQueue.new(11) # 11 is the default initial capacity
          else
            @queue = java.util.PriorityQueue.new(11, java.util.Collections.reverseOrder())
          end
        end

        # @!macro priority_queue_method_clear
        def clear
          @queue.clear
          true
        end

        # @!macro priority_queue_method_delete
        def delete(item)
          found = false
          while @queue.remove(item) do
            found = true
          end
          found
        end

        # @!macro priority_queue_method_empty
        def empty?
          @queue.size == 0
        end

        # @!macro priority_queue_method_include
        def include?(item)
          @queue.contains(item)
        end
        alias_method :has_priority?, :include?

        # @!macro priority_queue_method_length
        def length
          @queue.size
        end
        alias_method :size, :length

        # @!macro priority_queue_method_peek
        def peek
          @queue.peek
        end

        # @!macro priority_queue_method_pop
        def pop
          @queue.poll
        end
        alias_method :deq, :pop
        alias_method :shift, :pop

        # @!macro priority_queue_method_push
        def push(item)
          @queue.add(item)
        end
        alias_method :<<, :push
        alias_method :enq, :push

        # @!macro priority_queue_method_from_list
        def self.from_list(list, opts = {})
          queue = new(opts)
          list.each{|item| queue << item }
          queue
        end
      end
    end

    # @!visibility private
    # @!macro internal_implementation_note
    PriorityQueueImplementation = case
                                  when Concurrent.on_jruby?
                                    JavaPriorityQueue
                                  else
                                    MutexPriorityQueue
                                  end
    private_constant :PriorityQueueImplementation

    # @!macro priority_queue
    # 
    # @!visibility private
    class PriorityQueue < PriorityQueueImplementation

      alias_method :has_priority?, :include?

      alias_method :size, :length

      alias_method :deq, :pop
      alias_method :shift, :pop

      alias_method :<<, :push
      alias_method :enq, :push

      # @!method initialize(opts = {})
      #   @!macro priority_queue_method_initialize

      # @!method clear
      #   @!macro priority_queue_method_clear

      # @!method delete(item)
      #   @!macro priority_queue_method_delete

      # @!method empty?
      #   @!macro priority_queue_method_empty

      # @!method include?(item)
      #   @!macro priority_queue_method_include

      # @!method length
      #   @!macro priority_queue_method_length

      # @!method peek
      #   @!macro priority_queue_method_peek

      # @!method pop
      #   @!macro priority_queue_method_pop

      # @!method push(item)
      #   @!macro priority_queue_method_push

      # @!method self.from_list(list, opts = {})
      #   @!macro priority_queue_method_from_list
    end
  end
end
