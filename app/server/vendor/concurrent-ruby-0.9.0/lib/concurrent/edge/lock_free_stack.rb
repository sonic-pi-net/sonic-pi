module Concurrent
  module Edge
    class LockFreeStack < Synchronization::Object

      class Node
        attr_reader :value, :next_node

        def initialize(value, next_node)
          @value     = value
          @next_node = next_node
        end

        singleton_class.send :alias_method, :[], :new
      end

      class Empty < Node
        def next_node
          self
        end
      end

      EMPTY = Empty[nil, nil]

      def initialize
        @Head = AtomicReference.new EMPTY
        ensure_ivar_visibility!
      end

      def empty?
        @Head.get.equal? EMPTY
      end

      def compare_and_push(head, value)
        @Head.compare_and_set head, Node[value, head]
      end

      def push(value)
        while true
          head = @Head.get
          return self if @Head.compare_and_set head, Node[value, head]
        end
      end

      def peek
        @Head.get
      end

      def compare_and_pop(head)
        @Head.compare_and_set head, head.next_node
      end

      def pop
        while true
          head = @Head.get
          return head.value if @Head.compare_and_set head, head.next_node
        end
      end

      def compare_and_clear(head)
        @Head.compare_and_set head, EMPTY
      end

      def clear
        while true
          head = @Head.get
          return false if head == EMPTY
          return true if @Head.compare_and_set head, EMPTY
        end
      end

      include Enumerable

      def each
        return to_enum unless block_given?
        it = peek
        until it.equal?(EMPTY)
          yield it.value
          it = it.next_node
        end
        self
      end

    end
  end
end
