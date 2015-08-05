module Concurrent
  module Synchronization

    if Concurrent.on_rbx?
      
      # @!visibility private
      # @!macro internal_implementation_note
      class RbxObject < AbstractObject
        def initialize
          @Waiters = []
          ensure_ivar_visibility!
        end

        protected

        def synchronize(&block)
          Rubinius.synchronize(self, &block)
        end

        def ns_wait(timeout = nil)
          wchan = Rubinius::Channel.new

          begin
            @Waiters.push wchan
            Rubinius.unlock(self)
            signaled = wchan.receive_timeout timeout
          ensure
            Rubinius.lock(self)

            if !signaled && !@Waiters.delete(wchan)
              # we timed out, but got signaled afterwards,
              # so pass that signal on to the next waiter
              @Waiters.shift << true unless @Waiters.empty?
            end
          end

          self
        end

        def ns_signal
          @Waiters.shift << true unless @Waiters.empty?
          self
        end

        def ns_broadcast
          @Waiters.shift << true until @Waiters.empty?
          self
        end

        def ensure_ivar_visibility!
          # Rubinius instance variables are not volatile so we need to insert barrier
          Rubinius.memory_barrier
        end

        def self.attr_volatile *names
          names.each do |name|
            ivar = :"@volatile_#{name}"
            class_eval <<-RUBY, __FILE__, __LINE__ + 1
            def #{name}
              Rubinius.memory_barrier
              #{ivar}
            end

            def #{name}=(value)
              #{ivar} = value
              Rubinius.memory_barrier
            end
            RUBY
          end
          names.map { |n| [n, :"#{n}="] }.flatten
        end
      end
    end
  end
end
