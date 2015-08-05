module Concurrent
  module Synchronization

    # @!visibility private
    # @!macro internal_implementation_note
    Implementation = case
                     when Concurrent.on_jruby?
                       JavaObject
                     when Concurrent.on_cruby? && Concurrent.ruby_version(:<=, 1, 9, 3)
                       MonitorObject
                     when Concurrent.on_cruby? && Concurrent.ruby_version(:>, 1, 9, 3)
                       MutexObject
                     when Concurrent.on_rbx?
                       RbxObject
                     else
                       warn 'Possibly unsupported Ruby implementation'
                       MutexObject
                     end
    private_constant :Implementation

    # @!macro [attach] synchronization_object
    #  
    #   Safe synchronization under any Ruby implementation.
    #   It provides methods like {#synchronize}, {#wait}, {#signal} and {#broadcast}.
    #   Provides a single layer which can improve its implementation over time without changes needed to
    #   the classes using it. Use {Synchronization::Object} not this abstract class.
    #  
    #   @note this object does not support usage together with
    #     [`Thread#wakeup`](http://ruby-doc.org/core-2.2.0/Thread.html#method-i-wakeup)
    #     and [`Thread#raise`](http://ruby-doc.org/core-2.2.0/Thread.html#method-i-raise).
    #     `Thread#sleep` and `Thread#wakeup` will work as expected but mixing `Synchronization::Object#wait` and
    #     `Thread#wakeup` will not work on all platforms.
    #  
    #   @see {Event} implementation as an example of this class use
    #  
    #   @example simple
    #     class AnClass < Synchronization::Object
    #       def initialize
    #         super
    #         synchronize { @value = 'asd' }
    #       end
    #  
    #       def value
    #         synchronize { @value }
    #       end
    #     end
    #
    class Object < Implementation

      # @!method initialize(*args, &block)
      #   @!macro synchronization_object_method_initialize

      # @!method synchronize
      #   @!macro synchronization_object_method_synchronize

      # @!method initialize(*args, &block)
      #   @!macro synchronization_object_method_ns_initialize

      # @!method wait_until(timeout = nil, &condition)
      #   @!macro synchronization_object_method_ns_wait_until

      # @!method wait(timeout = nil)
      #   @!macro synchronization_object_method_ns_wait

      # @!method signal
      #   @!macro synchronization_object_method_ns_signal

      # @!method broadcast
      #   @!macro synchronization_object_method_ns_broadcast

      # @!method ensure_ivar_visibility!
      #   @!macro synchronization_object_method_ensure_ivar_visibility

      # @!method self.attr_volatile(*names)
      #   @!macro synchronization_object_method_self_attr_volatile
    end
  end
end
