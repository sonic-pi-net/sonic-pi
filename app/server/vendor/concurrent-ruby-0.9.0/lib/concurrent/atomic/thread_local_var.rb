require 'concurrent/atomic/thread_local_var/weak_key_map'

module Concurrent

  # @!macro [attach] thread_local_var
  #
  #   A `ThreadLocalVar` is a variable where the value is different for each thread.
  #   Each variable may have a default value, but when you modify the variable only
  #   the current thread will ever see that change.
  #   
  #   @example
  #     v = ThreadLocalVar.new(14)
  #     v.value #=> 14
  #     v.value = 2
  #     v.value #=> 2
  #   
  #   @example
  #     v = ThreadLocalVar.new(14)
  #   
  #     t1 = Thread.new do
  #       v.value #=> 14
  #       v.value = 1
  #       v.value #=> 1
  #     end
  #   
  #     t2 = Thread.new do
  #       v.value #=> 14
  #       v.value = 2
  #       v.value #=> 2
  #     end
  #   
  #     v.value #=> 14
  #
  #   @see https://docs.oracle.com/javase/7/docs/api/java/lang/ThreadLocal.html Java ThreadLocal
  #
  # @!visibility private
  class AbstractThreadLocalVar

    # @!visibility private
    NIL_SENTINEL = Object.new
    private_constant :NIL_SENTINEL

    # @!macro [attach] thread_local_var_method_initialize
    #
    #   Creates a thread local variable.
    #
    #   @param [Object] default the default value when otherwise unset
    def initialize(default = nil)
      @default = default
      allocate_storage
    end

    # @!macro [attach] thread_local_var_method_get
    #
    #   Returns the value in the current thread's copy of this thread-local variable.
    #
    #   @return [Object] the current value
    def value
      value = get

      if value.nil?
        @default
      elsif value == NIL_SENTINEL
        nil
      else
        value
      end
    end

    # @!macro [attach] thread_local_var_method_set
    #
    #   Sets the current thread's copy of this thread-local variable to the specified value.
    #   
    #   @param [Object] value the value to set
    #   @return [Object] the new value
    def value=(value)
      bind value
    end

    # @!macro [attach] thread_local_var_method_bind
    #
    #   Bind the given value to thread local storage during
    #   execution of the given block.
    #   
    #   @param [Object] value the value to bind
    #   @yield the operation to be performed with the bound variable
    #   @return [Object] the value
    def bind(value, &block)
      if value.nil?
        stored_value = NIL_SENTINEL
      else
        stored_value = value
      end

      set(stored_value, &block)

      value
    end

    protected

    # @!visibility private
    def allocate_storage
      raise NotImplementedError
    end

    # @!visibility private
    def get
      raise NotImplementedError
    end

    # @!visibility private
    def set(value)
      raise NotImplementedError
    end
  end

  # @!visibility private
  # @!macro internal_implementation_note
  class RubyThreadLocalVar < AbstractThreadLocalVar

    protected

    # @!visibility private
    def allocate_storage
      @storage = WeakKeyMap.new
    end

    # @!visibility private
    def get
      @storage[Thread.current]
    end

    # @!visibility private
    def set(value)
      key = Thread.current

      @storage[key] = value

      if block_given?
        begin
          yield
        ensure
          @storage.delete(key)
        end
      end
    end
  end

  if Concurrent.on_jruby?

    # @!visibility private
    # @!macro internal_implementation_note
    class JavaThreadLocalVar < AbstractThreadLocalVar

      protected

      # @!visibility private
      def allocate_storage
        @var = java.lang.ThreadLocal.new
      end

      # @!visibility private
      def get
        @var.get
      end

      # @!visibility private
      def set(value)
        @var.set(value)
      end
    end
  end

  # @!visibility private
  # @!macro internal_implementation_note
  ThreadLocalVarImplementation = case
                                 when Concurrent.on_jruby?
                                   JavaThreadLocalVar
                                 else
                                   RubyThreadLocalVar
                                 end
  private_constant :ThreadLocalVarImplementation

  # @!macro thread_local_var
  class ThreadLocalVar < ThreadLocalVarImplementation

    # @!method initialize(default = nil)
    #   @!macro thread_local_var_method_initialize

    # @!method value
    #   @!macro thread_local_var_method_get

    # @!method value=(value)
    #   @!macro thread_local_var_method_set

    # @!method bind(value, &block)
    #   @!macro thread_local_var_method_bind

  end
end
