require 'rbconfig'

module Concurrent
  module TestHelpers
    extend self

    def pool_termination_timeout
      5
    end

    def delta(v1, v2)
      if block_given?
        v1 = yield(v1)
        v2 = yield(v2)
      end
      return (v1 - v2).abs
    end

    def monotonic_interval
      raise ArgumentError.new('no block given') unless block_given?
      start_time = Concurrent.monotonic_time
      yield
      Concurrent.monotonic_time - start_time
    end

    def in_fiber(&block)
      Fiber.new(&block)
    end

    def in_thread(*arguments, &block)
      @created_threads ||= Queue.new
      new_thread = Thread.new(*arguments) do |*args, &b|
        Thread.abort_on_exception = true
        block.call(*args, &b)
      end
      @created_threads << new_thread
      new_thread
    end

    def is_sleeping(thread)
      expect(in_thread { Thread.pass until thread.status == 'sleep' }.join(1)).not_to eq nil
    end

    def repeat_until_success(timeout = 5, &test)
      start_time = Concurrent.monotonic_time
      last_exception = nil
      while Concurrent.monotonic_time - start_time < timeout
        begin
          test.call
          return true
        rescue Exception => e
          last_exception = e
          Thread.pass
        end
      end
      raise last_exception
    end

    def join_with(threads, timeout = 5)
      threads = Array(threads)
      threads.each do |t|
        joined_thread = t.join(timeout * threads.size)
        expect(joined_thread).not_to eq nil
      end
    end
  end
end
