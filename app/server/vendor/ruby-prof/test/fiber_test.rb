#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)
require 'timeout'
require 'set'
begin
  require 'fiber'
rescue LoadError
end

# --  Tests ----
class FiberTest < Test::Unit::TestCase

  def fiber_test
    @fiber_ids << Fiber.current.object_id
    enum = Enumerator.new do |yielder|
        [1,2].each do |x|
          @fiber_ids << Fiber.current.object_id
          sleep 0.1
          yielder.yield x
        end
      end
    while true
      begin
        enum.next
      rescue StopIteration
        break
      end
    end
    sleep 0.1
  end

  def setup
    # Need to use wall time for this test due to the sleep calls
    RubyProf::measure_mode = RubyProf::WALL_TIME
    @fiber_ids  = Set.new
    @root_fiber = Fiber.current.object_id
    @thread_id  = Thread.current.object_id
    @result     = RubyProf.profile { fiber_test }
  end

  def test_fibers
    profiled_fiber_ids = @result.threads.map(&:fiber_id)
    assert_equal(2, @result.threads.length)
    assert_equal([@thread_id], @result.threads.map(&:id).uniq)
    assert_equal(@fiber_ids, Set.new(profiled_fiber_ids))

    assert profiled_fiber_ids.include?(@root_fiber)
    assert(root_fiber_profile = @result.threads.detect{|t| t.fiber_id == @root_fiber})
    assert(enum_fiber_profile = @result.threads.detect{|t| t.fiber_id != @root_fiber})

    assert_in_delta(0.3, root_fiber_profile.total_time, 0.05)
    assert_in_delta(0.2, enum_fiber_profile.total_time, 0.05)

    assert(method_next = root_fiber_profile.methods.detect{|m| m.full_name == "Enumerator#next"})
    assert(method_each = enum_fiber_profile.methods.detect{|m| m.full_name == "Enumerator#each"})

    assert_in_delta(0.2, method_next.total_time, 0.05)
    assert_in_delta(0.2, method_each.total_time, 0.05)

    # RubyProf::CallInfoPrinter.new(@result).print
  end

end
