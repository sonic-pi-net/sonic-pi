#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class MeasureMemoryTest < TestCase
  include MemoryTestHelper

  def test_memory_mode
    RubyProf::measure_mode = RubyProf::MEMORY
    assert_equal(RubyProf::MEMORY, RubyProf::measure_mode)
  end

  def test_memory_enabled_defined
    assert(defined?(RubyProf::MEMORY_ENABLED))
  end

  if RubyProf::MEMORY_ENABLED
    def test_memory
      RubyProf::measure_mode = RubyProf::MEMORY
      RubyProf.enable_gc_stats_if_needed
      t = RubyProf.measure_memory
      assert_kind_of Float, t
      u = RubyProf.measure_memory
      assert_operator u, :>, t
      total = memory_test_helper
      assert(total > 0, 'Should measure more than zero kilobytes of memory usage')
      refute_equal(0, total % 1, 'Should not truncate fractional kilobyte measurements')
    ensure
      RubyProf.disable_gc_stats_if_needed
    end
  end
end
