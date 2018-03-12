#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class MeasureGCTimeTest < TestCase
  include MemoryTestHelper

  def test_gc_time_mode
    RubyProf::measure_mode = RubyProf::GC_TIME
    assert_equal(RubyProf::GC_TIME, RubyProf::measure_mode)
  end

  def test_gc_time_enabled_defined
    assert(defined?(RubyProf::GC_TIME_ENABLED))
  end

  if RubyProf::GC_TIME_ENABLED
    def test_gc_time
      RubyProf::measure_mode = RubyProf::GC_TIME
      RubyProf.enable_gc_stats_if_needed

      t = RubyProf.measure_gc_time
      assert_kind_of Float, t

      GC.start

      u = RubyProf.measure_gc_time
      assert u > t, [t, u].inspect

      memory_test_helper
    ensure
      RubyProf.disable_gc_stats_if_needed
    end
  end
end
