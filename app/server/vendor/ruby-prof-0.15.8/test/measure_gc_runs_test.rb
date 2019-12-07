#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class MeasureGCRunsTest < TestCase
  include MemoryTestHelper

  def test_gc_runs_mode
    RubyProf::measure_mode = RubyProf::GC_RUNS
    assert_equal(RubyProf::GC_RUNS, RubyProf::measure_mode)
  end

  def test_gc_runs_enabled_defined
    assert(defined?(RubyProf::GC_RUNS_ENABLED))
  end

  if RubyProf::GC_RUNS_ENABLED
    def test_gc_runs
      t = RubyProf.measure_gc_runs
      assert_kind_of Integer, t

      GC.enable_stats if GC.respond_to?(:enable_stats)
      GC.start

      u = RubyProf.measure_gc_runs
      assert u > t, [t, u].inspect
      RubyProf::measure_mode = RubyProf::GC_RUNS
      memory_test_helper
    end
  end
end
