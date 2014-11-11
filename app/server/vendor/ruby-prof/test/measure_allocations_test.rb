#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class MeasureAllocationsTest < Test::Unit::TestCase
  def test_allocations_mode
    RubyProf::measure_mode = RubyProf::ALLOCATIONS
    assert_equal(RubyProf::ALLOCATIONS, RubyProf::measure_mode)
  end

  def test_allocations_enabled_defined
    assert(defined?(RubyProf::ALLOCATIONS_ENABLED))
  end

  if RubyProf::ALLOCATIONS_ENABLED
    def test_allocations
      t = RubyProf.measure_allocations
      assert_kind_of Integer, t

      u = RubyProf.measure_allocations
      assert u > t, [t, u].inspect
    end
  end
end
