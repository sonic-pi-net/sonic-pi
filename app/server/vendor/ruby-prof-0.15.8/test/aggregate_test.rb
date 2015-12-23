#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

# Test data
#   A   B   C
#   |   |   |
#   Z   A   A
#       |   |
#       Z   Z

class AggClass
  def z
    sleep 1
  end

  def a
    z
  end

  def b
    a
  end

  def c
    a
  end
end

class AggregateTest < TestCase
  def setup
    # Need to use wall time for this test due to the sleep calls
    RubyProf::measure_mode = RubyProf::WALL_TIME
  end

  def test_all_call_infos_are_not_recursive
    c1 = AggClass.new
    result = RubyProf.profile do
      c1.a
      c1.b
      c1.c
    end
    methods = result.threads.first.methods.sort.reverse
    methods.each do |m|
      m.call_infos.each do |ci|
        assert(!ci.recursive)
      end
    end
  end

  def test_call_infos
    c1 = AggClass.new
    result = RubyProf.profile do
      c1.a
      c1.b
      c1.c
    end

    methods = result.threads.first.methods.sort.reverse
    method = methods.find {|meth| meth.full_name == 'AggClass#z'}

    # Check AggClass#z
    assert_equal('AggClass#z', method.full_name)
    assert_equal(3, method.called)
    assert_in_delta(3, method.total_time, 0.05)
    assert_in_delta(0, method.wait_time, 0.05)
    assert_in_delta(0, method.self_time, 0.05)
    assert_in_delta(3, method.children_time, 0.05)
    assert_equal(3, method.call_infos.length)

    call_info = method.call_infos[0]
    assert_equal('AggregateTest#test_call_infos->AggClass#a->AggClass#z', call_info.call_sequence)
    assert_equal(1, call_info.children.length)

    call_info = method.call_infos[1]
    assert_equal('AggregateTest#test_call_infos->AggClass#b->AggClass#a->AggClass#z', call_info.call_sequence)
    assert_equal(1, call_info.children.length)

    call_info = method.call_infos[2]
    assert_equal('AggregateTest#test_call_infos->AggClass#c->AggClass#a->AggClass#z', call_info.call_sequence)
    assert_equal(1, call_info.children.length)
  end

  def test_aggregates_parents
    c1 = AggClass.new
    result = RubyProf.profile do
      c1.a
      c1.b
      c1.c
    end

    methods = result.threads.first.methods.sort.reverse
    method = methods.find {|meth| meth.full_name == 'AggClass#z'}

    # Check AggClass#z
    assert_equal('AggClass#z', method.full_name)

    call_infos = method.aggregate_parents
    assert_equal(1, call_infos.length)

    call_info = call_infos.first
    assert_equal('AggClass#a', call_info.parent.target.full_name)
    assert_in_delta(3, call_info.total_time, 0.05)
    assert_in_delta(0, call_info.wait_time, 0.05)
    assert_in_delta(0, call_info.self_time, 0.05)
    assert_in_delta(3, call_info.children_time, 0.05)
    assert_equal(3, call_info.called)
  end

  def test_aggregates_children
    c1 = AggClass.new
    result = RubyProf.profile do
      c1.a
      c1.b
      c1.c
    end

    methods = result.threads.first.methods.sort.reverse
    method = methods.find {|meth| meth.full_name == 'AggClass#a'}

    # Check AggClass#a
    assert_equal('AggClass#a', method.full_name)

    call_infos = method.aggregate_children
    assert_equal(1, call_infos.length)

    call_info = call_infos.first
    assert_equal('AggClass#z', call_info.target.full_name)
    assert_in_delta(3, call_info.total_time, 0.05)
    assert_in_delta(0, call_info.wait_time, 0.05)
    assert_in_delta(0, call_info.self_time, 0.05)
    assert_in_delta(3, call_info.children_time, 0.05)
    assert_equal(3, call_info.called)
  end
end
