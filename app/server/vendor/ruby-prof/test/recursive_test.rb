#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

# Simple recursive test
def simple(n)
  sleep(1)
  n -= 1
  return if n == 0
  simple(n)
end


# More complicated recursive test
def render_partial(i)
  sleep(1)
  case i
  when 0
    render_partial(10)
  when 1
    2.times do |j|
      render_partial(j + 10)
    end
  end
end

def render
  2.times do |i|
    render_partial(i)
  end
end

# --  Tests ----
class RecursiveTest < Test::Unit::TestCase
  def setup
    # Need to use wall time for this test due to the sleep calls
    RubyProf::measure_mode = RubyProf::WALL_TIME
  end

  def test_simple
    result = RubyProf.profile do
      simple(2)
    end

    # Remove Fixnum+, Fixnum== for less than Ruby 1.9
    result.eliminate_methods!(%w(Fixnum#== Fixnum#-))

    methods = result.threads.first.methods.sort.reverse
    assert_equal(3, methods.length)

    # Method 0: RecursiveTest#test_simple
    method = methods[0]
    assert_equal('RecursiveTest#test_simple', method.full_name)
    assert_equal(1, method.called)
    assert_in_delta(2, method.total_time, 0.1)
    assert_in_delta(0, method.self_time, 0.01)
    assert_in_delta(0, method.wait_time, 0.01)
    assert_in_delta(2, method.children_time, 0.1)

    assert_equal(1, method.call_infos.length)
    call_info = method.call_infos[0]
    assert(!call_info.recursive)
    assert_equal('RecursiveTest#test_simple', call_info.call_sequence)
    assert_equal(1, call_info.children.length)

    # Method 1: Object#simple
    method = methods[1]
    assert_equal('Object#simple', method.full_name)
    assert_equal(2, method.called)
    assert_in_delta(2, method.total_time, 0.1)
    assert_in_delta(0, method.self_time, 0.1)
    assert_in_delta(0, method.wait_time, 0.1)
    assert_in_delta(2, method.children_time, 0.1)

    assert_equal(2, method.call_infos.length)

    call_info = method.call_infos.first
    assert_equal(2, call_info.children.length)
    assert_equal('RecursiveTest#test_simple->Object#simple', call_info.call_sequence)
    assert(!call_info.recursive)

    call_info = method.call_infos.last
    assert_equal(1, call_info.children.length)
    assert_equal('RecursiveTest#test_simple->Object#simple->Object#simple', call_info.call_sequence)
    assert(call_info.recursive)

    method = methods[2]
    assert_equal('Kernel#sleep', method.full_name)
    assert_equal(2, method.called)
    assert_in_delta(2, method.total_time, 0.1)
    assert_in_delta(2, method.self_time, 0.1)
    assert_in_delta(0, method.wait_time, 0.1)
    assert_in_delta(0, method.children_time, 0.1)

    assert_equal(2, method.call_infos.length)
    call_info = method.call_infos[0]
    assert_equal('RecursiveTest#test_simple->Object#simple->Kernel#sleep', call_info.call_sequence)
    assert_equal(0, call_info.children.length)
    assert(!call_info.recursive)

    call_info = method.call_infos[1]
    assert_equal('RecursiveTest#test_simple->Object#simple->Object#simple->Kernel#sleep', call_info.call_sequence)
    assert_equal(0, call_info.children.length)
    assert(!call_info.recursive)
  end

  def test_cycle
    result = RubyProf.profile do
      render
    end

    methods = result.threads.first.methods.sort.reverse
    assert_equal(5, methods.length)

    method = methods[0]
    assert_equal('RecursiveTest#test_cycle', method.full_name)
    assert_equal(1, method.called)
    assert_in_delta(5, method.total_time, 0.1)
    assert_in_delta(0, method.self_time, 0.01)
    assert_in_delta(0, method.wait_time, 0.01)
    assert_in_delta(5, method.children_time, 0.1)

    assert_equal(1, method.call_infos.length)
    call_info = method.call_infos[0]
    assert_equal('RecursiveTest#test_cycle', call_info.call_sequence)
    assert_equal(1, call_info.children.length)
    assert(!call_info.recursive)

    method = methods[1]
    assert_equal('Object#render', method.full_name)
    assert_equal(1, method.called)
    assert_in_delta(5, method.total_time, 0.1)
    assert_in_delta(0, method.self_time, 0.01)
    assert_in_delta(0, method.wait_time, 0.01)
    assert_in_delta(5, method.children_time, 0.1)

    assert_equal(1, method.call_infos.length)
    call_info = method.call_infos[0]
    assert_equal('RecursiveTest#test_cycle->Object#render', call_info.call_sequence)
    assert_equal(1, call_info.children.length)
    assert(!call_info.recursive)

    method = methods[2]
    assert_equal('Integer#times', method.full_name)
    assert_equal(2, method.called)
    assert_in_delta(5, method.total_time, 0.1)
    assert_in_delta(0, method.self_time, 0.1)
    assert_in_delta(0, method.wait_time, 0.1)
    assert_in_delta(5, method.children_time, 0.1)

    assert_equal(2, method.call_infos.length)
    call_info = method.call_infos[0]
    assert_equal('RecursiveTest#test_cycle->Object#render->Integer#times', call_info.call_sequence)
    assert_equal(1, call_info.children.length)
    assert(!call_info.recursive)

    call_info = method.call_infos[1]
    assert_equal('RecursiveTest#test_cycle->Object#render->Integer#times->Object#render_partial->Integer#times', call_info.call_sequence)
    assert_equal(1, call_info.children.length)
    assert(call_info.recursive)

    method = methods[3]
    assert_equal('Object#render_partial', method.full_name)
    assert_equal(5, method.called)
    assert_in_delta(5, method.total_time, 0.1)
    assert_in_delta(0, method.self_time, 0.1)
    assert_in_delta(0, method.wait_time, 0.01)
    assert_in_delta(5, method.children_time, 0.05)

    assert_equal(3, method.call_infos.length)
    call_info = method.call_infos[0]
    assert_equal('RecursiveTest#test_cycle->Object#render->Integer#times->Object#render_partial', call_info.call_sequence)
    assert_equal(3, call_info.children.length)
    assert(!call_info.recursive)

    call_info = method.call_infos[1]
    assert_equal('RecursiveTest#test_cycle->Object#render->Integer#times->Object#render_partial->Object#render_partial', call_info.call_sequence)
    assert_equal(1, call_info.children.length)
    assert(call_info.recursive)

    call_info = method.call_infos[2]
    assert_equal('RecursiveTest#test_cycle->Object#render->Integer#times->Object#render_partial->Integer#times->Object#render_partial', call_info.call_sequence)
    assert_equal(1, call_info.children.length)
    assert(call_info.recursive)

    method = methods[4]
    assert_equal('Kernel#sleep', method.full_name)
    assert_equal(5, method.called)
    assert_in_delta(5, method.total_time, 0.1)
    assert_in_delta(5, method.self_time, 0.1)
    assert_in_delta(0, method.wait_time, 0.01)
    assert_in_delta(0, method.children_time, 0.01)

    assert_equal(3, method.call_infos.length)
    call_info = method.call_infos[0]
    assert_equal('RecursiveTest#test_cycle->Object#render->Integer#times->Object#render_partial->Kernel#sleep', call_info.call_sequence)
    assert_equal(0, call_info.children.length)
    assert(!call_info.recursive)

    call_info = method.call_infos[1]
    assert_equal('RecursiveTest#test_cycle->Object#render->Integer#times->Object#render_partial->Object#render_partial->Kernel#sleep', call_info.call_sequence)
    assert_equal(0, call_info.children.length)
    assert(!call_info.recursive)

    call_info = method.call_infos[2]
    assert_equal('RecursiveTest#test_cycle->Object#render->Integer#times->Object#render_partial->Integer#times->Object#render_partial->Kernel#sleep', call_info.call_sequence)
    assert_equal(0, call_info.children.length)
    assert(!call_info.recursive)
  end
end
