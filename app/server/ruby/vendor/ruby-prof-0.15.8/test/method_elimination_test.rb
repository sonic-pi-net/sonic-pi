#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

# Test data
#     A
#    / \
#   B   C
#        \
#         B

module MethodElimination
  def self.a
    1.times {|i| c}
  end

  def self.b
    sleep 0.1
  end

  def self.c
    1.times {|i| b}
  end
end

class MethodEliminationTest < TestCase
  def setup
    # Need to use wall time for this test due to the sleep calls
    RubyProf::measure_mode = RubyProf::WALL_TIME
  end

  def test_setting_parent
    result = RubyProf.profile do
      1000.times { 1+1 }
    end
    method_infos = result.threads.first.methods.sort.reverse
    assert(m1 = method_infos[0])
    assert(c1 = m1.call_infos.first)
    assert_nil(c1.parent)
  end

  def test_methods_can_be_eliminated
    RubyProf.start
    5.times {MethodElimination.a}
    result = RubyProf.stop

    methods = result.threads.first.methods.sort.reverse

    assert_equal(6, methods.count)
    assert_equal('MethodEliminationTest#test_methods_can_be_eliminated', methods[0].full_name)
    assert_equal('Integer#times', methods[1].full_name)
    assert_equal('<Module::MethodElimination>#a', methods[2].full_name)
    assert_equal('<Module::MethodElimination>#c', methods[3].full_name)
    assert_equal('<Module::MethodElimination>#b', methods[4].full_name)
    assert_equal('Kernel#sleep', methods[5].full_name)

    result.eliminate_methods!([/Integer#times/])

    methods = result.threads.first.methods.sort.reverse
    assert_equal(5, methods.count)
    assert_equal('MethodEliminationTest#test_methods_can_be_eliminated', methods[0].full_name)
    assert_equal('<Module::MethodElimination>#a', methods[1].full_name)
    assert_equal('<Module::MethodElimination>#c', methods[2].full_name)
    assert_equal('<Module::MethodElimination>#b', methods[3].full_name)
    assert_equal('Kernel#sleep', methods[4].full_name)
  end

  private

  def assert_method_has_been_eliminated(result, eliminated_method)
    result.threads.each do |thread|
      thread.methods.each do |method|
        method.call_infos.each do |ci|
          assert(ci.target != eliminated_method, "broken self")
          assert(ci.parent.target != eliminated_method, "broken parent") if ci.parent
          ci.children.each do |callee|
            assert(callee.target != eliminated_method, "broken kid")
          end
        end
      end
    end
  end
end
