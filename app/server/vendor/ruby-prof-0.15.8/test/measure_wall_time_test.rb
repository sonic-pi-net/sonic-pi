#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class MeasureWallTimeTest < TestCase
  def setup
    # Need to use wall time for this test due to the sleep calls
    RubyProf::measure_mode = RubyProf::WALL_TIME
  end

  def test_mode
    RubyProf::measure_mode = RubyProf::WALL_TIME
    assert_equal(RubyProf::WALL_TIME, RubyProf::measure_mode)
  end

  def test_wall_time_enabled_defined
    assert(defined?(RubyProf::WALL_TIME_ENABLED))
  end

  def test_class_methods
    result = RubyProf.profile do
      RubyProf::C1.hello
    end

    thread = result.threads.first
    assert_in_delta(0.1, thread.total_time, 0.01)

    top_methods = thread.top_methods
    assert_equal(1, top_methods.count)
    assert_equal("MeasureWallTimeTest#test_class_methods", top_methods[0].full_name)

    # Length should be 3:
    #   MeasureWallTimeTest#test_class_methods
    #   <Class::RubyProf::C1>#hello
    #   Kernel#sleep

    methods = result.threads.first.methods.sort.reverse
    assert_equal(3, methods.length)

    # Check the names
    assert_equal('MeasureWallTimeTest#test_class_methods', methods[0].full_name)
    assert_equal('<Class::RubyProf::C1>#hello', methods[1].full_name)
    assert_equal('Kernel#sleep', methods[2].full_name)

    # Check times
    assert_in_delta(0.1, methods[0].total_time, 0.01)
    assert_in_delta(0, methods[0].wait_time, 0.01)
    assert_in_delta(0, methods[0].self_time, 0.01)

    assert_in_delta(0.1, methods[1].total_time, 0.01)
    assert_in_delta(0, methods[1].wait_time, 0.01)
    assert_in_delta(0, methods[1].self_time, 0.01)

    assert_in_delta(0.1, methods[2].total_time, 0.01)
    assert_in_delta(0, methods[2].wait_time, 0.01)
    assert_in_delta(0.1, methods[2].self_time, 0.01)
  end

  def test_instance_methods
    result = RubyProf.profile do
      RubyProf::C1.new.hello
    end

    thread = result.threads.first
    assert_in_delta(0.2, thread.total_time, 0.01)

    top_methods = thread.top_methods
    assert_equal(1, top_methods.count)
    assert_equal("MeasureWallTimeTest#test_instance_methods", top_methods[0].full_name)

    # Methods called
    #   MeasureWallTimeTest#test_instance_methods
    #   Class.new
    #   Class:Object#allocate
    #   for Object#initialize
    #   C1#hello
    #   Kernel#sleep

    methods = result.threads.first.methods.sort.reverse
    assert_equal(RubyProf.ruby_2? ? 5 : 6, methods.length)
    names = methods.map(&:full_name)
    assert_equal('MeasureWallTimeTest#test_instance_methods', names[0])
    assert_equal('RubyProf::C1#hello', names[1])
    assert_equal('Kernel#sleep', names[2])
    assert_equal('Class#new', names[3])

    # order can differ
    assert(names.include?("#{RubyProf.parent_object}#initialize"))
    unless RubyProf.ruby_2?
      assert(names.include?("<Class::#{RubyProf.parent_object}>#allocate"))
    end

    # Check times
    assert_in_delta(0.2, methods[0].total_time, 0.02)
    assert_in_delta(0, methods[0].wait_time, 0.02)
    assert_in_delta(0, methods[0].self_time, 0.02)

    assert_in_delta(0.2, methods[1].total_time, 0.02)
    assert_in_delta(0, methods[1].wait_time, 0.02)
    assert_in_delta(0, methods[1].self_time, 0.02)

    assert_in_delta(0.2, methods[2].total_time, 0.02)
    assert_in_delta(0, methods[2].wait_time, 0.02)
    assert_in_delta(0.2, methods[2].self_time, 0.02)

    assert_in_delta(0, methods[3].total_time, 0.01)
    assert_in_delta(0, methods[3].wait_time, 0.01)
    assert_in_delta(0, methods[3].self_time, 0.01)

    assert_in_delta(0, methods[4].total_time, 0.01)
    assert_in_delta(0, methods[4].wait_time, 0.01)
    assert_in_delta(0, methods[4].self_time, 0.01)

    unless RubyProf.ruby_2?
      assert_in_delta(0, methods[5].total_time, 0.01)
      assert_in_delta(0, methods[5].wait_time, 0.01)
      assert_in_delta(0, methods[5].self_time, 0.01)
    end
  end

  def test_module_methods
    result = RubyProf.profile do
      RubyProf::C2.hello
    end

    thread = result.threads.first
    assert_in_delta(0.3, thread.total_time, 0.01)

    top_methods = thread.top_methods
    assert_equal(1, top_methods.count)
    assert_equal("MeasureWallTimeTest#test_module_methods", top_methods[0].full_name)

    # Methods:
    #   MeasureWallTimeTest#test_module_methods
    #   M1#hello
    #   Kernel#sleep

    methods = result.threads.first.methods.sort.reverse
    assert_equal(3, methods.length)

    assert_equal('MeasureWallTimeTest#test_module_methods', methods[0].full_name)
    assert_equal('RubyProf::M1#hello', methods[1].full_name)
    assert_equal('Kernel#sleep', methods[2].full_name)

    # Check times
    assert_in_delta(0.3, methods[0].total_time, 0.1)
    assert_in_delta(0, methods[0].wait_time, 0.02)
    assert_in_delta(0, methods[0].self_time, 0.02)

    assert_in_delta(0.3, methods[1].total_time, 0.1)
    assert_in_delta(0, methods[1].wait_time, 0.02)
    assert_in_delta(0, methods[1].self_time, 0.02)

    assert_in_delta(0.3, methods[2].total_time, 0.1)
    assert_in_delta(0, methods[2].wait_time, 0.02)
    assert_in_delta(0.3, methods[2].self_time, 0.1)
  end

  def test_module_instance_methods
    result = RubyProf.profile do
      RubyProf::C2.new.hello
    end

    thread = result.threads.first
    assert_in_delta(0.3, thread.total_time, 0.01)

    top_methods = thread.top_methods
    assert_equal(1, top_methods.count)
    assert_equal("MeasureWallTimeTest#test_module_instance_methods", top_methods[0].full_name)

    # Methods:
    #   MeasureWallTimeTest#test_module_instance_methods
    #   Class#new
    #   <Class::Object>#allocate
    #   Object#initialize
    #   M1#hello
    #   Kernel#sleep

    methods = result.threads.first.methods.sort.reverse
    assert_equal(RubyProf.ruby_2? ? 5 : 6, methods.length)
    names = methods.map(&:full_name)
    assert_equal('MeasureWallTimeTest#test_module_instance_methods', names[0])
    assert_equal('RubyProf::M1#hello', names[1])
    assert_equal('Kernel#sleep', names[2])
    assert_equal('Class#new', names[3])

    # order can differ
    assert(names.include?("#{RubyProf.parent_object}#initialize"))
    unless RubyProf.ruby_2?
      assert(names.include?("<Class::#{RubyProf.parent_object}>#allocate"))
    end

    # Check times
    assert_in_delta(0.3, methods[0].total_time, 0.1)
    assert_in_delta(0, methods[0].wait_time, 0.1)
    assert_in_delta(0, methods[0].self_time, 0.1)

    assert_in_delta(0.3, methods[1].total_time, 0.02)
    assert_in_delta(0, methods[1].wait_time, 0.01)
    assert_in_delta(0, methods[1].self_time, 0.01)

    assert_in_delta(0.3, methods[2].total_time, 0.02)
    assert_in_delta(0, methods[2].wait_time, 0.01)
    assert_in_delta(0.3, methods[2].self_time, 0.02)

    assert_in_delta(0, methods[3].total_time, 0.01)
    assert_in_delta(0, methods[3].wait_time, 0.01)
    assert_in_delta(0, methods[3].self_time, 0.01)

    assert_in_delta(0, methods[4].total_time, 0.01)
    assert_in_delta(0, methods[4].wait_time, 0.01)
    assert_in_delta(0, methods[4].self_time, 0.01)

    unless RubyProf.ruby_2?
      assert_in_delta(0, methods[5].total_time, 0.01)
      assert_in_delta(0, methods[5].wait_time, 0.01)
      assert_in_delta(0, methods[5].self_time, 0.01)
    end
  end

  def test_singleton
    c3 = RubyProf::C3.new

    class << c3
      def hello
      end
    end

    result = RubyProf.profile do
      c3.hello
    end

    thread = result.threads.first
    assert_in_delta(0.0, thread.total_time, 0.01)

    top_methods = thread.top_methods
    assert_equal(1, top_methods.count)
    assert_equal("MeasureWallTimeTest#test_singleton", top_methods[0].full_name)

    methods = result.threads.first.methods.sort.reverse
    assert_equal(2, methods.length)

    assert_equal('MeasureWallTimeTest#test_singleton', methods[0].full_name)
    assert_equal('<Object::RubyProf::C3>#hello', methods[1].full_name)

    assert_in_delta(0, methods[0].total_time, 0.01)
    assert_in_delta(0, methods[0].wait_time, 0.01)
    assert_in_delta(0, methods[0].self_time, 0.01)

    assert_in_delta(0, methods[1].total_time, 0.01)
    assert_in_delta(0, methods[1].wait_time, 0.01)
    assert_in_delta(0, methods[1].self_time, 0.01)
  end
end
