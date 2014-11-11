#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class MeasureCpuTimeTest < Test::Unit::TestCase
  def setup
    RubyProf::measure_mode = RubyProf::CPU_TIME
  end

  def test_mode
    RubyProf::measure_mode = RubyProf::CPU_TIME
    assert_equal(RubyProf::CPU_TIME, RubyProf::measure_mode)
  end

  def test_cpu_time_enabled_defined
    assert(defined?(RubyProf::CPU_TIME_ENABLED))
  end

  def test_class_methods
    result = RubyProf.profile do
      RubyProf::C7.hello
    end

    # Length should be greater 2:
    #   MeasureCpuTimeTest#test_class_methods
    #   <Class::RubyProf::C1>#hello
    #   ....

    methods = result.threads.first.methods.sort.reverse[0..1]
    assert_equal(2, methods.length)

    # Check the names
    assert_equal('MeasureCpuTimeTest#test_class_methods', methods[0].full_name)
    assert_equal('<Class::RubyProf::C7>#hello', methods[1].full_name)

    # Check times
    assert_in_delta(0.1, methods[0].total_time, 0.02)
    assert_in_delta(0, methods[0].wait_time, 0.02)
    assert_in_delta(0, methods[0].self_time, 0.02)

    assert_in_delta(0.1, methods[1].total_time, 0.02)
    assert_in_delta(0, methods[1].wait_time, 0.02)
    assert_in_delta(0, methods[1].self_time, 0.02)
  end

  def test_instance_methods
    result = RubyProf.profile do
      RubyProf::C7.new.hello
    end

    methods = result.threads.first.methods.sort.reverse[0..1]
    assert_equal(2, methods.length)

    # Methods at this point:
    #   MeasureCpuTimeTest#test_instance_methods
    #   C7#hello
    #   ...

    names = methods.map(&:full_name)
    assert_equal('MeasureCpuTimeTest#test_instance_methods', names[0])
    assert_equal('RubyProf::C7#hello', names[1])


    # Check times
    assert_in_delta(0.2, methods[0].total_time, 0.03)
    assert_in_delta(0, methods[0].wait_time, 0.03)
    assert_in_delta(0, methods[0].self_time, 0.03)

    assert_in_delta(0.2, methods[1].total_time, 0.03)
    assert_in_delta(0, methods[1].wait_time, 0.03)
    assert_in_delta(0, methods[1].self_time, 0.1)
  end

  def test_module_methods
    result = RubyProf.profile do
      RubyProf::C8.hello
    end

    # Methods:
    #   MeasureCpuTimeTest#test_module_methods
    #   M1#hello
    #   ...

    methods = result.threads.first.methods.sort.reverse[0..1]
    assert_equal(2, methods.length)

    assert_equal('MeasureCpuTimeTest#test_module_methods', methods[0].full_name)
    assert_equal('RubyProf::M7#hello', methods[1].full_name)

    # Check times
    assert_in_delta(0.3, methods[0].total_time, 0.1)
    assert_in_delta(0, methods[0].wait_time, 0.02)
    assert_in_delta(0, methods[0].self_time, 0.02)

    assert_in_delta(0.3, methods[1].total_time, 0.1)
    assert_in_delta(0, methods[1].wait_time, 0.02)
    assert_in_delta(0, methods[1].self_time, 0.1)
  end

  def test_module_instance_methods
    result = RubyProf.profile do
      RubyProf::C8.new.hello
    end

    # Methods:
    #   MeasureCpuTimeTest#test_module_instance_methods
    #   M7#hello
    #   ...

    methods = result.threads.first.methods.sort.reverse[0..1]
    assert_equal(2, methods.length)
    names = methods.map(&:full_name)
    assert_equal('MeasureCpuTimeTest#test_module_instance_methods', names[0])
    assert_equal('RubyProf::M7#hello', names[1])

    # Check times
    assert_in_delta(0.3, methods[0].total_time, 0.1)
    assert_in_delta(0, methods[0].wait_time, 0.1)
    assert_in_delta(0, methods[0].self_time, 0.1)

    assert_in_delta(0.3, methods[1].total_time, 0.02)
    assert_in_delta(0, methods[1].wait_time, 0.01)
    assert_in_delta(0, methods[1].self_time, 0.05)
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

    methods = result.threads.first.methods.sort.reverse
    assert_equal(2, methods.length)

    assert_equal('MeasureCpuTimeTest#test_singleton', methods[0].full_name)
    assert_equal('<Object::RubyProf::C3>#hello', methods[1].full_name)

    assert_in_delta(0, methods[0].total_time, 0.01)
    assert_in_delta(0, methods[0].wait_time, 0.01)
    assert_in_delta(0, methods[0].self_time, 0.01)

    assert_in_delta(0, methods[1].total_time, 0.01)
    assert_in_delta(0, methods[1].wait_time, 0.01)
    assert_in_delta(0, methods[1].self_time, 0.01)
  end
end
