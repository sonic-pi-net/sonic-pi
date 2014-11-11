#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class MeasureProcessTimeTest < Test::Unit::TestCase
  def setup
    # Need to fix this for linux (windows works since PROCESS_TIME is WALL_TIME anyway)
    RubyProf::measure_mode = RubyProf::PROCESS_TIME
  end

  def test_mode
    assert_equal(RubyProf::PROCESS_TIME, RubyProf::measure_mode)
  end

  def test_process_time_enabled_defined
    assert(defined?(RubyProf::PROCESS_TIME_ENABLED))
  end

  def test_primes
    start = Process.times
    result = RubyProf.profile do
      run_primes(10000)
    end
    finish = Process.times

    total_time = (finish.utime - start.utime) + (finish.stime - start.stime)

    thread = result.threads.first
    assert_in_delta(total_time, thread.total_time, 0.03)

    methods = result.threads.first.methods.sort.reverse

    # WTF?
    if RUBY_VERSION =~ /^1\.9\.3/
      assert_equal 16, methods.length
    elsif RUBY_VERSION =~ /^2\.0/
      assert_equal 15, methods.length
    else # if RUBY_VERSION =~ /^2\.1/
      assert_equal 14, methods.length
    end
    # puts methods.map(&:full_name).inspect

    # Check times
    assert_equal("MeasureProcessTimeTest#test_primes", methods[0].full_name)
    assert_in_delta(total_time, methods[0].total_time, 0.02)
    assert_in_delta(0.0, methods[0].wait_time, 0.01)
    assert_in_delta(0.0, methods[0].self_time, 0.01)

    assert_equal("Object#run_primes", methods[1].full_name)
    assert_in_delta(total_time, methods[1].total_time, 0.02)
    assert_in_delta(0.0, methods[1].wait_time, 0.01)
    assert_in_delta(0.0, methods[1].self_time, 0.01)

    assert_equal("Object#find_primes", methods[2].full_name)
    assert_equal("Array#select", methods[3].full_name)
    assert_equal("Object#is_prime", methods[4].full_name)
    assert_equal("Integer#upto", methods[5].full_name)
    assert_equal("Object#make_random_array", methods[6].full_name)
    assert_equal("Array#each_index", methods[7].full_name)
    assert_equal("Kernel#rand", methods[8].full_name)
  end
end
