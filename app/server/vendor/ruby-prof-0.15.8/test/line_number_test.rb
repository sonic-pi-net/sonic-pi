#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class LineNumbers
  def method1
  end

  def method2
    method1
  end

  def method3
    sleep(1)
  end
end

# --  Tests ----
class LineNumbersTest < TestCase
  def test_function_line_no
    numbers = LineNumbers.new

    result = RubyProf.profile do
      numbers.method2
    end

    methods = result.threads.first.methods.sort.reverse
    assert_equal(3, methods.length)

    method = methods[0]
    assert_equal('LineNumbersTest#test_function_line_no', method.full_name)
    assert_equal(25, method.line)

    method = methods[1]
    assert_equal('LineNumbers#method2', method.full_name)
    assert_equal(10, method.line)

    method = methods[2]
    assert_equal('LineNumbers#method1', method.full_name)
    assert_equal(7, method.line)
  end

  def test_c_function
    numbers = LineNumbers.new

    result = RubyProf.profile do
      numbers.method3
    end

    methods = result.threads.first.methods.sort_by {|method| method.full_name}
    assert_equal(3, methods.length)

    # Methods:
    #   LineNumbers#method3
    #   LineNumbersTest#test_c_function
    #   Kernel#sleep

    method = methods[0]
    assert_equal('Kernel#sleep', method.full_name)
    assert_equal(0, method.line)

    method = methods[1]
    assert_equal('LineNumbers#method3', method.full_name)
    assert_equal(14, method.line)

    method = methods[2]
    assert_equal('LineNumbersTest#test_c_function', method.full_name)
    assert_equal(48, method.line)
  end
end
