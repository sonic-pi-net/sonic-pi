# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCWithOffset < Minitest::Test
  include TZInfo

  class TestBaseClass
    attr_reader :format
    attr_accessor :strftime_result

    def strftime(format)
      @format = format
      @strftime_result
    end
  end

  class TestClass < TestBaseClass
    include TZInfo::WithOffset

    attr_accessor :timezone_offset

    def if_timezone_offset_test(value)
      if_timezone_offset(value) {|p,v| yield(p, v) }
    end
  end

  def strftime_test(expected_super_format, format, abbreviation)
    t = TestClass.new
    t.timezone_offset = abbreviation ? TimezoneOffset.new(0, 0, abbreviation) : nil
    expected_result = 'super_result'
    t.strftime_result = expected_result
    assert_same(expected_result, t.strftime(format))
    assert_equal(expected_super_format, t.format)
  end

  def test_strftime
    strftime_test('%H:%M:%S BST', '%H:%M:%S %Z', 'BST')
    strftime_test('BST', '%Z', 'BST')
    strftime_test('%%ZBST', '%%Z%Z', 'BST')
    strftime_test('BST BST', '%Z %Z', 'BST')
    strftime_test('BST %%Z %%BST %%%%Z %%%%BST', '%Z %%Z %%%Z %%%%Z %%%%%Z', 'BST')
  end

  def test_strftime_handles_percent_in_abbreviation
    strftime_test('%%H:%%M:%%S', '%Z', '%H:%M:%S')
  end

  def test_strftime_nil_timezone_offset
    strftime_test('%Z', '%Z', nil)
  end

  def test_strftime_nil_format
    t = TestClass.new
    t.timezone_offset = TimezoneOffset.new(0, 0, 'BST')
    error = assert_raises(ArgumentError) { t.strftime(nil) }
    assert_match(/\bformat\b/, error.message)
  end

  def test_if_timezone_offset
    o = TimezoneOffset.new(0, 0, 'BST')
    t = TestClass.new
    t.timezone_offset = o
    v = Object.new
    r = Object.new

    block_called = 0

    result = t.if_timezone_offset_test(v) do |bo,bv|
      block_called += 1
      assert_same(o, bo)
      assert_same(v, bv)
      r
    end

    assert_same(r, result)
    assert_equal(1, block_called)
  end

  def test_if_timezone_offset_nil_timezone_offset
    t = TestClass.new
    v = Object.new

    result = t.if_timezone_offset_test(v) {|bo,bv| raise 'block called unexpectedly' }
    assert_same(result, v)
  end
end
