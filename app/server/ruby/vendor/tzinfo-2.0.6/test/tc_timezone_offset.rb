# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCTimezoneOffset < Minitest::Test
  include TZInfo

  [:base_utc_offset, :utc_offset].each do |method|
    define_method("test_#{method}") do
      o1 = TimezoneOffset.new(18000, 0, 'TEST')
      o2 = TimezoneOffset.new(-3600, 3600, 'TEST2')

      assert_equal(18000, o1.public_send(method))
      assert_equal(-3600, o2.public_send(method))
    end
  end

  def test_std_offset
    o1 = TimezoneOffset.new(18000, 0, 'TEST')
    o2 = TimezoneOffset.new(-3600, 3600, 'TEST2')

    assert_equal(0, o1.std_offset)
    assert_equal(3600, o2.std_offset)
  end

  [:observed_utc_offset, :utc_total_offset].each do |method|
    define_method("test_#{method}") do
      o1 = TimezoneOffset.new(18000, 0, 'TEST')
      o2 = TimezoneOffset.new(-3600, 3600, 'TEST2')

      assert_equal(18000, o1.public_send(method))
      assert_equal(0, o2.public_send(method))
    end
  end

  %w(abbreviation abbr).each do |method|
    define_method("test_#{method}") do
      o1 = TimezoneOffset.new(18000, 0, 'TEST')
      o2 = TimezoneOffset.new(-3600, 3600, 'TEST2')

      assert_equal('TEST', o1.public_send(method))
      assert_equal('TEST2', o2.public_send(method))
    end

    define_method("test_#{method}_frozen") do
      abbreviation = 'TEST'.dup
      refute(abbreviation.frozen?)
      o = TimezoneOffset.new(18000, 0, abbreviation)
      assert_same(abbreviation, o.public_send(method))
      assert(o.public_send(method).frozen?)
    end
  end

  def test_dst
    o1 = TimezoneOffset.new(18000, 0, 'TEST')
    o2 = TimezoneOffset.new(-3600, 3600, 'TEST2')

    assert_equal(false, o1.dst?)
    assert_equal(true, o2.dst?)
  end

  def test_equality
    o1 = TimezoneOffset.new(18000, 0, 'TEST')
    o2 = TimezoneOffset.new(18000, 0, 'TEST')
    o3 = TimezoneOffset.new(18001, 0, 'TEST')
    o4 = TimezoneOffset.new(18000, 1, 'TEST')
    o5 = TimezoneOffset.new(18000, 0, 'TEST2')

    assert_equal(true, o1 == o1)
    assert_equal(true, o1 == o2)
    assert_equal(false, o1 == o3)
    assert_equal(false, o1 == o4)
    assert_equal(false, o1 == o5)
    assert_equal(false, o1 == Object.new)
  end

  def test_eql
    o1 = TimezoneOffset.new(18000, 0, 'TEST')
    o2 = TimezoneOffset.new(18000, 0, 'TEST')
    o3 = TimezoneOffset.new(18001, 0, 'TEST')
    o4 = TimezoneOffset.new(18000, 1, 'TEST')
    o5 = TimezoneOffset.new(18000, 0, 'TEST2')

    assert_equal(true, o1.eql?(o1))
    assert_equal(true, o1.eql?(o2))
    assert_equal(false, o1.eql?(o3))
    assert_equal(false, o1.eql?(o4))
    assert_equal(false, o1.eql?(o5))
    assert_equal(false, o1.eql?(Object.new))
  end

  def test_hash
    o1 = TimezoneOffset.new(18000, 0, 'TEST')
    o2 = TimezoneOffset.new(-3600, 3600, 'TEST2')

    assert_equal([18000, 0, 'TEST'].hash, o1.hash)
    assert_equal([-3600, 3600, 'TEST2'].hash, o2.hash)
  end

  def test_inspect
    o = TimezoneOffset.new(18000, 0, 'TEST')
    assert_equal('#<TZInfo::TimezoneOffset: @base_utc_offset=18000, @std_offset=0, @abbreviation=TEST>', o.inspect)
  end
end
