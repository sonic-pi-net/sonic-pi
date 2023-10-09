# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCOffsetTimezonePeriod < Minitest::Test
  include TZInfo

  def test_initialize
    o = TimezoneOffset.new(-7200, 3600, 'SPECIAL')
    p = OffsetTimezonePeriod.new(o)
    assert_same(o, p.offset)
  end

  def test_initialize_nil
    error = assert_raises(ArgumentError) { OffsetTimezonePeriod.new(nil) }
    assert_match(/\boffset\b/, error.message)
  end

  def test_start_transition
    p = OffsetTimezonePeriod.new(TimezoneOffset.new(-7200, 3600, 'SPECIAL'))
    assert_nil(p.start_transition)
  end

  def test_end_transition
    p = OffsetTimezonePeriod.new(TimezoneOffset.new(-7200, 3600, 'SPECIAL'))
    assert_nil(p.end_transition)
  end

  def test_equality
    o1 = TimezoneOffset.new(0, 3600, 'TEST')
    o2 = TimezoneOffset.new(0, 0, 'TEST')

    p1 = OffsetTimezonePeriod.new(o1)
    p2 = OffsetTimezonePeriod.new(o1)
    p3 = OffsetTimezonePeriod.new(o2)
    p4 = TimezonePeriod.new(o1)

    assert_equal(true, p1 == p1)
    assert_equal(true, p1 == p2)
    assert_equal(false, p1 == p3)
    assert_equal(false, p1 == p4)
    assert_equal(false, p1 == Object.new)
  end

  def test_eql
    o1 = TimezoneOffset.new(0, 3600, 'TEST')
    o2 = TimezoneOffset.new(0, 0, 'TEST')

    p1 = OffsetTimezonePeriod.new(o1)
    p2 = OffsetTimezonePeriod.new(o1)
    p3 = OffsetTimezonePeriod.new(o2)
    p4 = TimezonePeriod.new(o1)

    assert_equal(true, p1.eql?(p1))
    assert_equal(true, p1.eql?(p2))
    assert_equal(false, p1.eql?(p3))
    assert_equal(false, p1.eql?(p4))
    assert_equal(false, p1.eql?(Object.new))
  end

  def test_hash
    o = TimezoneOffset.new(0, 3600, 'TEST')
    p = OffsetTimezonePeriod.new(o)
    assert_equal(o.hash, p.hash)
  end
end
