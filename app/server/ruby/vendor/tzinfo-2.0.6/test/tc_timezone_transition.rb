# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'
require 'date'

class TCTimezoneTransition < Minitest::Test
  include TZInfo

  def test_offset
    t = TimezoneTransition.new(TimezoneOffset.new(3600, 3600, 'TDT'),
      TimezoneOffset.new(3600, 0, 'TST'), 1148949080)

    assert_equal(TimezoneOffset.new(3600, 3600, 'TDT'), t.offset)
  end

  def test_previous_offset
    t = TimezoneTransition.new(TimezoneOffset.new(3600, 3600, 'TDT'),
      TimezoneOffset.new(3600, 0, 'TST'), 1148949080)

    assert_equal(TimezoneOffset.new(3600, 0, 'TST'), t.previous_offset)
  end

  def test_timestamp_value
    t = TimezoneTransition.new(TimezoneOffset.new(3600, 3600, 'TDT'),
      TimezoneOffset.new(3600, 0, 'TST'), 1148949080)

    assert_equal(1148949080, t.timestamp_value)
  end

  def test_at
    t = TimezoneTransition.new(TimezoneOffset.new(3600, 3600, 'TDT'),
      TimezoneOffset.new(3600, 0, 'TST'), 1148949080)

    assert_equal_with_offset(Timestamp.utc(1148949080), t.at)
  end

  def test_local_end_at
    t1 = TimezoneTransition.new(TimezoneOffset.new(3600, 3600, 'TDT'),
      TimezoneOffset.new(3600, 0, 'TST'), 1148949080)
    t2 = TimezoneTransition.new(TimezoneOffset.new(7200, 0, 'TST'),
      TimezoneOffset.new(7200, 3600, 'TDT'), 1148949080)
    t3 = TimezoneTransition.new(TimezoneOffset.new(-3600, 3600, 'TDT'),
      TimezoneOffset.new(-3600, 0, 'TST'), 1148949080)

    assert_equal_with_offset_and_timezone_offset(TimestampWithOffset.new(1148949080, 0,  3600).set_timezone_offset(t1.previous_offset), t1.local_end_at)
    assert_equal_with_offset_and_timezone_offset(TimestampWithOffset.new(1148949080, 0, 10800).set_timezone_offset(t2.previous_offset), t2.local_end_at)
    assert_equal_with_offset_and_timezone_offset(TimestampWithOffset.new(1148949080, 0, -3600).set_timezone_offset(t3.previous_offset), t3.local_end_at)
  end

  def test_local_start_at
    t1 = TimezoneTransition.new(TimezoneOffset.new(3600, 3600, 'TDT'),
      TimezoneOffset.new(3600, 0, 'TST'), 1148949080)
    t2 = TimezoneTransition.new(TimezoneOffset.new(7200, 0, 'TST'),
      TimezoneOffset.new(7200, 3600, 'TDT'), 1148949080)
    t3 = TimezoneTransition.new(TimezoneOffset.new(-3600, 3600, 'TDT'),
      TimezoneOffset.new(-3600, 0, 'TST'), 1148949080)

    assert_equal_with_offset_and_timezone_offset(TimestampWithOffset.new(1148949080, 0, 7200).set_timezone_offset(t1.offset), t1.local_start_at)
    assert_equal_with_offset_and_timezone_offset(TimestampWithOffset.new(1148949080, 0, 7200).set_timezone_offset(t2.offset), t2.local_start_at)
    assert_equal_with_offset_and_timezone_offset(TimestampWithOffset.new(1148949080, 0,    0).set_timezone_offset(t3.offset), t3.local_start_at)
  end

  def test_equality
    t1 = TimezoneTransition.new(TimezoneOffset.new(3600, 3600, 'TDT'),
      TimezoneOffset.new(3600, 0, 'TST'), 1148949080)
    t2 = TimezoneTransition.new(TimezoneOffset.new(3600, 3600, 'TDT'),
      TimezoneOffset.new(3600, 0, 'TST'), 1148949080)
    t3 = TimezoneTransition.new(TimezoneOffset.new(3600, 3600, 'TDT'),
      TimezoneOffset.new(3600, 0, 'TST'), 1148949081)
    t4 = TimezoneTransition.new(TimezoneOffset.new(3601, 3600, 'TDT'),
      TimezoneOffset.new(3600, 0, 'TST'), 1148949080)
    t5 = TimezoneTransition.new(TimezoneOffset.new(3600, 3600, 'TDT'),
      TimezoneOffset.new(3601, 0, 'TST'), 1148949080)

    assert_equal(true, t1 == t1)
    assert_equal(true, t1 == t2)
    assert_equal(false, t1 == t3)
    assert_equal(false, t1 == t4)
    assert_equal(false, t1 == t5)
    assert_equal(false, t1 == Object.new)

    assert_equal(true, t1.eql?(t1))
    assert_equal(true, t1.eql?(t2))
    assert_equal(false, t1.eql?(t3))
    assert_equal(false, t1.eql?(t4))
    assert_equal(false, t1.eql?(t5))
    assert_equal(false, t1.eql?(Object.new))
  end

  def test_hash
    t = TimezoneTransition.new(TimezoneOffset.new(3600, 3600, 'TDTA'),
      TimezoneOffset.new(3600, 0, 'TSTA'), 1148949080)

    assert_equal([TimezoneOffset.new(3600, 3600, 'TDTA'),
      TimezoneOffset.new(3600, 0, 'TSTA'), 1148949080].hash,
      t.hash)
  end
end
