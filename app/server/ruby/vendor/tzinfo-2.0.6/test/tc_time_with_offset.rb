# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCTimeWithOffset < Minitest::Test
  include TZInfo

  def time_with_offset(year, month, day, hour, minute, second, tz_offset)
    TimeWithOffset.new(year, month, day, hour, minute, second, tz_offset.observed_utc_offset).set_timezone_offset(tz_offset)
  end

  def assert_is_time_or_time_with_offset_with_nil_timezone_offset(value)
    if value.kind_of?(TimeWithOffset)
      assert_nil(value.timezone_offset)
    else
      assert_equal(Time, value.class)
    end
  end

  def test_set_timezone_offset_unchanged_offset
    [TimeWithOffset.new(2017,1,15,23,0,Rational(11,10),0), TimeWithOffset.new(2017,1,15,23,0,Rational(11,10),3600)].each do |two|
      o1 = TimezoneOffset.new(two.utc_offset, 0, 'TEST')
      o2 = TimezoneOffset.new(0, two.utc_offset, 'TEST')
      assert_nil(two.timezone_offset)
      assert_same(two, two.set_timezone_offset(o1))
      assert_same(o1, two.timezone_offset)
      assert_same(two, two.set_timezone_offset(o2))
      assert_same(o2, two.timezone_offset)
    end
  end

  def test_set_timezone_offset_set_offset
    o = TimezoneOffset.new(3600, 0, 'TEST')

    [TimeWithOffset.utc(2017,1,15,23,0,Rational(11,10)), TimeWithOffset.new(2017,1,15,23,0,Rational(11,10),0), TimeWithOffset.new(2017,1,15,22,0,Rational(11,10),-3600)].each do |two|
      assert_nil(two.timezone_offset)
      assert_same(two, two.set_timezone_offset(o))
      assert_same(o, two.timezone_offset)
      assert_equal_with_offset(Time.new(2017,1,16,0,0,Rational(11,10),3600), two)
    end
  end

  def test_set_timezone_offset_utc
    two = TimeWithOffset.utc(2017,1,15,23,0,Rational(11,10))
    o = TimezoneOffset.new(0, 0, 'TEST')

    assert_equal(true, two.utc?)
    assert_same(two, two.set_timezone_offset(o))
    assert_same(o, two.timezone_offset)
    assert_equal_with_offset(Time.new(2017,1,15,23,0,Rational(11,10),0), two)
  end

  def test_set_timezone_offset_nil_timezone_offset
    two = TimeWithOffset.new(2017,1,15,23,0,Rational(11,10),0)
    error = assert_raises(ArgumentError) { two.set_timezone_offset(nil) }
    assert_match(/\btimezone_offset\b/, error.message)
  end

  def test_strftime
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    assert_equal('23:00:01 TEST', two.strftime('%H:%M:%S %Z'))
    assert_equal('TEST', two.strftime('%Z'))
    assert_equal('%ZTEST', two.strftime('%%Z%Z'))
    assert_equal('TEST TEST', two.strftime('%Z %Z'))
    assert_equal('TEST %Z %TEST %%Z %%TEST', two.strftime('%Z %%Z %%%Z %%%%Z %%%%%Z'))
  end

  def test_strftime_handles_percent_in_abbreviation
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, '%H:%M:%S'))
    assert_equal('%H:%M:%S', two.strftime('%Z'))
  end

  def test_strftime_nil_timezone_offset
    # Will use Time#strftime's handling of the %Z directive.
    t = Time.new(2017,1,15,23,0,1,3600)
    two = TimeWithOffset.new(2017,1,15,23,0,1,3600)
    assert_nil(two.timezone_offset)

    # JRuby 1.7 returns '+01:00' instead of empty string.
    assert_equal(t.strftime('%Z'), two.strftime('%Z'))
  end

  def test_strftime_nil_format
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    error = assert_raises(ArgumentError) { two.strftime(nil) }
    assert_match(/\bformat\b/, error.message)
  end

  def test_add
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))

    # MRI returns Time. JRuby returns TimeWithOffset.
    assert_is_time_or_time_with_offset_with_nil_timezone_offset(two + 1)
  end

  def test_subtract_seconds
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))

    # MRI returns Time. JRuby returns TimeWithOffset.
    assert_is_time_or_time_with_offset_with_nil_timezone_offset(two - 1)
  end

  def test_subtract_time
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    assert_equal(1, two - Time.utc(2017,1,15,22,0,0))
  end

  def test_subtract_time_with_offset
    two1 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    two2 = time_with_offset(2017,1,15,23,0,0,TimezoneOffset.new(0, 3600, 'TEST'))
    assert_equal(1, two1 - two2)
  end

  def test_compare
    o = TimezoneOffset.new(0, 0, 'TEST')
    t1 = Time.new(2017,1,15,23,0,1,0)
    t2 = Time.new(2017,1,15,23,0,2,0)
    two1 = time_with_offset(2017,1,15,23,0,1,o)
    two2 = time_with_offset(2017,1,15,23,0,2,o)

    assert_equal(0, two1 <=> two1)
    assert_equal(0, two1 <=> t1)
    assert_equal(0, t1 <=> two1)

    assert_equal(-1, two1 <=> two2)
    assert_equal(-1, two1 <=> t2)
    assert_equal(-1, t1 <=> two2)

    assert_equal(1, two2 <=> two1)
    assert_equal(1, two2 <=> t1)
    assert_equal(1, t2 <=> two1)
  end

  def test_dst
    two1 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(   0,    0, 'TEST'))
    two2 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(   0, 3600, 'TEST'))
    two3 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(3600,    0, 'TEST'))
    two4 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(3600, 3600, 'TEST'))

    assert_equal(false, two1.dst?)
    assert_equal(true,  two2.dst?)
    assert_equal(false, two3.dst?)
    assert_equal(true,  two4.dst?)
  end

  def test_dst_nil_timezone_offset
    two = TimeWithOffset.new(2017,1,15,23,0,1,3600)
    assert_nil(two.timezone_offset)
    assert_equal(false, two.dst?)
  end

  def test_eql
    o = TimezoneOffset.new(0, 0, 'TEST')
    t1 = Time.new(2017,1,15,23,0,1,0)
    t2 = Time.new(2017,1,15,23,0,2,0)
    two1 = time_with_offset(2017,1,15,23,0,1,o)
    two2 = time_with_offset(2017,1,15,23,0,2,o)

    assert_equal(true, two1.eql?(two1))
    assert_equal(true, two1.eql?(t1))
    assert_equal(true, t1.eql?(two1))

    assert_equal(false, two1.eql?(two2))
    assert_equal(false, two1.eql?(t2))
    assert_equal(false, t1.eql?(two2))
  end

  def test_getgm
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))

    # MRI returns TimeWithOffset. JRuby returns Time.
    assert_is_time_or_time_with_offset_with_nil_timezone_offset(two.getgm)
  end

  def test_getlocal
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))

    # MRI returns TimeWithOffset. JRuby returns Time.
    assert_is_time_or_time_with_offset_with_nil_timezone_offset(two.getlocal)
  end

  def test_getlocal_0
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))

    # MRI and JRuby >= 9.3 returns TimeWithOffset. JRuby < 9.3 returns Time.
    local = two.getlocal(0)
    assert_equal(0, local.utc_offset)
    assert_is_time_or_time_with_offset_with_nil_timezone_offset(two.getlocal(0))
  end

  def test_getutc
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))

    # MRI returns TimeWithOffset. JRuby returns Time.
    assert_is_time_or_time_with_offset_with_nil_timezone_offset(two.getutc)
  end

  def test_gmtime
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(3600, 0, 'TEST'))
    assert_same(two, two.gmtime)
    assert_equal(true, two.utc?)
    assert_nil(two.timezone_offset)
  end

  def test_isdst
    two1 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(   0,    0, 'TEST'))
    two2 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(   0, 3600, 'TEST'))
    two3 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(3600,    0, 'TEST'))
    two4 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(3600, 3600, 'TEST'))

    assert_equal(false, two1.isdst)
    assert_equal(true,  two2.isdst)
    assert_equal(false, two3.isdst)
    assert_equal(true,  two4.isdst)
  end

  def test_isdst_nil_timezone_offset
    two = TimeWithOffset.new(2017,1,15,23,0,1,3600)
    assert_nil(two.timezone_offset)
    assert_equal(false, two.isdst)
  end

  def test_localtime_without_offset
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    assert_same(two, two.localtime)
    assert_nil(two.timezone_offset)
  end

  def test_localtime_with_offset
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    assert_same(two, two.localtime(3600))
    assert_equal(3600, two.utc_offset)
    assert_nil(two.timezone_offset)
  end

  def test_localtime_with_offset_unchanged
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(3600, 0, 'TEST'))
    assert_same(two, two.localtime(3600))
    assert_equal(3600, two.utc_offset)
    assert_nil(two.timezone_offset)
  end

  def test_round
    o = TimezoneOffset.new(3600, 0, 'TEST')
    two = time_with_offset(2017,1,15,23,0,Rational(111,100),o)

    r = two.round
    r0 = two.round(0)
    r1 = two.round(1)

    [r, r0, r1].each do |result|
      assert_kind_of(TimeWithOffset, result)
      assert_same(o, result.timezone_offset)
    end

    assert_equal_with_offset(Time.new(2017,1,15,23,0,1,3600), r)
    assert_equal_with_offset(Time.new(2017,1,15,23,0,1,3600), r0)
    assert_equal_with_offset(Time.new(2017,1,15,23,0,Rational(11,10),3600), r1)
  end

  def test_round_nil_timezone_offset
    two = TimeWithOffset.new(2017,1,15,23,0,Rational(111,100),3600)
    assert_nil(two.timezone_offset)

    r = two.round
    r0 = two.round(0)
    r1 = two.round(1)

    [r, r0, r1].each { |result| assert_equal(Time, result.class) }

    assert_equal_with_offset(Time.new(2017,1,15,23,0,1,3600), r)
    assert_equal_with_offset(Time.new(2017,1,15,23,0,1,3600), r0)
    assert_equal_with_offset(Time.new(2017,1,15,23,0,Rational(11,10),3600), r1)
  end

  def test_succ
    skip('Time#succ is not supported') unless Time.new.respond_to?(:succ)

    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'BST'))

    # succ is obsolete and outputs a warning.
    without_warnings do
      assert_is_time_or_time_with_offset_with_nil_timezone_offset(two.succ)
    end
  end

  def test_to_a
    two1 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(   0,    0, 'TEST'))
    two2 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(   0, 3600, 'TEST'))
    two3 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(3600,    0, 'TEST'))
    two4 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(3600, 3600, 'TEST'))

    assert_equal([1,0,23,15,1,2017,0,15,false,'TEST'], two1.to_a)
    assert_equal([1,0,23,15,1,2017,0,15,true, 'TEST'], two2.to_a)
    assert_equal([1,0,23,15,1,2017,0,15,false,'TEST'], two3.to_a)
    assert_equal([1,0,23,15,1,2017,0,15,true, 'TEST'], two4.to_a)
  end

  def test_to_a_nil_timezone_offset
    t = Time.new(2017,1,15,23,0,1,3600)
    two = TimeWithOffset.new(2017,1,15,23,0,1,3600)
    assert_nil(two.timezone_offset)

    # For local times, the last element is nil on MRI and empty string on JRuby.
    # Compare with the actual Time#to_a output.
    assert_equal(t.to_a, two.to_a)
  end

  def test_utc
    two = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(3600, 0, 'TEST'))
    assert_same(two, two.utc)
    assert_equal(true, two.utc?)
    assert_nil(two.timezone_offset)
  end

  def test_zone
    two1 = time_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(3600, 0, 'TEST'))
    two2 = TimeWithOffset.utc(2017,1,15,23,0,1).set_timezone_offset(TimezoneOffset.new(0, 0, 'GMT'))
    assert_equal('TEST', two1.zone)
    assert_equal('GMT', two2.zone)
  end

  def test_zone_nil_timezone_offset
    t = Time.new(2017,1,15,23,0,1,3600)
    two = TimeWithOffset.new(2017,1,15,23,0,1,3600)
    assert_nil(two.timezone_offset)

    # For local times, zone returns nil on MRI and empty string on JRuby.
    # Compare with the actual Time#zone output.
    assert_nil_or_equal(t.zone, two.zone)
  end

  def test_to_datetime
    o = TimezoneOffset.new(3600, 0, 'TEST')
    two = time_with_offset(2017,1,15,23,0,Rational(11,10),o)
    d = two.to_datetime
    assert_kind_of(DateTimeWithOffset, d)
    assert_same(o, d.timezone_offset)
    assert_equal_with_offset(DateTime.new(2017,1,15,23,0,Rational(11,10),Rational(1,24)), d)
  end

  def test_to_datetime_nil_timezone_offset
    two = TimeWithOffset.new(2017,1,15,23,0,Rational(11,10),3600)
    assert_nil(two.timezone_offset)
    d = two.to_datetime
    assert_equal(DateTime, d.class)
    assert_equal_with_offset(DateTime.new(2017,1,15,23,0,Rational(11,10),Rational(1,24)), d)
  end

  def test_class_at_returns_local_time
    two = TimeWithOffset.at(1484521201)
    assert_kind_of(TimeWithOffset, two)
    assert_nil(two.timezone_offset)
    assert_equal(1484521201, two.to_i)
  end

  def test_class_new_returns_local_time
    two = TimeWithOffset.new(2017,1,15,23,0,1,0)
    assert_kind_of(TimeWithOffset, two)
    assert_nil(two.timezone_offset)
    assert_equal(1484521201, two.to_i)
  end

  def test_class_utc_returns_local_time
    two = TimeWithOffset.utc(2017,1,15,23,0,1)
    assert_kind_of(TimeWithOffset, two)
    assert_nil(two.timezone_offset)
    assert_equal(1484521201, two.to_i)
  end
end
