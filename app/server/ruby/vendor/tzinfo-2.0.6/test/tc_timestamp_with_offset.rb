# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCTimestampWithOffset < Minitest::Test
  include TZInfo

  def new_timestamp(year, month, day, hour, minute, second, offset, klass = Timestamp)
    time = Time.new(year, month, day, hour, minute, second, offset == :utc ? 0 : offset)
    klass.new(time.to_i, time.subsec, offset)
  end

  def timestamp_with_offset(year, month, day, hour, minute, second, tz_offset)
    new_timestamp(year, month, day, hour, minute, second, tz_offset.observed_utc_offset, TimestampWithOffset).set_timezone_offset(tz_offset)
  end

  def test_class_set_timezone_offset
    o = TimezoneOffset.new(7200, 0, 'TEST')
    [new_timestamp(2017,1,15,23,0,0,nil), new_timestamp(2017,1,15,23,0,0,:utc), new_timestamp(2017,1,15,23,0,0,0), new_timestamp(2017,1,15,23,0,Rational(11,10),0), new_timestamp(2017,1,15,23,0,Rational(11,10),3600)].each do |t|
      two = TimestampWithOffset.set_timezone_offset(t, o)
      assert_kind_of(TimestampWithOffset, two)
      assert_equal(t.value, two.value)
      assert_equal(t.sub_second, two.sub_second)
      assert_equal(7200, two.utc_offset)
      assert_same(o, two.timezone_offset)
    end
  end

  def test_class_set_timezone_offset_timestamp_nil
    o = TimezoneOffset.new(7200, 0, 'TEST')
    error = assert_raises(ArgumentError) { TimestampWithOffset.set_timezone_offset(nil, o) }
    assert_match(/\btimestamp\b/, error.message)
  end

  def test_class_set_timezone_offset_timezone_offset_nil
    t = new_timestamp(2017,1,15,23,0,0,0)
    error = assert_raises(ArgumentError) { TimestampWithOffset.set_timezone_offset(t, nil) }
    assert_match(/\btimezone_offset\b/, error.message)
  end

  def test_set_timezone_offset
    [new_timestamp(2017,1,15,23,0,Rational(11,10),0,TimestampWithOffset), new_timestamp(2017,1,15,23,0,Rational(11,10),3600,TimestampWithOffset)].each do |two|
      o1 = TimezoneOffset.new(two.utc_offset, 0, 'TEST')
      o2 = TimezoneOffset.new(0, two.utc_offset, 'TEST')
      assert_nil(two.timezone_offset)
      assert_same(two, two.set_timezone_offset(o1))
      assert_same(o1, two.timezone_offset)
      assert_same(two, two.set_timezone_offset(o2))
      assert_same(o2, two.timezone_offset)
    end
  end

  def test_set_timezone_offset_utc
    two = TimestampWithOffset.create(2017,1,15,23,0,1,Rational(1,10),:utc)
    o = TimezoneOffset.new(0, 0, 'TEST')
    error = assert_raises(ArgumentError) { two.set_timezone_offset(o) }
    assert_match(/\bmatch\b/, error.message)
  end

  def test_set_timezone_offset_unspecified_offset
    two = TimestampWithOffset.create(2017,1,15,23,0,1,Rational(1,10))
    o = TimezoneOffset.new(0, 0, 'TEST')
    error = assert_raises(ArgumentError) { two.set_timezone_offset(o) }
    assert_match(/\bmatch\b/, error.message)
  end

  def test_set_timezone_offset_offset_mismatch
    two1 = new_timestamp(2017,1,15,23,0,Rational(11,10),0,TimestampWithOffset)
    o1a = TimezoneOffset.new(3600,    0, 'TEST')
    o1b = TimezoneOffset.new(   0, 3600, 'TEST')

    two2 = new_timestamp(2017,1,15,23,0,Rational(11,10),3600,TimestampWithOffset)
    o2a = TimezoneOffset.new(3600, 3600, 'TEST')
    o2b = TimezoneOffset.new(   0,    0, 'TEST')

    [[two1, [o1a, o1b]], [two2, [o2a, o2b]]].each do |two, offsets|
      offsets.each do |o|
        error = assert_raises(ArgumentError) { two.set_timezone_offset(o) }
        assert_match(/\bmatch\b/, error.message)
      end
    end
  end

  def test_set_timezone_offset_nil_timezone_offset
    two = new_timestamp(2017,1,15,23,0,Rational(11,10),0,TimestampWithOffset)
    error = assert_raises(ArgumentError) { two.set_timezone_offset(nil) }
    assert_match(/\btimezone_offset\b/, error.message)
  end

  def test_strftime
    two = timestamp_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    assert_equal('23:00:01 TEST', two.strftime('%H:%M:%S %Z'))
    assert_equal('TEST', two.strftime('%Z'))
    assert_equal('%ZTEST', two.strftime('%%Z%Z'))
    assert_equal('TEST TEST', two.strftime('%Z %Z'))
    assert_equal('TEST %Z %TEST %%Z %%TEST', two.strftime('%Z %%Z %%%Z %%%%Z %%%%%Z'))
  end

  def test_strftime_handles_percent_in_abbreviation
    two = timestamp_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, '%H:%M:%S'))
    assert_equal('%H:%M:%S', two.strftime('%Z'))
  end

  def test_strftime_nil_timezone_offset
    t = new_timestamp(2017,1,15,23,0,1,3600)
    two = new_timestamp(2017,1,15,23,0,1,3600,TimestampWithOffset)
    assert_nil(two.timezone_offset)

    # JRuby 1.7 returns '+01:00' instead of empty string.
    assert_equal(t.strftime('%Z'), two.strftime('%Z'))
  end

  def test_strftime_nil_format
    two = timestamp_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    error = assert_raises(ArgumentError) { two.strftime(nil) }
    assert_match(/\bformat\b/, error.message)
  end

  def test_utc
    two = timestamp_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    assert_equal(Timestamp, two.utc.class)
  end

  def test_to_time
    o = TimezoneOffset.new(0, 3600, 'TEST')
    two = timestamp_with_offset(2017,1,15,23,0,Rational(11,10),o)
    t = two.to_time
    assert_kind_of(TimeWithOffset, t)
    assert_same(o, t.timezone_offset)
    assert_equal_with_offset(Time.new(2017,1,15,23,0,Rational(11,10),3600), t)
  end

  def test_to_time_nil_timezone_offset
    two = new_timestamp(2017,1,15,23,0,Rational(11,10),3600,TimestampWithOffset)
    assert_nil(two.timezone_offset)
    t = two.to_time
    assert_equal(Time, t.class)
    assert_equal_with_offset(Time.new(2017,1,15,23,0,Rational(11,10),3600), t)
  end

  def test_to_datetime
    o = TimezoneOffset.new(0, 3600, 'TEST')
    two = timestamp_with_offset(2017,1,15,23,0,Rational(11,10),o)
    dt = two.to_datetime
    assert_kind_of(DateTimeWithOffset, dt)
    assert_same(o, dt.timezone_offset)
    assert_equal_with_offset(DateTime.new(2017,1,15,23,0,Rational(11,10),Rational(1,24)), dt)
  end

  def test_to_datetime_nil_timezone_offset
    two = new_timestamp(2017,1,15,23,0,Rational(11,10),3600,TimestampWithOffset)
    assert_nil(two.timezone_offset)
    dt = two.to_datetime
    assert_equal(DateTime, dt.class)
    assert_equal_with_offset(DateTime.new(2017,1,15,23,0,Rational(11,10),Rational(1,24)), dt)
  end

  def test_compare
    o = TimezoneOffset.new(0, 0, 'TEST')
    t1 = new_timestamp(2017,1,15,23,0,1,0)
    t2 = new_timestamp(2017,1,15,23,0,2,0)
    two1 = timestamp_with_offset(2017,1,15,23,0,1,o)
    two2 = timestamp_with_offset(2017,1,15,23,0,2,o)

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

  def test_inspect
    assert_equal('#<TZInfo::TimestampWithOffset: @value=1476316800, @sub_second=0, @utc_offset=0, @utc=false>', timestamp_with_offset(2016,10,13,0,0,0,TimezoneOffset.new(0, 0, 'TEST')).inspect)
    assert_equal('#<TZInfo::TimestampWithOffset: @value=1476316800, @sub_second=1/10, @utc_offset=3600, @utc=false>', timestamp_with_offset(2016,10,13,1,0,Rational(1,10),TimezoneOffset.new(3600, 0, 'TEST')).inspect)
    assert_equal('#<TZInfo::TimestampWithOffset: @value=1476316800, @sub_second=0, @utc_offset=0, @utc=true>', TimestampWithOffset.new(1476316800, Rational(0, 1), :utc).inspect)
  end

  def test_class_create_returns_local_timestamp
    two = TimestampWithOffset.create(2016, 10, 13, 1, 0, 0, 0, 3600)
    assert_kind_of(TimestampWithOffset, two)
    assert_nil(two.timezone_offset)
    assert_equal(1476316800, two.value)
    assert_equal(0, two.sub_second)
    assert_equal(3600, two.utc_offset)
  end

  def test_class_new_returns_local_timestamp
    two = TimestampWithOffset.new(1476316800, 0, 3600)
    assert_kind_of(TimestampWithOffset, two)
    assert_nil(two.timezone_offset)
    assert_equal(1476316800, two.value)
    assert_equal(0, two.sub_second)
    assert_equal(3600, two.utc_offset)
  end
end
