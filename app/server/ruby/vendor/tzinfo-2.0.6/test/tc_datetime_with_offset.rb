# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCDateTimeWithOffset < Minitest::Test
  include TZInfo

  def datetime_with_offset(year, month, day, hour, minute, second, tz_offset)
    DateTimeWithOffset.new(year, month, day, hour, minute, second, tz_offset.observed_utc_offset.to_r / 86400).set_timezone_offset(tz_offset)
  end

  def test_with_offset
    [DateTimeWithOffset.new(2017,1,15,23,0,Rational(11,10),0), DateTimeWithOffset.new(2017,1,15,23,0,Rational(11,10),Rational(1,24))].each do |dtwo|
      o1 = TimezoneOffset.new(dtwo.offset * 86400, 0, 'TEST')
      o2 = TimezoneOffset.new(0, dtwo.offset * 86400, 'TEST')
      assert_nil(dtwo.timezone_offset)
      assert_same(dtwo, dtwo.set_timezone_offset(o1))
      assert_same(o1, dtwo.timezone_offset)
      assert_same(dtwo, dtwo.set_timezone_offset(o2))
      assert_same(o2, dtwo.timezone_offset)
    end
  end

  def test_with_offset_offset_mismatch
    dtwo1 = DateTimeWithOffset.new(2017,1,15,23,0,Rational(11,10),0)
    o1a = TimezoneOffset.new(3600,    0, 'TEST')
    o1b = TimezoneOffset.new(   0, 3600, 'TEST')

    dtwo2 = DateTimeWithOffset.new(2017,1,15,23,0,Rational(11,10),Rational(1,24))
    o2a = TimezoneOffset.new(3600, 3600, 'TEST')
    o2b = TimezoneOffset.new(   0,    0, 'TEST')

    [[dtwo1, [o1a, o1b]], [dtwo2, [o2a, o2b]]].each do |dtwo, offsets|
      offsets.each do |o|
        error = assert_raises(ArgumentError) { dtwo.set_timezone_offset(o) }
        assert_match(/\bmatch\b/, error.message)
      end
    end
  end

  def test_with_offset_nil_timezone_offset
    lt = DateTimeWithOffset.new(2017,1,15,23,0,Rational(11,10),0)
    error = assert_raises(ArgumentError) { lt.set_timezone_offset(nil) }
    assert_match(/\btimezone_offset\b/, error.message)
  end

  def test_strftime
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    assert_equal('23:00:01 TEST', dtwo.strftime('%H:%M:%S %Z'))
    assert_equal('TEST', dtwo.strftime('%Z'))
    assert_equal('%ZTEST', dtwo.strftime('%%Z%Z'))
    assert_equal('TEST TEST', dtwo.strftime('%Z %Z'))
    assert_equal('TEST %Z %TEST %%Z %%TEST', dtwo.strftime('%Z %%Z %%%Z %%%%Z %%%%%Z'))
  end

  def test_strftime_handles_percent_in_abbreviation
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, '%H:%M:%S'))
    assert_equal('%H:%M:%S', dtwo.strftime('%Z'))
  end

  def test_strftime_nil_timezone_offset
    # Will use DateTime#strftime's handling of the %Z directive.
    dtwo = DateTimeWithOffset.new(2017,1,15,23,0,1,Rational(1,24))
    assert_nil(dtwo.timezone_offset)
    assert_equal('+01:00', dtwo.strftime('%Z'))
  end

  def test_strftime_nil_format
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    error = assert_raises(ArgumentError) { dtwo.strftime(nil) }
    assert_match(/\bformat\b/, error.message)
  end

  def test_to_date
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    d = dtwo.to_date
    assert_equal(Date, d.class)
    assert_equal(Date.new(2017,1,15), d)
  end

  def test_to_datetime
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    assert_same(dtwo, dtwo.to_datetime)
  end

  def test_to_time
    o = TimezoneOffset.new(3600, 0, 'TEST')
    dtwo = datetime_with_offset(2017,1,15,23,0,Rational(11,10),o)
    t = dtwo.to_time
    assert_kind_of(TimeWithOffset, t)
    assert_same(o, t.timezone_offset)
    assert_equal_with_offset(Time.new(2017,1,15,23,0,Rational(11,10),3600), t)
  end

  def test_to_time_nil_timezone_offset
    dtwo = DateTimeWithOffset.new(2017,1,15,23,0,Rational(11,10),Rational(1,24))
    assert_nil(dtwo.timezone_offset)
    t = dtwo.to_time
    assert_equal(Time, t.class)

    # Depending on the version of Ruby, this will return a Time instance using
    # the local system offset or the offset of the DateTime. Don't test the
    # offset.
    assert_equal(Time.new(2017,1,15,23,0,Rational(11,10),3600), t)
  end

  def test_add
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    assert_nil((dtwo + 1).timezone_offset)
  end

  def test_subtract_seconds
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    assert_nil((dtwo - 1).timezone_offset)
  end

  def test_subtract_datetime
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    assert_equal(dtwo - DateTime.new(2017,1,15,22,0,0), Rational(1, 86400))
  end

  def test_subtract_datetime_with_offset
    dtwo1 = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    dtwo2 = datetime_with_offset(2017,1,15,23,0,0,TimezoneOffset.new(0, 3600, 'TEST'))
    assert_equal(dtwo1 - dtwo2, Rational(1, 86400))
  end

  def test_add_months
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    assert_nil((dtwo >> 1).timezone_offset)
  end

  def test_subtract_months
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 3600, 'TEST'))
    assert_nil((dtwo << 1).timezone_offset)
  end

  def test_compare
    o = TimezoneOffset.new(0, 0, 'TEST')
    dt1 = DateTime.new(2017,1,15,23,0,1)
    dt2 = DateTime.new(2017,1,15,23,0,2)
    dtwo1 = datetime_with_offset(2017,1,15,23,0,1,o)
    dtwo2 = datetime_with_offset(2017,1,15,23,0,2,o)

    assert_equal(0, dtwo1 <=> dtwo1)
    assert_equal(0, dtwo1 <=> dt1)
    assert_equal(0, dt1 <=> dtwo1)

    assert_equal(-1, dtwo1 <=> dtwo2)
    assert_equal(-1, dtwo1 <=> dt2)
    assert_equal(-1, dt1 <=> dtwo2)

    assert_equal(1, dtwo2 <=> dtwo1)
    assert_equal(1, dtwo2 <=> dt1)
    assert_equal(1, dt2 <=> dtwo1)
  end

  def test_day_equals
    o = TimezoneOffset.new(0, 0, 'TEST')
    dt1 = DateTime.new(2017,1,15,23,0,1)
    dt2 = DateTime.new(2017,1,16,23,0,1)
    dtwo1 = datetime_with_offset(2017,1,15,23,0,1,o)
    dtwo2 = datetime_with_offset(2017,1,16,23,0,1,o)
    d1 = Date.new(2017,1,15)
    d2 = Date.new(2017,1,16)

    assert_equal(true, dtwo1 === dtwo1)
    assert_equal(true, dtwo1 === dt1)
    assert_equal(true, dtwo1 === d1)
    assert_equal(true, dt1 === dtwo1)
    assert_equal(true, d1 === dtwo1)

    assert_equal(false, dtwo1 === dtwo2)
    assert_equal(false, dtwo1 === dt2)
    assert_equal(false, dtwo1 === d2)
    assert_equal(false, dt2 === dtwo1)
    assert_equal(false, d2 === dtwo1)
  end

  def test_downto_block
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    block_call_count = 0

    result = dtwo.downto(Date.new(2017,1,14)) do |v|
      assert_nil(v.timezone_offset)
      block_call_count += 1
    end

    assert_same(dtwo, result)
    assert_equal(2, block_call_count)
  end

  def test_downto_enumerator
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    block_call_count = 0

    dtwo.downto(Date.new(2017,1,14)).each do |v|
      assert_nil(v.timezone_offset)
      block_call_count += 1
    end

    assert_equal(2, block_call_count)
  end

  def new_start_wrapper_test(method, start)
    o = TimezoneOffset.new(0, 0, 'TEST')
    dtwo = datetime_with_offset(2017,1,15,23,0,1,o)
    result = dtwo.public_send(method)
    assert_equal(start, result.start)
    assert_same(o, result.timezone_offset)
  end

  def test_england
    new_start_wrapper_test(:england, Date::ENGLAND)
  end

  def test_gregorian
    new_start_wrapper_test(:gregorian, Date::GREGORIAN)
  end

  def test_italy
    new_start_wrapper_test(:italy, Date::ITALY)
  end

  def test_julian
    new_start_wrapper_test(:julian, Date::JULIAN)
  end

  def test_new_start
    o = TimezoneOffset.new(0, 0, 'TEST')
    dtwo = datetime_with_offset(2017,1,15,23,0,1,o)
    result = dtwo.new_start(Date::ENGLAND)
    assert_equal(Date::ENGLAND, result.start)
    assert_same(o, result.timezone_offset)
  end

  def test_next
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    assert_nil(dtwo.next.timezone_offset)
  end

  def next_prev_test(type, unit)
    dt = DateTime.new(2017,1,15,23,0,1)
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    method = "#{type}_#{unit}"

    no_args = dtwo.public_send(method)
    assert_equal_with_offset(dt.public_send(method), no_args)
    assert_nil(no_args.timezone_offset)

    args = dtwo.public_send(method, 2)
    assert_equal_with_offset(dt.public_send(method, 2), args)
    assert_nil(args.timezone_offset)
  end

  def next_test(unit)
    next_prev_test(:next, unit)
  end

  def test_next_day
    next_test(:day)
  end

  def test_next_month
    next_test(:month)
  end

  def test_next_year
    next_test(:year)
  end

  def prev_test(unit)
    next_prev_test(:prev, unit)
  end

  def test_prev_day
    prev_test(:day)
  end

  def test_prev_month
    prev_test(:month)
  end

  def test_prev_year
    prev_test(:year)
  end

  def test_step_block
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    block_call_count = 0

    result = dtwo.step(Date.new(2017,1,17)) do |v|
      assert_nil(v.timezone_offset)
      block_call_count += 1
    end

    assert_same(dtwo, result)
    assert_equal(2, block_call_count)
  end

  def test_step_enumerator
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    block_call_count = 0

    dtwo.step(Date.new(2017,1,17)).each do |v|
      assert_nil(v.timezone_offset)
      block_call_count += 1
    end

    assert_equal(2, block_call_count)
  end

  def test_step_non_default_block
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    block_call_count = 0

    result = dtwo.step(Date.new(2017,1,14), -2) do |v|
      assert_nil(v.timezone_offset)
      block_call_count += 1
    end

    assert_same(dtwo, result)
    assert_equal(1, block_call_count)
  end

  def test_step_non_default_enumerator
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    block_call_count = 0

    dtwo.step(Date.new(2017,1,14), -2).each do |v|
      assert_nil(v.timezone_offset)
      block_call_count += 1
    end

    assert_equal(1, block_call_count)
  end

  def test_succ
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    assert_nil(dtwo.succ.timezone_offset)
  end

  def test_upto_block
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    block_call_count = 0

    result = dtwo.upto(Date.new(2017,1,17)) do |v|
      assert_nil(v.timezone_offset)
      block_call_count += 1
    end

    assert_same(dtwo, result)
    assert_equal(2, block_call_count)
  end

  def test_upto_enumerator
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))
    block_call_count = 0

    dtwo.upto(Date.new(2017,1,17)).each do |v|
      assert_nil(v.timezone_offset)
      block_call_count += 1
    end

    assert_equal(2, block_call_count)
  end

  def test_new_offset
    # new_offset isn't formally documented, but is referenced in a code sample
    # in the description of the DateTime class.
    dt = DateTime.new(2017,1,15,23,0,1)
    dtwo = datetime_with_offset(2017,1,15,23,0,1,TimezoneOffset.new(0, 0, 'TEST'))

    [0, Rational(1,24), '-01:00'].each do |o|
      result = dtwo.new_offset(o)
      assert_equal_with_offset(dt.new_offset(o), result)
      assert_nil(result.timezone_offset)
    end
  end

  def test_class_jd_returns_local_datetime
    ld = DateTimeWithOffset.jd(Rational(212351324401, 86400))
    assert_kind_of(DateTimeWithOffset, ld)
    assert_nil(ld.timezone_offset)
    assert_equal(2457769, ld.jd)
    assert_equal(Rational(82801, 86400), ld.day_fraction)
  end

  def test_class_new_returns_local_datetime
    ld = DateTimeWithOffset.new(2017,1,15,23,0,1)
    assert_kind_of(DateTimeWithOffset, ld)
    assert_nil(ld.timezone_offset)
    assert_equal(2457769, ld.jd)
    assert_equal(Rational(82801, 86400), ld.day_fraction)
  end
end
