# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCRubyTimeTimezone < Minitest::Test
  include TZInfo

  def test_new_time_with_time_zone_paris
    check_can_create_time_with_time_zone

    tz = Timezone.get('Europe/Paris')

    std_time = Time.new(2018, 12, 1, 0, 0, 0, tz)
    assert_same(tz, std_time.zone)
    assert_equal(3600, std_time.utc_offset)
    assert_equal(Time.utc(2018, 11, 30, 23, 0, 0), std_time.getutc)

    dst_time = Time.new(2018, 6, 1, 0, 0, 0, tz)
    assert_same(tz, dst_time.zone)
    assert_equal(7200, dst_time.utc_offset)
    assert_equal(Time.utc(2018, 5, 31, 22, 0, 0), dst_time.getutc)
  end

  def test_new_time_with_time_zone_new_york
    check_can_create_time_with_time_zone

    tz = Timezone.get('America/New_York')

    std_time = Time.new(2018, 12, 1, 0, 0, 0, tz)
    assert_same(tz, std_time.zone)
    assert_equal(-18000, std_time.utc_offset)
    assert_equal(Time.utc(2018, 12, 1, 5, 0, 0), std_time.getutc)

    dst_time = Time.new(2018, 6, 1, 0, 0, 0, tz)
    assert_same(tz, dst_time.zone)
    assert_equal(-14400, dst_time.utc_offset)
    assert_equal(Time.utc(2018, 6, 1, 4, 0, 0), dst_time.getutc)
  end

  def test_time_at_with_time_zone_paris
    check_can_create_time_with_time_zone

    tz = Timezone.get('Europe/Paris')

    std_time = Time.at(1543618800, in: tz)
    assert_same(tz, std_time.zone)
    assert_equal(3600, std_time.utc_offset)
    assert_equal(2018, std_time.year)
    assert_equal(12, std_time.month)
    assert_equal(1, std_time.day)
    assert_equal(0, std_time.hour)
    assert_equal(0, std_time.min)
    assert_equal(0, std_time.sec)

    dst_time = Time.at(1527804000, in: tz)
    assert_same(tz, dst_time.zone)
    assert_equal(7200, dst_time.utc_offset)
    assert_equal(2018, dst_time.year)
    assert_equal(6, dst_time.month)
    assert_equal(1, dst_time.day)
    assert_equal(0, dst_time.hour)
    assert_equal(0, dst_time.min)
    assert_equal(0, dst_time.sec)
  end

  def test_time_at_with_time_zone_new_york
    check_can_create_time_with_time_zone

    if RUBY_ENGINE == 'ruby' && RUBY_VERSION < '2.7' && [0].pack('J').bytesize <= 4
      skip('`Time.at(n, in: zone)` is broken with negative UTC offsets on 32-bit MRI Ruby 2.6')
    end

    tz = Timezone.get('America/New_York')

    std_time = Time.at(1543640400, in: tz)
    assert_same(tz, std_time.zone)
    assert_equal(-18000, std_time.utc_offset)
    assert_equal(2018, std_time.year)
    assert_equal(12, std_time.month)
    assert_equal(1, std_time.day)
    assert_equal(0, std_time.hour)
    assert_equal(0, std_time.min)
    assert_equal(0, std_time.sec)

    dst_time = Time.at(1527825600, in: tz)
    assert_same(tz, dst_time.zone)
    assert_equal(-14400, dst_time.utc_offset)
    assert_equal(2018, dst_time.year)
    assert_equal(6, dst_time.month)
    assert_equal(1, dst_time.day)
    assert_equal(0, dst_time.hour)
    assert_equal(0, dst_time.min)
    assert_equal(0, dst_time.sec)
  end

  private

  def check_can_create_time_with_time_zone
    unless RUBY_VERSION >= '2.6'
      skip("Cannot create Time with a time zone with Ruby #{RUBY_VERSION}")
    end
  end
end
