# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCTimezoneProxy < Minitest::Test
  include TZInfo

  def assert_raises_invalid_timezone_identifier(identifier)
    error = assert_raises(InvalidTimezoneIdentifier) { yield }
    assert_match(Regexp.new('\b' + Regexp.escape(identifier) + '\b'), error.message)
  end

  test_encodings('ISO-8859-1', 'UTF-8', 'UTF-16').each do |encoding|
    define_method("test_not_exist_with_#{encoding.to_method}_encoded_identifier") do
      identifier = 'Nothing/Special'.encode(encoding.name)
      proxy = TimezoneProxy.new(identifier)
      t = Time.utc(2006,1,1,0,0,0)
      assert_same(identifier, proxy.identifier)
      assert_same(identifier, proxy.name)
      assert_equal('Nothing - Special', proxy.friendly_identifier)
      assert_equal('Nothing - Special', proxy.to_s)

      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.canonical_identifier }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.canonical_zone }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.current_period }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.current_period_and_time }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.current_time_and_period }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.local_to_utc(t) }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.now }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.offsets_up_to(t) }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.period_for(t) }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.period_for_local(t) }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.period_for_utc(t) }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.periods_for_local(t) }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.strftime('%Z', t) }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.to_local(t) }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.transitions_up_to(t) }
      assert_raises_invalid_timezone_identifier('Nothing/Special') { proxy.utc_to_local(t) }
    end

    define_method("test_valid_with_#{encoding.to_method}_encoded_identifier") do
      proxy = TimezoneProxy.new('Europe/London'.encode(encoding.name))
      real = Timezone.get('Europe/London')

      t1 = Time.utc(2005,8,1,0,0,0)
      t2 = Time.utc(2004,8,1,0,0,0)

      assert_equal(real.canonical_identifier, proxy.canonical_identifier)
      assert_equal(real.canonical_zone, proxy.canonical_zone)
      assert_nothing_raised { proxy.current_period }
      assert_nothing_raised { proxy.current_period_and_time }
      assert_nothing_raised { proxy.current_time_and_period }
      assert_equal(real.friendly_identifier(true), proxy.friendly_identifier(true))
      assert_equal(real.friendly_identifier(false), proxy.friendly_identifier(false))
      assert_equal(real.friendly_identifier, proxy.friendly_identifier)
      assert_equal(real.identifier, proxy.identifier)
      assert_equal_with_offset(real.local_to_utc(t1), proxy.local_to_utc(t1))
      assert_equal(real.name, proxy.name)
      assert_nothing_raised { proxy.now }
      assert_equal(real.offsets_up_to(t1), proxy.offsets_up_to(t1))
      assert_equal(real.offsets_up_to(t1, t2), proxy.offsets_up_to(t1, t2))
      assert_equal(real.period_for(t1), proxy.period_for(t1))
      assert_equal(real.period_for_local(t1), proxy.period_for_local(t1))
      assert_equal(real.period_for_utc(t1), proxy.period_for_utc(t1))
      assert_equal(real.periods_for_local(t1), proxy.periods_for_local(t1))
      assert_equal(real.strftime('%Z', t1), proxy.strftime('%Z', t1))
      assert_equal_with_offset(real.to_local(t1), proxy.to_local(t1))
      assert_equal(real.to_s, proxy.to_s)
      assert_equal(real.transitions_up_to(t1), proxy.transitions_up_to(t1))
      assert_equal(real.transitions_up_to(t1, t2), proxy.transitions_up_to(t1, t2))
      assert_equal_with_offset(real.utc_to_local(t1), proxy.utc_to_local(t1))


      assert(real == proxy)
      assert(proxy == real)
      assert_equal(0, real <=> proxy)
      assert_equal(0, proxy <=> real)
    end
  end

  def test_canonical_linked
    # Test that the implementation of canonical_zone and canonical_identifier
    # are actually calling the real timezone and not just returning it and
    # its identifier.

    real = Timezone.get('UTC')
    proxy = TimezoneProxy.new('UTC')

    assert_kind_of(DataTimezone, proxy.canonical_zone)

    # ZoneinfoDataSource doesn't return LinkedTimezoneInfo instances for any
    # timezone.
    if real.kind_of?(LinkedTimezone)
      assert_equal('Etc/UTC', proxy.canonical_identifier)
      assert_equal('Etc/UTC', proxy.canonical_zone.identifier)
    else
      if DataSource.get.kind_of?(DataSources::RubyDataSource)
        # Not got a LinkedTimezone despite using a DataSource that supports it.
        # Raise an exception as this shouldn't happen.
        raise 'Non-LinkedTimezone instance returned for UTC using RubyDataSource'
      end

      assert_equal('UTC', proxy.canonical_identifier)
      assert_equal('UTC', proxy.canonical_zone.identifier)
    end
  end

  def test_after_freeze
    proxy = TimezoneProxy.new('Europe/London')
    real = Timezone.get('Europe/London')
    t = Time.utc(2017, 6, 1)
    proxy.freeze
    assert_equal('Europe/London', proxy.identifier)
    assert_equal(real.utc_to_local(t), proxy.utc_to_local(t))
  end

  def test_equals
    assert_equal(true, TimezoneProxy.new('Europe/London') == TimezoneProxy.new('Europe/London'))
    assert_equal(false, TimezoneProxy.new('Europe/London') == TimezoneProxy.new('Europe/Paris'))
    assert(!(TimezoneProxy.new('Europe/London') == Object.new))
  end

  def test_compare
    assert_equal(0, TimezoneProxy.new('Europe/London') <=> TimezoneProxy.new('Europe/London'))
    assert_equal(0, Timezone.get('Europe/London') <=> TimezoneProxy.new('Europe/London'))
    assert_equal(0, TimezoneProxy.new('Europe/London') <=> Timezone.get('Europe/London'))
    assert_equal(-1, TimezoneProxy.new('Europe/London') <=> TimezoneProxy.new('Europe/Paris'))
    assert_equal(-1, Timezone.get('Europe/London') <=> TimezoneProxy.new('Europe/Paris'))
    assert_equal(-1, TimezoneProxy.new('Europe/London') <=> Timezone.get('Europe/Paris'))
    assert_equal(1, TimezoneProxy.new('Europe/Paris') <=> TimezoneProxy.new('Europe/London'))
    assert_equal(1, Timezone.get('Europe/Paris') <=> TimezoneProxy.new('Europe/London'))
    assert_equal(1, TimezoneProxy.new('Europe/Paris') <=> Timezone.get('Europe/London'))
    assert_equal(-1, TimezoneProxy.new('America/New_York') <=> TimezoneProxy.new('Europe/Paris'))
    assert_equal(-1, Timezone.get('America/New_York') <=> TimezoneProxy.new('Europe/Paris'))
    assert_equal(-1, TimezoneProxy.new('America/New_York') <=> Timezone.get('Europe/Paris'))
    assert_equal(1, TimezoneProxy.new('Europe/Paris') <=> TimezoneProxy.new('America/New_York'))
    assert_equal(1, Timezone.get('Europe/Paris') <=> TimezoneProxy.new('America/New_York'))
    assert_equal(1, TimezoneProxy.new('Europe/Paris') <=> Timezone.get('America/New_York'))
  end

  def test_kind
    assert_kind_of(Timezone, TimezoneProxy.new('America/New_York'))
  end

  def test_marshal
    tp = TimezoneProxy.new('Europe/London')
    tp2 = Marshal.load(Marshal.dump(tp))

    assert_kind_of(TimezoneProxy, tp2)
    assert_equal('Europe/London', tp2.identifier)
  end

  def test_inspect
    tp = TimezoneProxy.new('Europe/London')
    assert_equal('#<TZInfo::TimezoneProxy: Europe/London>', tp.inspect)
  end
end
