# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCTimezone < Minitest::Test
  include TZInfo

  class << self
    include TZInfo

    private

    def time_with_unspecified_offset_test(method)
      define_method("test_#{method}_time_with_unspecified_offset") do
        tz = Timezone.get('Europe/London')

        time_types_test(:unspecified_offset) do |h|
          t = h.time(2006, 7, 15, 22, 12, 2, 0, nil)
          error = assert_raises(ArgumentError) { tz.public_send(method, t) }
          assert_equal('time must have a specified utc_offset', error.message)
        end
      end
    end

    def nil_time_test(method)
      define_method("test_#{method}_nil_time") do
        tz = Timezone.get('Europe/London')
        error = assert_raises(ArgumentError) { tz.public_send(method, nil) }
        assert_equal('time must be specified', error.message)
      end
    end
  end

  class BlockCalled < StandardError
  end

  class BaseTestTimezone < Timezone
    protected

    def times_equal(t1, t2)
      return false unless t1.class == t2.class

      return false unless t1 == t2

      if t1.respond_to?(:utc_offset)
        return false unless t1.utc_offset == t2.utc_offset
      elsif t1.respond_to?(:offset)
        return false unless t1.offset == t2.offset
      end

      if t1.respond_to?(:utc?)
        return false unless t1.utc? == t2.utc?
      end

      true
    end
  end

  class TestTimezone < BaseTestTimezone
    def initialize(identifier, period_for_result = nil, periods_for_local_result = nil, expected = nil)
      super()
      @identifier = identifier
      @period_for_result = period_for_result
      @periods_for_local_result = periods_for_local_result || []
      @expected = expected
    end

    def identifier
      @identifier
    end

    def period_for(time)
      raise "Unexpected time #{time} in period_for (expecting #{@expected})" unless times_equal(@expected, time)
      @period_for_result
    end

    def periods_for_local(local_time)
      raise "Unexpected local_time #{local_time} in periods_for_local (expecting #{@expected})" unless times_equal(@expected, local_time)
      @periods_for_local_result.clone
    end

    def transitions_up_to(utc_to, utc_from = nil)
      raise 'transitions_up_to called'
    end
  end

  class OffsetsUpToTestTimezone < BaseTestTimezone
    def initialize(identifier, expected_to, expected_from, transitions_up_to_result)
      super()
      @identifier = identifier
      @expected_to = expected_to
      @expected_from = expected_from
      @transitions_up_to_result = transitions_up_to_result
    end

    def identifier
      @identifier
    end

    def period_for(time)
      raise 'period_for called'
    end

    def periods_for_local(local_time)
      raise 'periods_for_local called'
    end

    def transitions_up_to(to, from = nil)
      raise ArgumentError, 'to must be specified' unless to
      raise "Unexpected to #{to || 'nil'} in transitions_up_to (expecting #{@expected_to})" unless times_equal(@expected_to, to)
      raise "Unexpected from #{from || 'nil'} in transitions_up_to (expecting #{@expected_from})" unless (!@expected_from && !from) || times_equal(@expected_from, from)

      raise ArgumentError, 'to must have a specified utc_offset' unless to.utc_offset

      if from
        raise ArgumentError, 'from must have a specified utc_offset' unless from.utc_offset
      end

      raise ArgumentError, 'to must be greater than from' if from && to <= from

      @transitions_up_to_result
    end
  end

  class OffsetsUpToNoTransitionsTestTimezone < BaseTestTimezone
    def initialize(identifier, expected_to, expected_from, period_for_result)
      super()
      @identifier = identifier
      @expected_to = expected_to
      @expected_from = expected_from
      @period_for_result = period_for_result
    end

    def identifier
      @identifier
    end

    def period_for(time)
      raise "Unexpected time #{time} in period_for (should be #{@expected_from})" if @expected_from && !times_equal(@expected_from, time)
      raise "Unexpected time #{time} in period_for (should be < #{@expected_to})" if !@expected_from && @expected_to <= time

      @period_for_result
    end

    def periods_for_local(local_time)
      raise 'periods_for_local called'
    end

    def transitions_up_to(to, from = nil)
      raise "Unexpected to #{to || 'nil'} in transitions_up_to (expecting #{@expected_to})" unless times_equal(@expected_to, to)
      raise "Unexpected from #{from || 'nil'} in transitions_up_to (expecting #{@expected_from})" unless (!@expected_from && !from) || times_equal(@expected_from, from)

      if from && to <= from
        raise ArgumentError, 'to must be greater than from'
      end

      []
    end
  end

  def setup
    @orig_default_dst = Timezone.default_dst
    @orig_data_source = DataSource.get
  end

  def teardown
    Timezone.default_dst = @orig_default_dst
    DataSource.set(@orig_data_source)
  end

  def test_default_dst_initial_value
    assert_nil(Timezone.default_dst)
  end

  def test_set_default_dst
    Timezone.default_dst = true
    assert_equal(true, Timezone.default_dst)
    Timezone.default_dst = false
    assert_equal(false, Timezone.default_dst)
    Timezone.default_dst = nil
    assert_nil(Timezone.default_dst)
    Timezone.default_dst = 0
    assert_equal(true, Timezone.default_dst)
  end

  test_encodings('ISO-8859-1', 'UTF-8', 'UTF-16').each do |encoding|
    define_method("test_get_valid_data_with_#{encoding.to_method}_encoded_identifier") do
      tz = Timezone.get('Europe/London'.encode(encoding.name))

      assert_kind_of(DataTimezone, tz)
      assert_equal('Europe/London', tz.identifier)
    end

    define_method("test_get_valid_linked_with_#{encoding.to_method}_encoded_identifier") do
      tz = Timezone.get('UTC'.encode(encoding.name))

      # ZoneinfoDataSource doesn't return DataSources::LinkedTimezoneInfo for any timezone.
      if DataSource.get.get_timezone_info('UTC').kind_of?(DataSources::LinkedTimezoneInfo)
        assert_kind_of(LinkedTimezone, tz)
      else
        assert_kind_of(DataTimezone, tz)
      end

      assert_equal('UTC', tz.identifier)
    end

    define_method("test_get_valid_three_levels_with_#{encoding.to_method}_encoded_identifier") do
      tz = Timezone.get('America/Argentina/Buenos_Aires'.encode(encoding.name))

      assert_kind_of(DataTimezone, tz)
      assert_equal('America/Argentina/Buenos_Aires', tz.identifier)
    end

    define_method("test_get_not_exist_with_#{encoding.to_method}_encoded_identifier") do
      error = assert_raises(InvalidTimezoneIdentifier) { Timezone.get('Nowhere/Special'.encode(encoding.name)) }
      assert_match(/\bNowhere\/Special/, error.message)
    end

    define_method("test_get_invalid_with_#{encoding.to_method}_encoded_identifier") do
      error = assert_raises(InvalidTimezoneIdentifier) { Timezone.get('../Definitions/UTC'.encode(encoding.name)) }
      assert_match(/\W\.\.\/Definitions\/UTC\b/, error.message)
    end

    define_method("test_get_case_with_#{encoding.to_method}_encoded_identifier") do
      Timezone.get('Europe/Prague')
      error = assert_raises(InvalidTimezoneIdentifier) { Timezone.get('Europe/prague'.encode(encoding.name)) }
      assert_match(/\bEurope\/prague\b/, error.message)
    end
  end

  def test_get_nil
    error = assert_raises(InvalidTimezoneIdentifier) { Timezone.get(nil) }
    assert_match(/\bnil\b/, error.message)
  end

  test_encodings('ISO-8859-1', 'UTF-8', 'UTF-16').each do |encoding|
    define_method("test_get_proxy_valid_with_#{encoding.to_method}_encoded_identifier") do
      identifier = 'Europe/London'.encode(encoding.name)
      proxy = Timezone.get_proxy(identifier)
      assert_kind_of(TimezoneProxy, proxy)
      assert_same(identifier, proxy.identifier)
    end

    define_method("test_get_proxy_not_exist_with_#{encoding.to_method}_encoded_identifier") do
      identifier = 'Not/There'.encode(encoding.name)
      proxy = Timezone.get_proxy(identifier)
      assert_kind_of(TimezoneProxy, proxy)
      assert_same(identifier, proxy.identifier)
    end

    define_method("test_get_proxy_invalid_with_#{encoding.to_method}_encoded_identifier") do
      identifier = '../Invalid/Identifier'.encode(encoding.name)
      proxy = Timezone.get_proxy(identifier)
      assert_kind_of(TimezoneProxy, proxy)
      assert_same(identifier, proxy.identifier)
    end
  end

  def test_get_tainted_loaded
    skip_if_taint_is_undefined_or_no_op
    Timezone.get('Europe/Andorra')

    safe_test(unavailable: :skip) do
      identifier = 'Europe/Andorra'.dup.taint
      assert(identifier.tainted?)
      tz = Timezone.get(identifier)
      assert_equal('Europe/Andorra', tz.identifier)
      assert(identifier.tainted?)
    end
  end

  def test_get_tainted_and_frozen_loaded
    skip_if_taint_is_undefined_or_no_op
    Timezone.get('Europe/Andorra')

    safe_test do
      tz = Timezone.get('Europe/Andorra'.dup.taint.freeze)
      assert_equal('Europe/Andorra', tz.identifier)
    end
  end

  def test_get_tainted_not_previously_loaded
    skip_if_taint_is_undefined_or_no_op

    safe_test(unavailable: :skip) do
      identifier = 'Europe/Andorra'.dup.taint
      assert(identifier.tainted?)
      tz = Timezone.get(identifier)
      assert_equal('Europe/Andorra', tz.identifier)
      assert(identifier.tainted?)
    end
  end

  def test_get_tainted_and_frozen_not_previously_loaded
    skip_if_taint_is_undefined_or_no_op

    safe_test do
      tz = Timezone.get('Europe/Amsterdam'.dup.taint.freeze)
      assert_equal('Europe/Amsterdam', tz.identifier)
    end
  end

  def test_new_no_args
    tz = Timezone.new

    assert_raises_unknown_timezone { tz.identifier }
    assert_raises_unknown_timezone { tz.friendly_identifier }
    assert_raises_unknown_timezone { tz.now }
    assert_raises_unknown_timezone { tz.current_period_and_time }
    assert_raises_unknown_timezone { tz.canonical_identifier }
    assert_raises_unknown_timezone { tz.canonical_zone }

    time_types_test do |h|
      time = h.time(2006,1,1,1,0,0)
      assert_raises_unknown_timezone { tz.utc_to_local(time) }
      assert_raises_unknown_timezone { tz.to_local(time) }
      assert_raises_unknown_timezone { tz.local_to_utc(time) }
      assert_raises_unknown_timezone { tz.period_for(time) }
      assert_raises_unknown_timezone { tz.period_for_utc(time) }
      assert_raises_unknown_timezone { tz.periods_for_local(time) }
      assert_raises_unknown_timezone { tz.period_for_local(time) }
      assert_raises_unknown_timezone { tz.transitions_up_to(time) }
      assert_raises_unknown_timezone { tz.offsets_up_to(time) }
    end
  end

  def test_all
    all = Timezone.all
    expected = DataSource.get.timezone_identifiers.collect {|identifier| Timezone.get_proxy(identifier)}
    assert_equal(expected, all)
  end

  def test_all_identifiers
    all = Timezone.all_identifiers
    assert_equal(DataSource.get.timezone_identifiers, all)
  end

  def test_all_data_zones
    all_data = Timezone.all_data_zones
    expected = DataSource.get.data_timezone_identifiers.collect {|identifier| Timezone.get_proxy(identifier)}
    assert_equal(expected, all_data)
  end

  def test_all_data_zone_identifiers
    all_data = Timezone.all_data_zone_identifiers
    assert_equal(DataSource.get.data_timezone_identifiers, all_data)
  end

  def test_all_linked_zones
    all_linked = Timezone.all_linked_zones
    expected = DataSource.get.linked_timezone_identifiers.collect {|identifier| Timezone.get_proxy(identifier)}
    assert_equal(expected, all_linked)
  end

  def test_all_linked_zone_identifiers
    all_linked = Timezone.all_linked_zone_identifiers
    assert_equal(DataSource.get.linked_timezone_identifiers, all_linked)
  end

  def test_all_country_zones
    # Probably should relax this test - just need all the zones, don't care
    # about order.
    expected = Country.all.inject([]) do |result,country|
      result += country.zones
    end
    expected.uniq!

    all_country_zones = Timezone.all_country_zones
    assert_equal(expected, all_country_zones)

    all_country_zone_identifiers = Timezone.all_country_zone_identifiers
    assert_equal(all_country_zone_identifiers.length, all_country_zones.length)

    all_country_zones.each do |zone|
      assert_kind_of(TimezoneProxy, zone)
      assert(all_country_zone_identifiers.include?(zone.identifier))
    end
  end

  def test_all_country_zone_identifiers
    # Probably should relax this test - just need all the zones, don't care
    # about order.
    expected = Country.all.inject([]) do |result,country|
      result += country.zone_identifiers
    end
    expected.uniq!

    assert_equal(expected, Timezone.all_country_zone_identifiers)
  end

  def test_identifier
    assert_raises_unknown_timezone { Timezone.new.identifier }
    assert_equal('Europe/Paris', TestTimezone.new('Europe/Paris').identifier)
  end

  def test_name
    assert_raises_unknown_timezone { Timezone.new.name }
    assert_equal('Europe/Paris', TestTimezone.new('Europe/Paris').name)
  end

  def test_friendly_identifier
    assert_equal('Paris', TestTimezone.new('Europe/Paris').friendly_identifier(true))
    assert_equal('Europe - Paris', TestTimezone.new('Europe/Paris').friendly_identifier(false))
    assert_equal('Europe - Paris', TestTimezone.new('Europe/Paris').friendly_identifier)
    assert_equal('Knox, Indiana', TestTimezone.new('America/Indiana/Knox').friendly_identifier(true))
    assert_equal('America - Knox, Indiana', TestTimezone.new('America/Indiana/Knox').friendly_identifier(false))
    assert_equal('America - Knox, Indiana', TestTimezone.new('America/Indiana/Knox').friendly_identifier)
    assert_equal('Dumont D\'Urville', TestTimezone.new('Antarctica/DumontDUrville').friendly_identifier(true))
    assert_equal('Antarctica - Dumont D\'Urville', TestTimezone.new('Antarctica/DumontDUrville').friendly_identifier(false))
    assert_equal('Antarctica - Dumont D\'Urville', TestTimezone.new('Antarctica/DumontDUrville').friendly_identifier)
    assert_equal('McMurdo', TestTimezone.new('Antarctica/McMurdo').friendly_identifier(true))
    assert_equal('Antarctica - McMurdo', TestTimezone.new('Antarctica/McMurdo').friendly_identifier(false))
    assert_equal('Antarctica - McMurdo', TestTimezone.new('Antarctica/McMurdo').friendly_identifier)
    assert_equal('GMT+1', TestTimezone.new('Etc/GMT+1').friendly_identifier(true))
    assert_equal('Etc - GMT+1', TestTimezone.new('Etc/GMT+1').friendly_identifier(false))
    assert_equal('Etc - GMT+1', TestTimezone.new('Etc/GMT+1').friendly_identifier)
    assert_equal('UTC', TestTimezone.new('UTC').friendly_identifier(true))
    assert_equal('UTC', TestTimezone.new('UTC').friendly_identifier(false))
    assert_equal('UTC', TestTimezone.new('UTC').friendly_identifier)
    assert_equal('', TestTimezone.new('').friendly_identifier(true))
    assert_equal('', TestTimezone.new('').friendly_identifier(false))
    assert_equal('', TestTimezone.new('').friendly_identifier)
  end

  test_encodings('ISO-8859-1', 'UTF-8', 'UTF-16').each do |encoding|
    define_method("test_friendly_identifier_with_#{encoding.to_method}_encoded_identifier") do
      tz = TestTimezone.new('Europe/Paris'.encode(encoding.name).freeze)
      assert_equal('Paris', tz.friendly_identifier(true))
      assert_equal('Europe - Paris', tz.friendly_identifier(false))
    end
  end

  def test_friendly_identifier_non_binary_encoding
    refute_equal(Encoding::ASCII_8BIT, TestTimezone.new('Europe/Paris').friendly_identifier(true).encoding)
    refute_equal(Encoding::ASCII_8BIT, TestTimezone.new('Europe/Paris').friendly_identifier(false).encoding)
  end

  def test_to_s
    assert_equal('Europe - Paris', TestTimezone.new('Europe/Paris').to_s)
    assert_equal('America - Knox, Indiana', TestTimezone.new('America/Indiana/Knox').to_s)
    assert_equal('Antarctica - Dumont D\'Urville', TestTimezone.new('Antarctica/DumontDUrville').to_s)
    assert_equal('Antarctica - McMurdo', TestTimezone.new('Antarctica/McMurdo').to_s)
    assert_equal('Etc - GMT+1', TestTimezone.new('Etc/GMT+1').to_s)
    assert_equal('UTC', TestTimezone.new('UTC').to_s)
  end

  def test_period_for_local
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,1,0,0).to_i),
      TimezoneTransition.new(o2, o1, Time.utc(2005,03,27,1,0,0).to_i))

    time_types_test(:unspecified_offset) do |h|
      t = h.time(2005,2,18,16,24,23)
      assert_equal(period, TestTimezone.new('Europe/London', nil, [period], Timestamp.for(t, :ignore)).period_for_local(t))
    end

    time_types_test(:utc) do |h|
      t = h.time(2005,2,18,16,24,23,0,:utc)
      assert_equal(period, TestTimezone.new('Europe/London', nil, [period], Timestamp.for(t, :ignore)).period_for_local(t))
    end

    time_types_test(:offset) do |h|
      t = h.time(2005,2,18,16,24,23,0,18000)
      assert_equal(period, TestTimezone.new('Europe/London', nil, [period], Timestamp.for(t, :ignore)).period_for_local(t))
    end

    time_types_test do |h|
      t1 = h.time(2005,2,18,16,24,23,0,0)
      t2 = h.time(2005,2,18,16,24,23,Rational(789,1000), 0)

      assert_equal(period, TestTimezone.new('Europe/London', nil, [period], Timestamp.for(t1, :ignore)).period_for_local(t1))
      assert_equal(period, TestTimezone.new('Europe/London', nil, [period], Timestamp.for(t2, :ignore)).period_for_local(t2))
    end
  end

  def test_period_for_local_not_found
    time_types_test do |h|
      t = h.time(2004,4,4,2,30,0)
      tz = TestTimezone.new('America/New_York', nil, [], Timestamp.for(t, :ignore))

      assert_raises_period_not_found(t) { tz.period_for_local(t) }
    end
  end

  def test_period_for_local_ambiguous
    o1 = TimezoneOffset.new(-18000, 0, 'EST')
    o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
    t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
    t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    time_types_test do |h|
      t = h.time(2004,10,31,1,30,0)
      tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.for(t, :ignore))
      assert_raises_ambiguous_time(t) { tz.period_for_local(t) }
    end
  end

  def test_period_for_local_default_dst_set_true
    Timezone.default_dst = true

    o1 = TimezoneOffset.new(-18000, 0, 'EST')
    o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
    t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
    t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    time_types_test do |h|
      t = h.time(2004,10,31,1,30,0)
      tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.for(t, :ignore))
      assert_equal(p1, tz.period_for_local(t))
      assert_equal(p1, tz.period_for_local(t, true))
      assert_equal(p2, tz.period_for_local(t, false))
      assert_raises_ambiguous_time(t) { tz.period_for_local(t, nil) }
    end
  end

  def test_period_for_local_default_dst_set_false
    Timezone.default_dst = false

    o1 = TimezoneOffset.new(-18000, 0, 'EST')
    o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
    t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
    t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    time_types_test do |h|
      t = h.time(2004,10,31,1,30,0)
      tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.for(t, :ignore))
      assert_equal(p2, tz.period_for_local(t))
      assert_equal(p1, tz.period_for_local(t, true))
      assert_equal(p2, tz.period_for_local(t, false))
      assert_raises_ambiguous_time(t) { tz.period_for_local(t, nil) }
    end
  end

  def test_period_for_local_dst_flag_resolved
    o1 = TimezoneOffset.new(-18000, 0, 'EST')
    o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
    t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
    t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    t = Time.utc(2004,10,31,1,30,0)

    tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.for(t, :ignore))

    assert_equal(p1, tz.period_for_local(t, true))
    assert_equal(p2, tz.period_for_local(t, false))
    assert_equal(p1, tz.period_for_local(t, true) {|periods| raise BlockCalled, 'should not be called' })
    assert_equal(p2, tz.period_for_local(t, false) {|periods| raise BlockCalled, 'should not be called' })
  end

  def test_period_for_local_dst_block_called
    o1 = TimezoneOffset.new(-18000, 0, 'EST')
    o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
    t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
    t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    t = Time.utc(2004,10,31,1,30,0)

    tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.for(t, :ignore))

    assert_raises(BlockCalled) do
      tz.period_for_local(t) do |periods|
        assert_equal([p1, p2], periods)

        # raise exception to test that the block was called
        raise BlockCalled, 'should be raised'
      end
    end

    assert_equal(p1, tz.period_for_local(t) {|periods| periods.first})
    assert_equal(p2, tz.period_for_local(t) {|periods| periods.last})
    assert_equal(p1, tz.period_for_local(t) {|periods| [periods.first]})
    assert_equal(p2, tz.period_for_local(t) {|periods| [periods.last]})
  end

  def test_period_for_local_dst_cannot_resolve
    # At midnight local time on Aug 5 1915 in Warsaw, the clocks were put back
    # 24 minutes and both periods were non-DST. Hence the block should be
    # called regardless of the value of the Boolean dst parameter.

    o0 = TimezoneOffset.new(5040, 0, 'LMT')
    o1 = TimezoneOffset.new(5040, 0, 'WMT')
    o2 = TimezoneOffset.new(3600, 0, 'CET')
    o3 = TimezoneOffset.new(3600, 3600, 'CEST')

    t1 = TimezoneTransition.new(o1, o0, Time.utc(1879, 12, 31, 22, 36, 0).to_i)
    t2 = TimezoneTransition.new(o2, o1, Time.utc(1915,  8,  4, 22, 36, 0).to_i)
    t3 = TimezoneTransition.new(o3, o2, Time.utc(1916,  4, 30, 22,  0, 0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    t = Time.utc(1915,8,4,23,40,0)

    tz = TestTimezone.new('Europe/Warsaw', nil, [p1, p2], Timestamp.for(t, :ignore))

    assert_raises(BlockCalled) do
      tz.period_for_local(t, true) do |periods|
        assert_equal([p1, p2], periods)
        raise BlockCalled, 'should be raised'
      end
    end

    assert_raises(BlockCalled) do
      tz.period_for_local(t, false) do |periods|
        assert_equal([p1, p2], periods)
        raise BlockCalled, 'should be raised'
      end
    end
  end

  def test_period_for_local_block_ambiguous
    o1 = TimezoneOffset.new(-18000, 0, 'EST')
    o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
    t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
    t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    t = Time.utc(2004,10,31,1,30,0)

    tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.for(t, :ignore))

    assert_raises_ambiguous_time(t) do
      tz.period_for_local(t) {|periods| nil }
    end

    assert_raises_ambiguous_time(t) do
      tz.period_for_local(t) {|periods| periods }
    end

    assert_raises_ambiguous_time(t) do
      tz.period_for_local(t) {|periods| [] }
    end

    error = assert_raises(AmbiguousTime) do
      tz.period_for_local(t) {|periods| raise AmbiguousTime, 'Custom ambiguous time message' }
    end
    assert_equal('Custom ambiguous time message', error.message)
  end

  def test_period_for_local_nil
    tz = Timezone.get('Europe/London')

    error = assert_raises(ArgumentError) { tz.period_for_local(nil) }
    assert_match(/\blocal_time\b/, error.message)
  end

  def test_period_for_local_unsupported
    tz = Timezone.get('Europe/London')
    t = Time.utc(2004,10,31,1,30,0).to_i

    error = assert_raises(ArgumentError) { tz.period_for_local(t) }
    assert_match(Regexp.new("\\b#{Regexp.escape(t.class.name)}\\b"), error.message)
  end

  def test_period_for_utc
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,1,0,0).to_i),
      TimezoneTransition.new(o2, o1, Time.utc(2005,3,27,2,0,0).to_i))

    tz = TestTimezone.new('Europe/London', period, nil, Timestamp.create(2005,2,18,16,24,23,Rational(1,10),:utc))

    time_types_test(:unspecified_offset) do |h|
      assert_equal(period, tz.period_for_utc(h.time(2005,2,18,16,24,23,Rational(1,10))))
    end

    time_types_test(:utc) do |h|
      assert_equal(period, tz.period_for_utc(h.time(2005,2,18,16,24,23,Rational(1,10),:utc)))
    end

    time_types_test(:offset) do |h|
      assert_equal(period, tz.period_for_utc(h.time(2005,2,18,16,24,23,Rational(1,10),3600)))
    end

    time_types_test do |h|
      assert_equal(period, tz.period_for_utc(h.time(2005,2,18,16,24,23,Rational(1,10),0)))
    end
  end

  def test_period_for_utc_nil
    tz = Timezone.get('Europe/London')

    error = assert_raises(ArgumentError) { tz.period_for_utc(nil) }
    assert_match(/\butc_time\b/, error.message)
  end

  def test_period_for_utc_unsupported
    tz = Timezone.get('Europe/London')
    t = Time.utc(2004,10,31,1,30,0).to_i

    error = assert_raises(ArgumentError) { tz.period_for_utc(t) }
    assert_match(Regexp.new("\\b#{Regexp.escape(t.class.name)}\\b"), error.message)
  end

  def test_utc_to_local
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o2, o1, Time.utc(2005,3,27,2,0,0).to_i),
      TimezoneTransition.new(o1, o2, Time.utc(2005,10,30,1,0,0).to_i))

    time_types_test(:unspecified_offset) do |h|
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o2,2005,6,18,17,24,23,0),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2005,6,18,16,24,23,0,:utc)).utc_to_local(h.time(2005,6,18,16,24,23,0)))
    end

    time_types_test(:utc) do |h|
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o2,2005,6,18,17,24,23,0),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2005,6,18,16,24,23,0,:utc)).utc_to_local(h.time(2005,6,18,16,24,23,0,:utc)))
    end

    time_types_test(:offset) do |h|
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o2,2005,6,18,17,24,23,0),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2005,6,18,16,24,23,0,:utc)).utc_to_local(h.time(2005,6,18,16,24,23,0,10800)))
    end

    time_types_test do |h|
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o2,2005,6,18,17,24,23,0),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2005,6,18,16,24,23,0,:utc)).utc_to_local(h.time(2005,6,18,16,24,23,0,0)))
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o2,2005,6,18,17,24,23,Rational(567,1000)),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2005,6,18,16,24,23,Rational(567,1000),:utc)).utc_to_local(h.time(2005,6,18,16,24,23,Rational(567,1000),0)))
    end
  end

  def test_utc_to_local_zero_offset_not_utc
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o1, o2, Time.utc(2016,10,30,1,0,0).to_i),
      TimezoneTransition.new(o2, o1, Time.utc(2017, 3,26,1,0,0).to_i))

    time_types_test(:utc) do |h|
      # test that the result doesn't have utc? set to true
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o1,2016,12,31,0,0,0,0),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2016,12,31,0,0,0,0,:utc)).utc_to_local(h.time(2016,12,31,0,0,0,0,:utc)))
    end
  end

  def test_utc_to_local_local_time_input
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o2, o1, Time.utc(2005,3,27,2,0,0).to_i),
      TimezoneTransition.new(o1, o2, Time.utc(2005,10,30,1,0,0).to_i))

    time_types_test do |h|
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o2,2005,6,18,17,24,23,0),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2005,6,18,16,24,23,0,:utc)).utc_to_local(
          h.time_with_offset(TimezoneOffset.new(10800, 0, 'TEST'),2005,6,18,16,24,23,0)))
    end
  end

  def test_utc_to_local_nil
    tz = Timezone.get('Europe/London')
    error = assert_raises(ArgumentError) { tz.utc_to_local(nil) }
    assert_match(/\butc_time\b/, error.message)
  end

  def test_utc_to_local_unsupported
    tz = Timezone.get('Europe/London')
    t = Time.utc(2004,10,31,1,30,0).to_i

    error = assert_raises(ArgumentError) { tz.utc_to_local(t) }
    assert_match(Regexp.new("\\b#{Regexp.escape(t.class.name)}\\b"), error.message)
  end

  def test_to_local
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o2, o1, Time.utc(2005,3,27,2,0,0).to_i),
      TimezoneTransition.new(o1, o2, Time.utc(2005,10,30,1,0,0).to_i))

    time_types_test(:utc) do |h|
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o2,2005,6,18,17,24,23,0),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2005,6,18,16,24,23,0,:utc)).to_local(h.time(2005,6,18,16,24,23,0,:utc)))
    end

    time_types_test(:offset) do |h|
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o2,2005,6,18,14,24,23,0),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2005,6,18,16,24,23,0,10800)).to_local(h.time(2005,6,18,16,24,23,0,10800)))
    end

    time_types_test do |h|
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o2,2005,6,18,17,24,23,0),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2005,6,18,16,24,23,0,h.expected_zero_offset)).to_local(h.time(2005,6,18,16,24,23,0,0)))
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o2,2005,6,18,17,24,23,Rational(567,1000)),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2005,6,18,16,24,23,Rational(567,1000),h.expected_zero_offset)).to_local(h.time(2005,6,18,16,24,23,Rational(567,1000),0)))
    end
  end

  def test_to_local_zero_offset_not_utc
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o1, o2, Time.utc(2016,10,30,1,0,0).to_i),
      TimezoneTransition.new(o2, o1, Time.utc(2017, 3,26,1,0,0).to_i))

    time_types_test(:utc) do |h|
      # test that the result doesn't have utc? set to true
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o1,2016,12,31,0,0,0,0),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2016,12,31,0,0,0,0,:utc)).to_local(h.time(2016,12,31,0,0,0,0,:utc)))
    end
  end

  def test_to_local_local_time_input
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o2, o1, Time.utc(2005,3,27,2,0,0).to_i),
      TimezoneTransition.new(o1, o2, Time.utc(2005,10,30,1,0,0).to_i))

    time_types_test do |h|
      assert_equal_with_offset_and_timezone_offset(h.time_with_offset(o2,2005,6,18,14,24,23,0),
        TestTimezone.new('Europe/London', period, [], Timestamp.create(2005,6,18,16,24,23,0,10800)).to_local(
          h.time_with_offset(TimezoneOffset.new(10800, 0, 'TEST'),2005,6,18,16,24,23,0)))
    end
  end

  time_with_unspecified_offset_test(:to_local)
  nil_time_test(:to_local)

  def test_to_local_unsupported
    tz = Timezone.get('Europe/London')
    t = Time.utc(2004,10,31,1,30,0).to_i

    error = assert_raises(ArgumentError) { tz.to_local(t) }
    assert_match(Regexp.new("\\b#{Regexp.escape(t.class.name)}\\b"), error.message)
  end

  def test_local_to_utc
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o2, o1, Time.utc(2005,3,27,2,0,0).to_i),
      TimezoneTransition.new(o1, o2, Time.utc(2005,10,30,1,0,0).to_i))

    time_types_test(:unspecified_offset) do |h|
      assert_equal_with_offset_and_class(h.output_time(2005,6,18,15,24,23,0,:utc),
        TestTimezone.new('Europe/London', nil, [period], Timestamp.create(2005,6,18,16,24,23,0)).local_to_utc(h.time(2005,6,18,16,24,23,0)))
    end

    time_types_test(:utc) do |h|
      assert_equal_with_offset_and_class(h.output_time(2005,6,18,15,24,23,0,:utc),
        TestTimezone.new('Europe/London', nil, [period], Timestamp.create(2005,6,18,16,24,23,0)).local_to_utc(h.time(2005,6,18,16,24,23,0,:utc)))
    end

    time_types_test(:offset) do |h|
      assert_equal_with_offset_and_class(h.output_time(2005,6,18,15,24,23,0,:utc),
        TestTimezone.new('Europe/London', nil, [period], Timestamp.create(2005,6,18,16,24,23,0)).local_to_utc(h.time(2005,6,18,16,24,23,0,10800)))
    end

    time_types_test do |h|
      assert_equal_with_offset_and_class(h.output_time(2005,6,18,15,24,23,0,:utc),
        TestTimezone.new('Europe/London', nil, [period], Timestamp.create(2005,6,18,16,24,23,0)).local_to_utc(h.time(2005,6,18,16,24,23,0,0)))
      assert_equal_with_offset_and_class(h.output_time(2005,6,18,15,24,23,Rational(567,1000),:utc),
        TestTimezone.new('Europe/London', nil, [period], Timestamp.create(2005,6,18,16,24,23,Rational(567,1000))).local_to_utc(h.time(2005,6,18,16,24,23,Rational(567,1000))))
    end
  end

  def test_local_to_utc_local_time_input
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o2, o1, Time.utc(2005,3,27,2,0,0).to_i),
      TimezoneTransition.new(o1, o2, Time.utc(2005,10,30,1,0,0).to_i))

    time_types_test do |h|
      assert_equal_with_offset_and_class(h.output_time(2005,6,18,15,24,23,0,:utc),
        TestTimezone.new('Europe/London', nil, [period], Timestamp.create(2005,6,18,16,24,23,0)).local_to_utc(
          h.time_with_offset(TimezoneOffset.new(10800,0,'TEST'),2005,6,18,16,24,23,0)))
    end
  end

  def test_local_to_utc_local_timestamp_without_offset_input
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o2, o1, Time.utc(2005,3,27,2,0,0).to_i),
      TimezoneTransition.new(o1, o2, Time.utc(2005,10,30,1,0,0).to_i))

    assert_equal_with_offset_and_class(Timestamp.create(2005,6,18,15,24,23,0,:utc),
      TestTimezone.new('Europe/London', nil, [period], Timestamp.create(2005,6,18,16,24,23,0)).local_to_utc(
        TimestampWithOffset.create(2005,6,18,16,24,23)))
  end

  def test_local_to_utc_ambiguous
    o1 = TimezoneOffset.new(-18000, 0, 'EST')
    o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

    tt1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
    tt2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
    tt3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

    p1 = TransitionsTimezonePeriod.new(tt1, tt2)
    p2 = TransitionsTimezonePeriod.new(tt2, tt3)

    time_types_test do |h|
      t1 = h.time(2004,10,31,1,30,0)
      t2 = h.time(2004,10,31,1,30,0,Rational(501,1000000))

      tz1 = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.create(2004,10,31,1,30,0))
      tz2 = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.create(2004,10,31,1,30,0,Rational(501,1000000)))

      assert_raises_ambiguous_time(t1) { tz1.local_to_utc(t1) }
      assert_raises_ambiguous_time(t2) { tz2.local_to_utc(t2) }
    end
  end

  def test_local_to_utc_not_found
    time_types_test do |h|
      t = h.time(2004,4,4,2,0,0)
      tz = TestTimezone.new('America/New_York', nil, [], Timestamp.create(2004,4,4,2,0,0))
      assert_raises_period_not_found(t) { tz.local_to_utc(t) }
    end
  end

  def test_local_to_utc_default_dst_set_true
    Timezone.default_dst = true

    o1 = TimezoneOffset.new(-18000, 0, 'EST')
    o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
    t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
    t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    time_types_test do |h|
      t = h.time(2004,10,31,1,30,0)
      tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.create(2004,10,31,1,30,0))

      assert_equal_with_offset_and_class(h.output_time(2004,10,31,5,30,0,0,:utc), tz.local_to_utc(t))
      assert_equal_with_offset_and_class(h.output_time(2004,10,31,5,30,0,0,:utc), tz.local_to_utc(t, true))
      assert_equal_with_offset_and_class(h.output_time(2004,10,31,6,30,0,0,:utc), tz.local_to_utc(t, false))
      assert_raises_ambiguous_time(t) { tz.local_to_utc(t, nil) }
      assert_equal_with_offset_and_class(h.output_time(2004,10,31,5,30,0,0,:utc), tz.local_to_utc(t) {|periods| raise BlockCalled, 'should not be called' })
    end
  end

  def test_local_to_utc_default_dst_set_false
    Timezone.default_dst = false

    o1 = TimezoneOffset.new(-18000, 0, 'EST')
    o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
    t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
    t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    time_types_test do |h|
      t = h.time(2004,10,31,1,30,0)
      tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.create(2004,10,31,1,30,0))

      assert_equal_with_offset_and_class(h.output_time(2004,10,31,6,30,0,0,:utc), tz.local_to_utc(t))
      assert_equal_with_offset_and_class(h.output_time(2004,10,31,6,30,0,0,:utc), tz.local_to_utc(t, false))
      assert_equal_with_offset_and_class(h.output_time(2004,10,31,5,30,0,0,:utc), tz.local_to_utc(t, true))
      assert_raises_ambiguous_time(t) { tz.local_to_utc(t, nil) }
      assert_equal_with_offset_and_class(h.output_time(2004,10,31,6,30,0,0,:utc), tz.local_to_utc(t) {|periods| raise BlockCalled, 'should not be called' })
    end
  end

  def test_local_to_utc_dst_flag_resolved
    o1 = TimezoneOffset.new(-18000, 0, 'EST')
    o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
    t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
    t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    time_types_test do |h|
      t = h.time(2004,10,31,1,30,0)
      tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.create(2004,10,31,1,30,0))

      assert_equal_with_offset_and_class(h.output_time(2004,10,31,5,30,0,0,:utc), tz.local_to_utc(t, true))
      assert_equal_with_offset_and_class(h.output_time(2004,10,31,6,30,0,0,:utc), tz.local_to_utc(t, false))
      assert_equal_with_offset_and_class(h.output_time(2004,10,31,5,30,0,0,:utc), tz.local_to_utc(t, true) {|periods| raise BlockCalled, 'should not be called' })
      assert_equal_with_offset_and_class(h.output_time(2004,10,31,6,30,0,0,:utc), tz.local_to_utc(t, false) {|periods| raise BlockCalled, 'should not be called' })
    end
  end

  def test_local_to_utc_dst_block_called
    o1 = TimezoneOffset.new(-18000, 0, 'EST')
    o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
    t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
    t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    time_types_test do |h|
      t = h.time(2004,10,31,1,30,0)
      tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.create(2004,10,31,1,30,0))

      assert_raises(BlockCalled) do
        tz.local_to_utc(t) do |periods|
          assert_equal([p1, p2], periods)

          # raise exception to test that the block was called
          raise BlockCalled, 'should be raised'
        end
      end

      assert_equal_with_offset_and_class(h.output_time(2004,10,31,5,30,0,0,:utc), tz.local_to_utc(t) {|periods| periods.first})
      assert_equal_with_offset_and_class(h.output_time(2004,10,31,6,30,0,0,:utc), tz.local_to_utc(t) {|periods| periods.last})
      assert_equal_with_offset_and_class(h.output_time(2004,10,31,5,30,0,0,:utc), tz.local_to_utc(t) {|periods| [periods.first]})
      assert_equal_with_offset_and_class(h.output_time(2004,10,31,6,30,0,0,:utc), tz.local_to_utc(t) {|periods| [periods.last]})
    end
  end

  def test_local_to_utc_dst_cannot_resolve
    # At midnight local time on Aug 5 1915 in Warsaw, the clocks were put back
    # 24 minutes and both periods were non-DST. Hence the block should be
    # called regardless of the value of the Boolean dst parameter.

    o0 = TimezoneOffset.new(5040, 0, 'LMT')
    o1 = TimezoneOffset.new(5040, 0, 'WMT')
    o2 = TimezoneOffset.new(3600, 0, 'CET')
    o3 = TimezoneOffset.new(3600, 3600, 'CEST')

    t1 = TimezoneTransition.new(o1, o0, Time.utc(1879, 12, 31, 22, 36, 0).to_i)
    t2 = TimezoneTransition.new(o2, o1, Time.utc(1915,  8,  4, 22, 36, 0).to_i)
    t3 = TimezoneTransition.new(o3, o2, Time.utc(1916,  4, 30, 22,  0, 0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    time_types_test do |h|
      t = h.time(1915,8,4,23,40,0)
      tz = TestTimezone.new('Europe/Warsaw', nil, [p1, p2], Timestamp.create(1915,8,4,23,40,0))

      assert_raises(BlockCalled) do
        tz.local_to_utc(t, true) do |periods|
          assert_equal([p1, p2], periods)
          raise BlockCalled, 'should be raised'
        end
      end

      assert_raises(BlockCalled) do
        tz.local_to_utc(t, false) do |periods|
          assert_equal([p1, p2], periods)
          raise BlockCalled, 'should be raised'
        end
      end

      assert_equal_with_offset_and_class(h.output_time(1915,8,4,22,16,0,0,:utc), tz.local_to_utc(t) {|periods| periods.first})
      assert_equal_with_offset_and_class(h.output_time(1915,8,4,22,40,0,0,:utc), tz.local_to_utc(t) {|periods| periods.last})
      assert_equal_with_offset_and_class(h.output_time(1915,8,4,22,16,0,0,:utc), tz.local_to_utc(t) {|periods| [periods.first]})
      assert_equal_with_offset_and_class(h.output_time(1915,8,4,22,40,0,0,:utc), tz.local_to_utc(t) {|periods| [periods.last]})
    end
  end

  def test_local_to_utc_block_ambiguous
    o1 = TimezoneOffset.new(-18000, 0, 'EST')
    o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
    t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
    t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

    p1 = TransitionsTimezonePeriod.new(t1, t2)
    p2 = TransitionsTimezonePeriod.new(t2, t3)

    time_types_test do |h|
      t = h.time(2004,10,31,1,30,0)
      tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.create(2004,10,31,1,30,0))

      assert_raises_ambiguous_time(t) { tz.local_to_utc(t) {|periods| nil} }
      assert_raises_ambiguous_time(t) { tz.local_to_utc(t) {|periods| periods} }
      assert_raises_ambiguous_time(t) { tz.local_to_utc(t) {|periods| []} }
      error = assert_raises(AmbiguousTime) { tz.local_to_utc(t) {|periods| raise AmbiguousTime, 'Custom ambiguous time message'} }
      assert_equal('Custom ambiguous time message', error.message)
    end
  end

  def test_local_to_utc_nil
    tz = Timezone.get('Europe/London')
    error = assert_raises(ArgumentError) { tz.local_to_utc(nil) }
    assert_match(/\blocal_time\b/, error.message)
  end

  def test_local_to_utc_unsupported
    tz = Timezone.get('Europe/London')
    t = Time.utc(2004,10,31,1,30,0).to_i

    error = assert_raises(ArgumentError) { tz.local_to_utc(t) }
    assert_match(Regexp.new("\\b#{Regexp.escape(t.class.name)}\\b"), error.message)
  end

  time_types_helpers(:output) do |h|
    method = "local_#{h.type}"

    define_method("test_#{method}") do
      o1 = TimezoneOffset.new(0, 0, 'GMT')
      o2 = TimezoneOffset.new(0, 3600, 'BST')

      period = TransitionsTimezonePeriod.new(
        TimezoneTransition.new(o2, o1, Time.utc(2005,3,27,2,0,0).to_i),
        TimezoneTransition.new(o1, o2, Time.utc(2005,10,30,1,0,0).to_i))

      assert_equal_with_offset_and_class(h.time_with_offset(o2,2005,6,18,16,24,23,0),
        TestTimezone.new('Europe/London', nil, [period], Timestamp.create(2005,6,18,16,24,23,0)).public_send(method,2005,6,18,16,24,23,0))
      assert_equal_with_offset_and_class(h.time_with_offset(o2,2005,6,18,16,24,23,Rational(567,1000)),
        TestTimezone.new('Europe/London', nil, [period], Timestamp.create(2005,6,18,16,24,23,Rational(567,1000))).public_send(method,2005,6,18,16,24,23,Rational(567,1000)))
    end

    define_method("test_#{method}_not_found") do
      ts = Timestamp.create(2004,4,4,2,30,0)
      tz = TestTimezone.new('America/New_York', nil, [], ts)
      assert_raises_period_not_found(ts) { tz.public_send(method,2004,4,4,2,30,0) }
    end

    define_method("test_#{method}_ambiguous") do
      o1 = TimezoneOffset.new(-18000, 0, 'EST')
      o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
      t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

      p1 = TransitionsTimezonePeriod.new(t1, t2)
      p2 = TransitionsTimezonePeriod.new(t2, t3)

      ts = Timestamp.create(2004,10,31,1,30,0)

      tz = TestTimezone.new('America/New_York', nil, [p1, p2], ts)

      assert_raises_ambiguous_time(ts) { tz.public_send(method,2004,10,31,1,30,0) }
    end

    define_method("test_#{method}_default_dst_set_true") do
      Timezone.default_dst = true

      o1 = TimezoneOffset.new(-18000, 0, 'EST')
      o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
      t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

      p1 = TransitionsTimezonePeriod.new(t1, t2)
      p2 = TransitionsTimezonePeriod.new(t2, t3)

      ts = Timestamp.create(2004,10,31,1,30,0)

      tz = TestTimezone.new('America/New_York', nil, [p1, p2], ts)

      assert_equal_with_offset_and_class(h.time_with_offset(o2,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0,0))
      assert_equal_with_offset_and_class(h.time_with_offset(o2,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0,0,true))
      assert_equal_with_offset_and_class(h.time_with_offset(o1,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0,0,false))
      assert_raises_ambiguous_time(ts) { tz.public_send(method,2004,10,31,1,30,0,0,nil) }
      assert_equal_with_offset_and_class(h.time_with_offset(o2,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0,0) {|periods| raise BlockCalled, 'should not be called' })
    end

    define_method("test_#{method}_default_dst_set_false") do
      Timezone.default_dst = false

      o1 = TimezoneOffset.new(-18000, 0, 'EST')
      o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
      t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

      p1 = TransitionsTimezonePeriod.new(t1, t2)
      p2 = TransitionsTimezonePeriod.new(t2, t3)

      ts = Timestamp.create(2004,10,31,1,30,0)

      tz = TestTimezone.new('America/New_York', nil, [p1, p2], ts)

      assert_equal_with_offset_and_class(h.time_with_offset(o1,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0,0))
      assert_equal_with_offset_and_class(h.time_with_offset(o1,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0,0,false))
      assert_equal_with_offset_and_class(h.time_with_offset(o2,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0,0,true))
      assert_raises_ambiguous_time(ts) { tz.public_send(method,2004,10,31,1,30,0,0,nil) }
      assert_equal_with_offset_and_class(h.time_with_offset(o1,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0,0) {|periods| raise BlockCalled, 'should not be called' })
    end

    define_method("test_#{method}_default_dst_flag_resolved") do
      o1 = TimezoneOffset.new(-18000, 0, 'EST')
      o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
      t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

      p1 = TransitionsTimezonePeriod.new(t1, t2)
      p2 = TransitionsTimezonePeriod.new(t2, t3)

      tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.create(2004,10,31,1,30,0))

      assert_equal_with_offset_and_class(h.time_with_offset(o2,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0,0,true))
      assert_equal_with_offset_and_class(h.time_with_offset(o1,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0,0,false))
      assert_equal_with_offset_and_class(h.time_with_offset(o2,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0,0,true) {|periods| raise BlockCalled, 'should not be called' })
      assert_equal_with_offset_and_class(h.time_with_offset(o1,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0,0,false) {|periods| raise BlockCalled, 'should not be called' })
    end

    define_method("test_#{method}_default_dst_block_called") do
      o1 = TimezoneOffset.new(-18000, 0, 'EST')
      o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
      t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

      p1 = TransitionsTimezonePeriod.new(t1, t2)
      p2 = TransitionsTimezonePeriod.new(t2, t3)

      tz = TestTimezone.new('America/New_York', nil, [p1, p2], Timestamp.create(2004,10,31,1,30,0))

      assert_raises(BlockCalled) do
        tz.public_send(method,2004,10,31,1,30,0) do |periods|
          assert_equal([p1, p2], periods)

          # raise exception to test that the block was called
          raise BlockCalled, 'should be raised'
        end
      end

      assert_equal_with_offset_and_class(h.time_with_offset(o2,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0) {|periods| periods.first })
      assert_equal_with_offset_and_class(h.time_with_offset(o1,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0) {|periods| periods.last })
      assert_equal_with_offset_and_class(h.time_with_offset(o2,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0) {|periods| [periods.first] })
      assert_equal_with_offset_and_class(h.time_with_offset(o1,2004,10,31,1,30,0,0), tz.public_send(method,2004,10,31,1,30,0) {|periods| [periods.last] })
    end

    define_method("test_#{method}_default_dst_cannot_resolve") do
      # At midnight local time on Aug 5 1915 in Warsaw, the clocks were put back
      # 24 minutes and both periods were non-DST. Hence the block should be
      # called regardless of the value of the Boolean dst parameter.

      o0 = TimezoneOffset.new(5040, 0, 'LMT')
      o1 = TimezoneOffset.new(5040, 0, 'WMT')
      o2 = TimezoneOffset.new(3600, 0, 'CET')
      o3 = TimezoneOffset.new(3600, 3600, 'CEST')

      t1 = TimezoneTransition.new(o1, o0, Time.utc(1879, 12, 31, 22, 36, 0).to_i)
      t2 = TimezoneTransition.new(o2, o1, Time.utc(1915,  8,  4, 22, 36, 0).to_i)
      t3 = TimezoneTransition.new(o3, o2, Time.utc(1916,  4, 30, 22,  0, 0).to_i)

      p1 = TransitionsTimezonePeriod.new(t1, t2)
      p2 = TransitionsTimezonePeriod.new(t2, t3)

      tz = TestTimezone.new('Europe/Warsaw', nil, [p1, p2], Timestamp.create(1915,8,4,23,40,0))

      assert_raises(BlockCalled) do
        tz.public_send(method,1915,8,4,23,40,0,0,true) do |periods|
          assert_equal([p1, p2], periods)
          raise BlockCalled, 'should be raised'
        end
      end

      assert_raises(BlockCalled) do
        tz.public_send(method,1915,8,4,23,40,0,0,false) do |periods|
          assert_equal([p1, p2], periods)
          raise BlockCalled, 'should be raised'
        end
      end

      assert_equal_with_offset_and_class(h.time_with_offset(o1,1915,8,4,23,40,0,0), tz.public_send(method,1915,8,4,23,40,0) {|periods| periods.first})
      assert_equal_with_offset_and_class(h.time_with_offset(o2,1915,8,4,23,40,0,0), tz.public_send(method,1915,8,4,23,40,0) {|periods| periods.last})
      assert_equal_with_offset_and_class(h.time_with_offset(o1,1915,8,4,23,40,0,0), tz.public_send(method,1915,8,4,23,40,0) {|periods| [periods.first]})
      assert_equal_with_offset_and_class(h.time_with_offset(o2,1915,8,4,23,40,0,0), tz.public_send(method,1915,8,4,23,40,0) {|periods| [periods.last]})
    end

    define_method("test_#{method}_block_ambiguous") do
      o1 = TimezoneOffset.new(-18000, 0, 'EST')
      o2 = TimezoneOffset.new(-18000, 3600, 'EDT')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2004,4,4,8,0,0).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(2004,10,31,6,0,0).to_i)
      t3 = TimezoneTransition.new(o2, o1, Time.utc(2005,4,3,8,0,0).to_i)

      p1 = TransitionsTimezonePeriod.new(t1, t2)
      p2 = TransitionsTimezonePeriod.new(t2, t3)

      ts = Timestamp.create(2004,10,31,1,30,0)

      tz = TestTimezone.new('America/New_York', nil, [p1, p2], ts)

      assert_raises_ambiguous_time(ts) { tz.public_send(method,2004,10,31,1,30,0) {|periods| nil } }
      assert_raises_ambiguous_time(ts) { tz.public_send(method,2004,10,31,1,30,0) {|periods| periods } }
      assert_raises_ambiguous_time(ts) { tz.public_send(method,2004,10,31,1,30,0) {|periods| [] } }
      error = assert_raises(AmbiguousTime) { tz.public_send(method,2004,10,31,1,30,0) {|periods| raise AmbiguousTime, 'Custom ambiguous time message' } }
      assert_equal('Custom ambiguous time message', error.message)
    end

    define_method("test_#{method}_invalid_days_for_specific_month") do
      o = TimezoneOffset.new(18000, 0, 'TEST')
      p = OffsetTimezonePeriod.new(o)

      [[2018,2,29],[2018,11,31]].each do |time_args|
        tz = TestTimezone.new('Test', nil, [p], Timestamp.create(*time_args))
        assert_equal_with_offset_and_class(h.time_with_offset(o,time_args[0],time_args[1]+1,1,0,0,0,0), tz.public_send(method,*time_args))
      end
    end

    define_method("test_#{method}_month_out_of_range") do
      tz = Timezone.get('Europe/London')

      [0, 13].each do |month|
        error = assert_raises(RangeError) { tz.public_send(method, 2018, month) }
        assert_equal('month must be between 1 and 12', error.message)
      end
    end

    define_method("test_#{method}_day_out_of_range") do
      tz = Timezone.get('Europe/London')

      [0, 32].each do |day|
        error = assert_raises(RangeError) { tz.public_send(method, 2018, 1, day) }
        assert_equal('day must be between 1 and 31', error.message)
      end
    end

    define_method("test_#{method}_hour_out_of_range") do
      tz = Timezone.get('Europe/London')

      [-1, 24].each do |hour|
        error = assert_raises(RangeError) { tz.public_send(method, 2018, 1, 1, hour) }
        assert_equal('hour must be between 0 and 23', error.message)
      end
    end

    define_method("test_#{method}_minute_out_of_range") do
      tz = Timezone.get('Europe/London')

      [-1, 60].each do |minute|
        error = assert_raises(RangeError) { tz.public_send(method, 2018, 1, 1, 0, minute) }
        assert_equal('minute must be between 0 and 59', error.message)
      end
    end

    define_method("test_#{method}_second_out_of_range") do
      tz = Timezone.get('Europe/London')

      [-1, 60].each do |second|
        error = assert_raises(RangeError) { tz.public_send(method, 2018, 1, 1, 0, 0, second) }
        assert_equal('second must be between 0 and 59', error.message)
      end
    end

    define_method("test_#{method}_sub_second_out_of_range") do
      tz = Timezone.get('Europe/London')

      [Rational(-1, 10), Rational(11, 10)].each do |sub_second|
        error = assert_raises(RangeError) { tz.public_send(method, 2018, 1, 1, 0, 0, 0, sub_second) }
        assert_equal('sub_second must be >= 0 and < 1', error.message)
      end
    end

    define_method("test_#{method}_year_not_integer") do
      tz = Timezone.get('Europe/London')

      error = assert_raises(ArgumentError) { tz.public_send(method, 2018.0) }
      assert_equal('year must be an Integer', error.message)
    end

    define_method("test_#{method}_month_not_integer") do
      tz = Timezone.get('Europe/London')

      error = assert_raises(ArgumentError) { tz.public_send(method, 2018, 1.0) }
      assert_equal('month must be an Integer', error.message)
    end

    define_method("test_#{method}_day_not_integer") do
      tz = Timezone.get('Europe/London')

      error = assert_raises(ArgumentError) { tz.public_send(method, 2018, 1, 1.0) }
      assert_equal('day must be an Integer', error.message)
    end

    define_method("test_#{method}_hour_not_integer") do
      tz = Timezone.get('Europe/London')

      error = assert_raises(ArgumentError) { tz.public_send(method, 2018, 1, 1, 0.0) }
      assert_equal('hour must be an Integer', error.message)
    end

    define_method("test_#{method}_minute_not_integer") do
      tz = Timezone.get('Europe/London')

      error = assert_raises(ArgumentError) { tz.public_send(method, 2018, 1, 1, 0, 0.0) }
      assert_equal('minute must be an Integer', error.message)
    end

    define_method("test_#{method}_second_not_integer") do
      tz = Timezone.get('Europe/London')

      error = assert_raises(ArgumentError) { tz.public_send(method, 2018, 1, 1, 0, 0, 0.0) }
      assert_equal('second must be an Integer', error.message)
    end

    define_method("test_#{method}_sub_second_not_zero_integer_or_rational") do
      tz = Timezone.get('Europe/London')

      [nil, 0.1, 1].each do |sub_second|
        error = assert_raises(ArgumentError) { tz.public_send(method, 2018, 1, 1, 0, 0, 0, sub_second) }
        assert_equal('sub_second must be a Rational or the Integer 0', error.message)
      end
    end
  end

  def test_offsets_up_to_utc_and_zero_offset
    o1 = TimezoneOffset.new(-17900, 0,    'TESTLMT')
    o2 = TimezoneOffset.new(-18000, 3600, 'TESTD')
    o3 = TimezoneOffset.new(-18000, 0,    'TESTS')
    o4 = TimezoneOffset.new(-21600, 3600, 'TESTD')
    o5 = TimezoneOffset.new(-21600, 0,    'TESTS')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2010, 4,1,1,0,0).to_i)
    t2 = TimezoneTransition.new(o3, o2, Time.utc(2010,10,1,1,0,0).to_i)
    t3 = TimezoneTransition.new(o2, o3, Time.utc(2011, 3,1,1,0,0).to_i)
    t4 = TimezoneTransition.new(o4, o2, Time.utc(2011, 4,1,1,0,0).to_i)
    t5 = TimezoneTransition.new(o3, o4, Time.utc(2011,10,1,1,0,0).to_i)
    t6 = TimezoneTransition.new(o5, o3, Time.utc(2012, 3,1,1,0,0).to_i)

    time_types_test(:utc) do |h|
      assert_array_same_items([o1, o2, o3, o4, o5],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2012,3,1,1,0,1,0,:utc), nil, [t1, t2, t3, t4, t5, t6]).
        offsets_up_to(h.time(2012,3,1,1,0,1,0,:utc)))
      assert_array_same_items([o2, o3, o4, o5],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2012,3,1,1,0,1,0,:utc), Timestamp.create(2010,4,1,1,0,0,0,:utc), [t1, t2, t3, t4, t5, t6]).
        offsets_up_to(h.time(2012,3,1,1,0,1,0,:utc), h.time(2010,4,1,1,0,0,0,:utc)))
      assert_array_same_items([o1, o2, o3, o4],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2012,3,1,1,0,0,0,:utc), nil, [t1, t2, t3, t4, t5]).
        offsets_up_to(h.time(2012,3,1,1,0,0,0,:utc)))
      assert_array_same_items([o2, o3, o4, o5],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2012,3,1,1,0,1,0,:utc), Timestamp.create(2010,4,1,1,0,1,0,:utc), [t2, t3, t4, t5, t6]).
        offsets_up_to(h.time(2012,3,1,1,0,1,0,:utc), h.time(2010,4,1,1,0,1,0,:utc)))
      assert_array_same_items([o2, o3],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2011,3,1,2,0,0,0,:utc), Timestamp.create(2011,3,1,0,0,0,0,:utc), [t3]).
        offsets_up_to(h.time(2011,3,1,2,0,0,0,:utc), h.time(2011,3,1,0,0,0,0,:utc)))
      assert_array_same_items([o3, o4],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2012,3,1,1,0,0, 0, :utc), Timestamp.create(2011,4,1,1,0,0,0,:utc), [t4, t5]).
        offsets_up_to(h.time(2012,3,1,1,0,0,0,:utc), h.time(2011,4,1,1,0,0,0,:utc)))
    end

    time_types_test do |h|
      assert_array_same_items([o1, o2, o3, o4, o5],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2012,3,1,1,0,1,0,h.expected_zero_offset), nil, [t1, t2, t3, t4, t5, t6]).
        offsets_up_to(h.time(2012,3,1,1,0,1,0,0)))
      assert_array_same_items([o2, o3, o4, o5],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2012,3,1,1,0,1,0,h.expected_zero_offset), Timestamp.create(2010,4,1,1,0,0,0,h.expected_zero_offset), [t1, t2, t3, t4, t5, t6]).
        offsets_up_to(h.time(2012,3,1,1,0,1,0,0), h.time(2010,4,1,1,0,0,0,0)))
      assert_array_same_items([o1, o2, o3, o4],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2012,3,1,1,0,0,0,h.expected_zero_offset), nil, [t1, t2, t3, t4, t5]).
        offsets_up_to(h.time(2012,3,1,1,0,0,0,0)))
      assert_array_same_items([o2, o3, o4, o5],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2012,3,1,1,0,1,0,h.expected_zero_offset), Timestamp.create(2010,4,1,1,0,1,0, h.expected_zero_offset), [t2, t3, t4, t5, t6]).
        offsets_up_to(h.time(2012,3,1,1,0,1,0,0), h.time(2010,4,1,1,0,1,0,0)))
      assert_array_same_items([o2, o3],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2011,3,1,2,0,0,0,h.expected_zero_offset), Timestamp.create(2011,3,1,0,0,0,0,h.expected_zero_offset), [t3]).
        offsets_up_to(h.time(2011,3,1,2,0,0,0,0), h.time(2011,3,1,0,0,0,0,0)))
      assert_array_same_items([o3, o4],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2012,3,1,1,0,0,0,h.expected_zero_offset), Timestamp.create(2011,4,1,1,0,0,0,h.expected_zero_offset), [t4, t5]).
        offsets_up_to(h.time(2012,3,1,1,0,0,0,0), h.time(2011,4,1,1,0,0,0,0)))
    end
  end

  def test_offsets_up_to_offset
    o1 = TimezoneOffset.new(-17900,    0, 'TESTLMT')
    o2 = TimezoneOffset.new(-18000,    0, 'TESTS')
    o3 = TimezoneOffset.new(-18000, 3600, 'TESTD')

    t1 = TimezoneTransition.new(o2, o1, Time.utc(2009,12,31,23,59,59).to_i)
    t2 = TimezoneTransition.new(o3, o2, Time.utc(2010, 7, 1, 0, 0, 0).to_i)

    time_types_test(:offset) do |h|
      assert_array_same_items([o1, o2, o3],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2010,6,30,23,0,1,0,-3600), nil, [t1, t2]).
        offsets_up_to(h.time(2010,6,30,23,0,1,0,-3600)))
      assert_array_same_items([o1, o2],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2010,7,1,1,0,0,0,3600), nil, [t1]).
        offsets_up_to(h.time(2010,7,1,1,0,0,0,3600)))
      assert_array_same_items([o2, o3],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2011,1,1,0,0,0,0,h.expected_zero_offset), Timestamp.create(2010,1,1,0,59,59,0,3600), [t1, t2]).
        offsets_up_to(h.time(2011,1,1,0,0,0,0,0), h.time(2010,1,1,0,59,59,0,3600)))
      assert_array_same_items([o2, o3],
        OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2011,1,1,0,0,0,0,h.expected_zero_offset), Timestamp.create(2009,12,31,23,0,0,0,-3600), [t2]).
        offsets_up_to(h.time(2011,1,1,0,0,0,0,0), h.time(2009,12,31,23,0,0,0,-3600)))
    end
  end

  def test_offsets_up_to_no_transitions
    o = TimezoneOffset.new(600, 0, 'LMT')
    p = OffsetTimezonePeriod.new(o)

    time_types_test(:utc) do |h|
      assert_array_same_items([o],
        OffsetsUpToNoTransitionsTestTimezone.new('Test/Zone', Timestamp.create(2000,1,1,1,0,0,0,:utc), nil, p).
        offsets_up_to(h.time(2000,1,1,1,0,0,0,:utc)))
      assert_array_same_items([o],
        OffsetsUpToNoTransitionsTestTimezone.new('Test/Zone', Timestamp.create(2000,1,1,1,0,0,0,:utc), Timestamp.create(1990,1,1,1,0,0,0,:utc), p).
        offsets_up_to(h.time(2000,1,1,1,0,0,0,:utc), h.time(1990,1,1,1,0,0,0,:utc)))
    end

    time_types_test(:utc) do |h|
      assert_array_same_items([o],
        OffsetsUpToNoTransitionsTestTimezone.new('Test/Zone', Timestamp.create(2000,1,1,1,0,0,0,h.expected_zero_offset), nil, p).
        offsets_up_to(h.time(2000,1,1,1,0,0,0,0)))
      assert_array_same_items([o],
        OffsetsUpToNoTransitionsTestTimezone.new('Test/Zone', Timestamp.create(2000,1,1,1,0,0,0,h.expected_zero_offset), Timestamp.create(1990,1,1,1,0,0,0,h.expected_zero_offset), p).
        offsets_up_to(h.time(2000,1,1,1,0,0,0,0), h.time(1990,1,1,1,0,0,0,0)))

      assert_array_same_items([o],
        OffsetsUpToNoTransitionsTestTimezone.new('Test/Zone', Timestamp.create(2000,1,1,2,0,0,0,3600), nil, p).
        offsets_up_to(h.time(2000,1,1,2,0,0,0,3600)))
      assert_array_same_items([o],
        OffsetsUpToNoTransitionsTestTimezone.new('Test/Zone', Timestamp.create(2000,1,1,2,0,0,0,3600), Timestamp.create(1990,1,1,2,0,0,0,3600), p).
        offsets_up_to(h.time(2000,1,1,2,0,0,0,3600), h.time(1990,1,1,2,0,0,0,3600)))
    end
  end

  def test_offsets_up_to_utc_to_not_greater_than_utc_from
    time_types_test do |h|
      ts = Timestamp.create(2012,8,1,0,0,0,0,h.expected_zero_offset)
      tz = OffsetsUpToTestTimezone.new('Test/Zone', ts, ts, [])
      to = h.time(2012,8,1,0,0,0,0,0)
      from = h.time(2012,8,1,0,0,0,0,0)
      error = assert_raises(ArgumentError) { tz.offsets_up_to(to, from) }
      assert_equal('to must be greater than from', error.message)
    end
  end

  def test_offsets_up_to_unspecified_offset_to
    tz = OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2012,8,1,0,0,0), nil, [])

    time_types_test(:unspecified_offset) do |h|
      to = h.time(2012,8,1,0,0,0,0,nil)
      error = assert_raises(ArgumentError) { tz.offsets_up_to(to) }
      assert_equal('to must have a specified utc_offset', error.message)
    end
  end

  def test_offsets_up_to_unspecified_offset_from
    tz = OffsetsUpToTestTimezone.new('Test/Zone', Timestamp.create(2012,8,1,0,0,0,0,0), Timestamp.create(2012,1,1,0,0,0), [])

    time_types_test(:unspecified_offset) do |h|
      to = h.time(2012,8,1,0,0,0,0,0)
      from = h.time(2012,1,1,0,0,0,0,nil)
      error = assert_raises(ArgumentError) { tz.offsets_up_to(to, from) }
      assert_equal('from must have a specified utc_offset', error.message)
    end
  end

  def test_offsets_up_to_nil_to
    tz = OffsetsUpToTestTimezone.new('Test/Zone', nil, nil, [])
    error = assert_raises(ArgumentError) { tz.offsets_up_to(nil) }
    assert_equal('to must be specified', error.message)
  end

  def test_offsets_up_to_unsupported_to
    tz = Timezone.get('Europe/London')
    to = Time.utc(2012,8,1,0,0,0).to_i

    error = assert_raises(ArgumentError) { tz.offsets_up_to(to) }
    assert_match(Regexp.new("\\b#{Regexp.escape(to.class.name)}\\b"), error.message)
  end

  def test_offsets_up_to_unsupported_from
    tz = Timezone.get('Europe/London')
    to = Time.utc(2012,8,1,0,0,0)
    from = Time.utc(2012,1,1,0,0,0).to_i

    error = assert_raises(ArgumentError) { tz.offsets_up_to(to, from) }
    assert_match(Regexp.new("\\b#{Regexp.escape(from.class.name)}\\b"), error.message)
  end

  def test_now
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o2, o1, Time.utc(2005,3,27,2,0,0).to_i),
      TimezoneTransition.new(o1, o2, Time.utc(2005,10,30,1,0,0).to_i))

    now = Time.utc(2005,6,18,16,24,23).localtime

    tz = TestTimezone.new('Europe/London', period, [], Timestamp.for(now))

    Time.stub(:now, now) do
      assert_equal_with_offset_and_timezone_offset(TimeWithOffset.new(2005,6,18,17,24,23,3600).set_timezone_offset(o2), tz.now)
    end
  end

  def test_current_period
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 3600, 'BST')

    period = TransitionsTimezonePeriod.new(
      TimezoneTransition.new(o2, o1, Time.utc(2005,3,27,2,0,0).to_i),
      TimezoneTransition.new(o1, o2, Time.utc(2005,10,30,1,0,0).to_i))

    now = Time.utc(2005,6,18,16,24,23).localtime

    tz = TestTimezone.new('Europe/London', period, [], now)

    Time.stub(:now, now) do
      assert_same(period, tz.current_period)
    end
  end

  [:current_period_and_time, :current_time_and_period].each do |method|
    define_method("test_#{method}") do
      o1 = TimezoneOffset.new(0, 0, 'GMT')
      o2 = TimezoneOffset.new(0, 3600, 'BST')

      period = TransitionsTimezonePeriod.new(
        TimezoneTransition.new(o2, o1, Time.utc(2005,3,27,2,0,0).to_i),
        TimezoneTransition.new(o1, o2, Time.utc(2005,10,30,1,0,0).to_i))

      now = Time.utc(2005,6,18,16,24,23).localtime

      tz = TestTimezone.new('Europe/London', period, [], Timestamp.for(now))

      Time.stub(:now, now) do
        current = tz.public_send(method)
        assert_equal(2, current.length)
        assert_equal_with_offset_and_timezone_offset(TimeWithOffset.new(2005,6,18,17,24,23,3600).set_timezone_offset(o2), current.first)
        assert_same(period, current.last)
      end
    end
  end

  def test_compare
    assert_equal(0, TestTimezone.new('Europe/London') <=> TestTimezone.new('Europe/London'))
    assert_equal(-1, TestTimezone.new('Europe/London') <=> TestTimezone.new('Europe/london'))
    assert_equal(-1, TestTimezone.new('Europe/London') <=> TestTimezone.new('Europe/Paris'))
    assert_equal(1, TestTimezone.new('Europe/Paris') <=> TestTimezone.new('Europe/London'))
    assert_equal(-1, TestTimezone.new('America/New_York') <=> TestTimezone.new('Europe/Paris'))
    assert_equal(1, TestTimezone.new('Europe/Paris') <=> TestTimezone.new('America/New_York'))
  end

  def test_compare_non_comparable
    assert_nil(TestTimezone.new('Europe/London') <=> Object.new)
  end

  def test_equality
    assert_equal(true, TestTimezone.new('Europe/London') == TestTimezone.new('Europe/London'))
    assert_equal(false, TestTimezone.new('Europe/London') == TestTimezone.new('Europe/london'))
    assert_equal(false, TestTimezone.new('Europe/London') == TestTimezone.new('Europe/Paris'))
    assert(!(TestTimezone.new('Europe/London') == Object.new))
  end

  def test_eql
    assert_equal(true, TestTimezone.new('Europe/London').eql?(TestTimezone.new('Europe/London')))
    assert_equal(false, TestTimezone.new('Europe/London').eql?(TestTimezone.new('Europe/london')))
    assert_equal(false, TestTimezone.new('Europe/London').eql?(TestTimezone.new('Europe/Paris')))
    assert(!TestTimezone.new('Europe/London').eql?(Object.new))
  end

  def test_hash
    assert_equal('Europe/London'.hash, TestTimezone.new('Europe/London').hash)
    assert_equal('America/New_York'.hash, TestTimezone.new('America/New_York').hash)
  end

  define_method("test_=~_operator_matches_against_identifier") do
    tz = TestTimezone.new('Europe/London')
    assert_equal(0, tz  =~ /\AEurope\/London\z/)
    assert_equal(6, tz  =~ /\/London/)
  end

  define_method("test_=~_operator_returns_nil_when_regexp_does_not_match") do
    tz = TestTimezone.new('Europe/London')
    assert_nil(tz  =~ /America\/New_York/)
  end

  def test_marshal_data
    tz = Timezone.get('Europe/London')
    marshalled_tz = Marshal.load(Marshal.dump(tz))
    assert_kind_of(DataTimezone, marshalled_tz)
    assert_equal('Europe/London', marshalled_tz.identifier)
  end

  def test_marshal_linked
    tz = Timezone.get('UTC')
    marshalled_tz = Marshal.load(Marshal.dump(tz))

    # ZoneinfoDataSource doesn't return DataSources::LinkedTimezoneInfo for any timezone.
    if DataSource.get.get_timezone_info('UTC').kind_of?(DataSources::LinkedTimezoneInfo)
      assert_kind_of(LinkedTimezone, marshalled_tz)
    else
      assert_kind_of(DataTimezone, marshalled_tz)
    end

    assert_equal('UTC', marshalled_tz.identifier)
  end

  def strftime_assertions(tz, time)
    assert_equal('23:12:02 BST', tz.strftime('%H:%M:%S %Z', time))
    assert_equal('BST', tz.strftime('%Z', time))
    assert_equal('%ZBST', tz.strftime('%%Z%Z', time))
    assert_equal('BST BST', tz.strftime('%Z %Z', time))
    assert_equal('BST %Z %BST %%Z %%BST', tz.strftime('%Z %%Z %%%Z %%%%Z %%%%%Z', time))
  end

  def test_strftime
    tz = Timezone.get('Europe/London')

    time_types_test(:utc) do |h|
      strftime_assertions(tz, h.time(2006, 7, 15, 22, 12, 2, 0, :utc))
    end

    time_types_test(:offset) do |h|
      strftime_assertions(tz, h.time(2006, 7, 15, 23, 12, 2, 0,  3600))
      strftime_assertions(tz, h.time(2006, 7, 15, 21, 12, 2, 0, -3600))
    end

    time_types_test do |h|
      strftime_assertions(tz, h.time(2006, 7, 15, 22, 12, 2, 0, 0))
    end
  end

  def test_strftime_handles_percent_in_abbreviation
    o1 = TimezoneOffset.new(0, 0, 'GMT')
    o2 = TimezoneOffset.new(0, 0, '%H:%M:%S')
    tz = TestTimezone.new('Test/Zone',
      TransitionsTimezonePeriod.new(TimezoneTransition.new(o2, o1, Time.utc(2017,1,1,0,0,0).to_i), nil),
      nil, Timestamp.create(2017,1,15,15,50,0,0,:utc))

    assert_equal('%H:%M:%S', tz.strftime('%Z', Time.utc(2017,1,15,15,50,0)))
  end

  def test_strftime_unspecified_offset
    tz = Timezone.get('Europe/London')

    time_types_test(:unspecified_offset) do |h|
      t = h.time(2006, 7, 15, 22, 12, 2, 0, nil)
      error = assert_raises(ArgumentError) { tz.strftime('%Z', t) }
      assert_match(/\btime\b/, error.message)
    end
  end

  def test_strftime_nil_format
    tz = Timezone.get('Europe/London')
    t = Time.utc(2006, 7, 15, 22, 12, 2)
    error = assert_raises(ArgumentError) { tz.strftime(nil, t) }
    assert_match(/\bformat\b/, error.message)
  end

  def test_strftime_nil_time
    tz = Timezone.get('Europe/London')
    error = assert_raises(ArgumentError) { tz.strftime('%Z', nil) }
    assert_match(/\btime\b/, error.message)
  end

  def test_get_missing_data_source
    DataSource.set(DataSource.new)

    error = assert_raises(InvalidDataSource) do
      Timezone.get('Europe/London')
    end
    assert_equal('load_timezone_info not defined', error.message)
  end

  def test_all_missing_data_source
    DataSource.set(DataSource.new)

    error = assert_raises(InvalidDataSource) do
      Timezone.all
    end
    assert_match(/\A(data|linked)_timezone_identifiers not defined\z/, error.message)
  end

  def test_all_identifiers_missing_data_source
    DataSource.set(DataSource.new)

    error = assert_raises(InvalidDataSource) do
      Timezone.all_identifiers
    end
    assert_match(/\A(data|linked)_timezone_identifiers not defined\z/, error.message)
  end

  def test_all_data_zones_missing_data_source
    DataSource.set(DataSource.new)

    error = assert_raises(InvalidDataSource) do
      Timezone.all_data_zones
    end
    assert_equal('data_timezone_identifiers not defined', error.message)
  end

  def test_all_data_zone_identifiers_missing_data_source
    DataSource.set(DataSource.new)

    error = assert_raises(InvalidDataSource) do
      Timezone.all_data_zone_identifiers
    end
    assert_equal('data_timezone_identifiers not defined', error.message)
  end

  def test_all_linked_zones_missing_data_source
    DataSource.set(DataSource.new)

    error = assert_raises(InvalidDataSource) do
      Timezone.all_linked_zones
    end
    assert_equal('linked_timezone_identifiers not defined', error.message)
  end

  def test_all_linked_zone_identifiers_missing_data_source
    DataSource.set(DataSource.new)

    error = assert_raises(InvalidDataSource) do
      Timezone.all_linked_zone_identifiers
    end
    assert_equal('linked_timezone_identifiers not defined', error.message)
  end

  def test_inspect
    tz = TestTimezone.new('Europe/London')
    assert_equal('#<TCTimezone::TestTimezone: Europe/London>', tz.inspect)
  end

  %w(abbreviation abbr).each do |method|
    define_method("test_#{method}") do
      tz = Timezone.get('America/New_York')

      time_types_test do |h|
        assert_equal('EST', tz.public_send(method, h.time(2017,1,1,0,0,0,0,:utc)))
        assert_equal('EST', tz.public_send(method, h.time(2017,1,1,0,0,0,0,0)))
        assert_equal('EDT', tz.public_send(method, h.time(2017,7,1,0,0,0,0,:utc)))
        assert_equal('EDT', tz.public_send(method, h.time(2017,7,1,0,0,0,0,0)))
      end

      time_types_test(:offset) do |h|
        assert_equal('EST', tz.public_send(method, h.time(2016,12,31,19,0,0,0,-18000)))
        assert_equal('EDT', tz.public_send(method, h.time(2017, 6,30,20,0,0,0,-14400)))
      end
    end

    time_with_unspecified_offset_test(method)
    nil_time_test(method)
  end

  def test_dst?
    tz = Timezone.get('America/New_York')

    time_types_test do |h|
      assert_equal(false, tz.dst?(h.time(2017,1,1,0,0,0,0,:utc)))
      assert_equal(false, tz.dst?(h.time(2017,1,1,0,0,0,0,0)))
      assert_equal(true,  tz.dst?(h.time(2017,7,1,0,0,0,0,:utc)))
      assert_equal(true,  tz.dst?(h.time(2017,7,1,0,0,0,0,0)))
    end

    time_types_test(:offset) do |h|
      assert_equal(false, tz.dst?(h.time(2016,12,31,19,0,0,0,-18000)))
      assert_equal(true,  tz.dst?(h.time(2017, 6,30,20,0,0,0,-14400)))
    end
  end

  time_with_unspecified_offset_test(:dst?)
  nil_time_test(:dst?)

  def test_base_utc_offset
    tz = Timezone.get('America/New_York')

    time_types_test do |h|
      assert_equal(-18000, tz.base_utc_offset(h.time(2017,1,1,0,0,0,0,:utc)))
      assert_equal(-18000, tz.base_utc_offset(h.time(2017,1,1,0,0,0,0,0)))
      assert_equal(-18000, tz.base_utc_offset(h.time(2017,7,1,0,0,0,0,:utc)))
      assert_equal(-18000, tz.base_utc_offset(h.time(2017,7,1,0,0,0,0,0)))
    end

    time_types_test(:offset) do |h|
      assert_equal(-18000, tz.base_utc_offset(h.time(2016,12,31,19,0,0,0,-18000)))
      assert_equal(-18000, tz.base_utc_offset(h.time(2017, 6,30,20,0,0,0,-14400)))
    end
  end

  time_with_unspecified_offset_test(:base_utc_offset)
  nil_time_test(:base_utc_offset)

  %w(observed_utc_offset utc_offset).each do |method|
    define_method("test_#{method}") do
      tz = Timezone.get('America/New_York')

      time_types_test do |h|
        assert_equal(-18000, tz.public_send(method, h.time(2017,1,1,0,0,0,0,:utc)))
        assert_equal(-18000, tz.public_send(method, h.time(2017,1,1,0,0,0,0,0)))
        assert_equal(-14400, tz.public_send(method, h.time(2017,7,1,0,0,0,0,:utc)))
        assert_equal(-14400, tz.public_send(method, h.time(2017,7,1,0,0,0,0,0)))
      end

      time_types_test(:offset) do |h|
        assert_equal(-18000, tz.public_send(method, h.time(2016,12,31,19,0,0,0,-18000)))
        assert_equal(-14400, tz.public_send(method, h.time(2017, 6,30,20,0,0,0,-14400)))
      end
    end

    time_with_unspecified_offset_test(method)
    nil_time_test(method)
  end

  private

  def assert_raises_unknown_timezone(&block)
    error = assert_raises(UnknownTimezone, &block)
    assert_equal('TZInfo::Timezone should not be constructed directly (use TZInfo::Timezone.get instead)', error.message)
  end

  def assert_raises_ambiguous_time(time, &block)
    error = assert_raises(AmbiguousTime, &block)
    assert_equal("#{strftime(time, '%Y-%m-%d %H:%M:%S')} is an ambiguous local time.", error.message)
  end

  def assert_raises_period_not_found(time, &block)
    error = assert_raises(PeriodNotFound, &block)
    assert_equal("#{strftime(time, '%Y-%m-%d %H:%M:%S')} is an invalid local time.", error.message)
  end

  def strftime(time, format)
    return time.strftime(format) if time.respond_to?(:strftime)
    Timestamp.for(time).strftime(format)
  end
end
