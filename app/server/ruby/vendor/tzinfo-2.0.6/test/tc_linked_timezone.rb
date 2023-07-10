# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCLinkedTimezone < Minitest::Test
  include TZInfo

  class TestTimezone < Timezone
    attr_reader :period_for_result
    attr_reader :periods_for_local_result
    attr_reader :transitions_up_to_result
    attr_reader :time
    attr_reader :local_time
    attr_reader :to
    attr_reader :from

    def initialize(identifier, no_local_periods = false)
      super()
      @identifier = identifier

      # Don't have to be real TimezonePeriod or TimezoneTransition objects
      # (nothing will use them).
      @period_for_result = Object.new
      @periods_for_local_result = no_local_periods ? [] : [Object.new, Object.new]
      @transitions_up_to_result = [Object.new, Object.new]
    end

    def identifier
      @identifier
    end

    def period_for(time)
      raise ArgumentError, 'linked zone exception: nil' unless time
      @time = time
      @period_for_result
    end

    def periods_for_local(local_time)
      raise ArgumentError, 'linked zone exception: nil' unless local_time
      @local_time = local_time
      @periods_for_local_result
    end

    def transitions_up_to(to, from = nil)
      @to = to
      @from = from
      @transitions_up_to_result
    end

    def canonical_zone
      self
    end
  end


  def get_timezone(linked_tz, identifier)
    assert_equal(linked_tz.identifier, identifier)
    linked_tz
  end

  def stub_timezone_get(linked_tz = nil)
    linked_tz = TestTimezone.new('Test/Linked') unless linked_tz
    Timezone.stub(:get, ->(i) { get_timezone(linked_tz, i) }) { yield linked_tz }
  end

  def test_identifier
    stub_timezone_get do
      tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/Linked'))
      assert_equal('Test/Zone', tz.identifier)
    end
  end

  def test_invalid_linked_identifier
    error = assert_raises(InvalidTimezoneIdentifier) { LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Invalid/Identifier')) }
    assert_match(/\bInvalid\/Identifier\b/, error.message)
  end

  def test_period_for
    stub_timezone_get do |linked_tz|
      time_types_test do |h|
        tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/Linked'))
        t = h.time(2006, 6, 27, 23, 12, 28, Rational(1, 10), 0)
        assert_same(linked_tz.period_for_result, tz.period_for(t))
        assert_same(t, linked_tz.time)
      end
    end
  end

  def test_period_for_nil
    stub_timezone_get do
      tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/Linked'))

      error = assert_raises(ArgumentError) { tz.period_for(nil) }
      assert_equal('linked zone exception: nil', error.message)
    end
  end

  def test_periods_for_local
    stub_timezone_get do |linked_tz|
      time_types_test do |h|
        tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/Linked'))
        t = h.time(2006, 6, 27, 23, 12, 28, Rational(1, 10), 0)
        assert_same(linked_tz.periods_for_local_result, tz.periods_for_local(t))
        assert_same(t, linked_tz.local_time)
      end
    end
  end

  def test_periods_for_local_not_found
    stub_timezone_get(TestTimezone.new('Test/No/Local', true)) do |linked_tz|
      tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/No/Local'))
      t = Time.utc(2006, 6, 27, 23, 12, 28)
      assert_equal([], linked_tz.periods_for_local_result)
      assert_same(linked_tz.periods_for_local_result, tz.periods_for_local(t))
      assert_same(t, linked_tz.local_time)
    end
  end

  def test_periods_for_local_nil
    stub_timezone_get do
      tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/Linked'))

      error = assert_raises(ArgumentError) { tz.periods_for_local(nil) }
      assert_equal('linked zone exception: nil', error.message)
    end
  end

  def test_transitions_up_to
    stub_timezone_get do |linked_tz|
      time_types_test do |h|
        tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/Linked'))
        to = h.time(2013, 1, 1, 0, 0, 0, Rational(1, 10), 0)
        from = h.time(2012, 1, 1, 0, 0, 0, Rational(1, 10), 0)
        assert_same(linked_tz.transitions_up_to_result, tz.transitions_up_to(to, from))
        assert_same(to, linked_tz.to)
        assert_same(from, linked_tz.from)
      end
    end
  end

  def test_transitions_up_to_nil_from
    stub_timezone_get do |linked_tz|
      time_types_test do |h|
        tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/Linked'))
        linked_tz = Timezone.get('Test/Linked')
        to = h.time(2013, 1, 1, 0, 0, 0, Rational(1, 10), 0)
        assert_same(linked_tz.transitions_up_to_result, tz.transitions_up_to(to))
        assert_same(to, linked_tz.to)
        assert_nil(linked_tz.from)
      end
    end
  end

  def test_canonical_identifier
    stub_timezone_get do
      tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/Linked'))
      assert_equal('Test/Linked', tz.canonical_identifier)
    end
  end

  def test_canonical_identifier_recursive
    # Recursive links are not currently used in the Time Zone database, but
    # will be supported by TZInfo.
    recursive_data = TestTimezone.new('Test/Recursive/Data')
    recursive_linked = stub_timezone_get(recursive_data) do
      LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Recursive/Linked', 'Test/Recursive/Data'))
    end

    stub_timezone_get(recursive_linked) do
      tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/Recursive/Linked'))
      assert_equal('Test/Recursive/Data', tz.canonical_identifier)
    end
  end

  def test_canonical_zone
    stub_timezone_get do |linked_tz|
      tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/Linked'))
      assert_same(linked_tz, tz.canonical_zone)
    end
  end

  def test_canonical_zone_recursive
    # Recursive links are not currently used in the Time Zone database, but
    # will be supported by TZInfo.

    recursive_data = TestTimezone.new('Test/Recursive/Data')
    recursive_linked = stub_timezone_get(recursive_data) do
      LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Recursive/Linked', 'Test/Recursive/Data'))
    end

    stub_timezone_get(recursive_linked) do
      tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/Recursive/Linked'))
      assert_same(recursive_data, tz.canonical_zone)
    end
  end

  def test_inspect
    stub_timezone_get do
      tz = LinkedTimezone.new(DataSources::LinkedTimezoneInfo.new('Test/Zone', 'Test/Linked'))
      assert_equal('#<TZInfo::LinkedTimezone: Test/Zone>', tz.inspect)
    end
  end
end
