# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module DataSources
  class TCDataTimezoneInfo < Minitest::Test
    include TZInfo
    include TZInfo::DataSources

    def test_initialize_nil_identifier
      error = assert_raises(ArgumentError) { DataTimezoneInfo.new(nil) }
      assert_match(/\bidentifier\b/, error.message)
    end

    def test_identifier
      ti = DataTimezoneInfo.new('Test/Zone')
      assert_equal('Test/Zone', ti.identifier)
    end

    def test_identifier_frozen
      identifier = 'Test/Zone'.dup
      refute(identifier.frozen?)
      lti = DataTimezoneInfo.new(identifier)
      assert_same(identifier, lti.identifier)
      assert(lti.identifier.frozen?)
    end

    def test_construct_timezone
      ti = DataTimezoneInfo.new('Test/Zone')
      tz = ti.create_timezone
      assert_kind_of(DataTimezone, tz)
      assert_equal('Test/Zone', tz.identifier)
    end

    def test_period_for
      ti = DataTimezoneInfo.new('Test/Zone')
      ts = Timestamp.utc(1480291200)
      error = assert_raises(NotImplementedError) { ti.period_for(ts) }
      assert_equal('Subclasses must override period_for', error.message)
    end

    def test_periods_for_local
      ti = DataTimezoneInfo.new('Test/Zone')
      ts = Timestamp.new(1480291200)
      error = assert_raises(NotImplementedError) { ti.periods_for_local(ts) }
      assert_equal('Subclasses must override periods_for_local', error.message)
    end

    def test_transitions_up_to
      ti = DataTimezoneInfo.new('Test/Zone')
      ts = Timestamp.utc(1480291200)
      error = assert_raises(NotImplementedError) { ti.transitions_up_to(ts) }
      assert_equal('Subclasses must override transitions_up_to', error.message)
    end

    def test_inspect
      ti = DataTimezoneInfo.new('Test/Zone')
      assert_equal('#<TZInfo::DataSources::DataTimezoneInfo: Test/Zone>', ti.inspect)
    end
  end
end
