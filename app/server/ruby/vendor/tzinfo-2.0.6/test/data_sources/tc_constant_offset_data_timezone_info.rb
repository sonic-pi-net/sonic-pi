# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module DataSources
  class TCConstantOffsetDataTimezoneInfo < Minitest::Test
    include TZInfo
    include TZInfo::DataSources

    def test_initialize
      offset = TimezoneOffset.new(-17900, 0, 'TESTLMT')
      identifier = 'Test/Zone'.dup
      refute(identifier.frozen?)
      i = ConstantOffsetDataTimezoneInfo.new(identifier, offset)

      assert_same(identifier, i.identifier)
      assert_same(offset, i.constant_offset)
      assert(identifier.frozen?)
    end

    def test_initialize_nil_identifier
      o = TimezoneOffset.new(-17900, 0, 'TESTLMT')

      error = assert_raises(ArgumentError) { ConstantOffsetDataTimezoneInfo.new(nil, o) }
      assert_match(/\bidentifier\b/, error.message)
    end

    def test_initialize_nil_constant_offset
      error = assert_raises(ArgumentError) { ConstantOffsetDataTimezoneInfo.new('Test/Zone', nil) }
      assert_match(/\bconstant_offset\b/, error.message)
    end

    def test_period_for
      o = TimezoneOffset.new(-17900, 0, 'TESTLMT')
      i = ConstantOffsetDataTimezoneInfo.new('Test/Zone', o)

      assert_equal(OffsetTimezonePeriod.new(o), i.period_for(Timestamp.for(Time.utc(2017,1,1,0,0,0))))
      assert_equal(OffsetTimezonePeriod.new(o), i.period_for(Timestamp.for(Time.new(2017,1,1,0,0,0,0))))
      assert_equal(OffsetTimezonePeriod.new(o), i.period_for(Timestamp.for(Time.new(2017,1,1,1,0,0,3600))))
    end

    def test_periods_for_local
      o = TimezoneOffset.new(-17900, 0, 'TESTLMT')
      i = ConstantOffsetDataTimezoneInfo.new('Test/Zone', o)

      assert_equal([OffsetTimezonePeriod.new(o)], i.periods_for_local(Timestamp.for(Time.utc(2017,1,1,0,0,0), :ignore)))
    end

    def test_transitions_up_to
      i = ConstantOffsetDataTimezoneInfo.new('Test/Zone', TimezoneOffset.new(-17900, 0, 'TESTLMT'))

      assert_equal([], i.transitions_up_to(Timestamp.for(Time.utc(2017,1,1,0,0,0))))
    end

    def test_inspect
      i = ConstantOffsetDataTimezoneInfo.new('Test/Zone', TimezoneOffset.new(0, 0, 'TEST'))
      assert_equal('#<TZInfo::DataSources::ConstantOffsetDataTimezoneInfo: Test/Zone>', i.inspect)
    end
  end
end
