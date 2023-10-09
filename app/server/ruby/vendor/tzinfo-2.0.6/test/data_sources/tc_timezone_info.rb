# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module DataSources
  class TCTimezoneInfo < Minitest::Test
    include TZInfo
    include TZInfo::DataSources

    def test_initialize_nil_identifier
      error = assert_raises(ArgumentError) { TimezoneInfo.new(nil) }
      assert_match(/\bidentifier\b/, error.message)
    end

    def test_identifier
      ti = TimezoneInfo.new('Test/Zone')
      assert_equal('Test/Zone', ti.identifier)
    end

    def test_identifier_frozen
      identifier = 'Test/Zone'.dup
      refute(identifier.frozen?)
      ti = TimezoneInfo.new(identifier)
      assert_same(identifier, ti.identifier)
      assert(ti.identifier.frozen?)
    end

    def test_inspect
      ti = TimezoneInfo.new('Test/Zone')
      assert_equal('#<TZInfo::DataSources::TimezoneInfo: Test/Zone>', ti.inspect)
    end

    def test_create_timezone
      ti = TimezoneInfo.new('Test/Zone')
      error = assert_raises(NotImplementedError) { ti.create_timezone }
      assert_equal('Subclasses must override create_timezone', error.message)
    end
  end
end
