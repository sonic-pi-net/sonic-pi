# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCInfoTimezone < Minitest::Test
  include TZInfo

  def test_identifier
    tz = InfoTimezone.new(DataSources::TimezoneInfo.new('Test/Identifier'))
    assert_equal('Test/Identifier', tz.identifier)
  end

  def test_info
    i = DataSources::TimezoneInfo.new('Test/Identifier')
    tz = InfoTimezone.new(i)
    assert_same(i, tz.send(:info))
  end

  def test_inspect
    tz = InfoTimezone.new(DataSources::TimezoneInfo.new('Test/Identifier'))
    assert_equal('#<TZInfo::InfoTimezone: Test/Identifier>', tz.inspect)
  end
end

