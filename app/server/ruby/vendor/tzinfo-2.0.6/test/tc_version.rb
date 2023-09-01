# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCVersion < Minitest::Test
  def test_version
    assert(TZInfo::VERSION =~ /\A\d+(\.\d+){2}(\.[a-z]+\d*)?\z/)
  end
end
