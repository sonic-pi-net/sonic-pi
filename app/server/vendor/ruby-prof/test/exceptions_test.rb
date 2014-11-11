#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class ExceptionsTest < Test::Unit::TestCase
  def test_profile
    result = begin
      RubyProf.profile do
        raise(RuntimeError, 'Test error')
      end
    rescue
    end
    assert_not_nil(result)
  end
end
