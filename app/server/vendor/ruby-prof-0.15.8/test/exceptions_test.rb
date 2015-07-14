#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class ExceptionsTest < TestCase
  def test_profile
    result = begin
      RubyProf.profile do
        raise(RuntimeError, 'Test error')
      end
    rescue
    end
    refute_nil(result)
  end
end
