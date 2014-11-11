#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class FakeRackApp
  def call(env)
  end
end

class RackTest < Test::Unit::TestCase
  def test_create_print_path
    path = Dir.mktmpdir
    Dir.delete(path)

    Rack::RubyProf.new(FakeRackApp.new, :path => path)

    assert_equal(true, Dir.exist?(path))
  end
end