#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class CallInfoVisitorTest < TestCase
  def setup
    # Need to use wall time for this test due to the sleep calls
    RubyProf::measure_mode = RubyProf::WALL_TIME
  end

  def test_visit
    result = RubyProf.profile do
      RubyProf::C1.hello
    end

    visitor = RubyProf::CallInfoVisitor.new(result.threads.first.top_call_infos)

    method_names = Array.new

    visitor.visit do |call_info, event|
      method_names << call_info.target.full_name if event == :enter
    end

    assert_equal(3, method_names.length)
    assert_equal("CallInfoVisitorTest#test_visit", method_names[0])
    assert_equal("<Class::RubyProf::C1>#hello", method_names[1])
    assert_equal("Kernel#sleep", method_names[2])
  end
end

