#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path("../test_helper", __FILE__)

class DynamicMethodTest < TestCase

  class FruitMedley
    define_method(:apple) do
      sleep(0.1)
      "I'm a peach"
    end

    define_method(:orange) do
      sleep(0.2)
      "I'm an orange"
    end

    [:banana, :peach].each_with_index do |fruit,i|
      define_method(fruit) do
        sleep(i == 0 ? 0.3 : 0.4)
        "I'm a #{fruit}"
      end
    end
  end

  def setup
    # Need to use wall time for this test due to the sleep calls
    RubyProf::measure_mode = RubyProf::WALL_TIME
  end

  def test_dynamic_method
    medley = FruitMedley.new
    result = RubyProf.profile do
      medley.apple
      medley.orange
      medley.banana
      medley.peach
    end

    # RubyProf::FlatPrinter.new(result).print(STDOUT)

    methods = result.threads.first.methods.sort.reverse
    expected_method_names = %w(
      DynamicMethodTest#test_dynamic_method
      Kernel#sleep
      DynamicMethodTest::FruitMedley#peach
      DynamicMethodTest::FruitMedley#banana
      DynamicMethodTest::FruitMedley#orange
      DynamicMethodTest::FruitMedley#apple
      Symbol#to_s
    )
    assert_equal expected_method_names.join("\n"), methods.map(&:full_name).join("\n")
  end
end
