require "helper"

class UniMIDI::AdapterTest < Test::Unit::TestCase

  context "Adapter" do

    context "Device#type" do

      should "be an input" do
        input = TestHelper.devices[:input]
        assert_equal(:input, input.type)
      end

      should "be an output" do
        output = TestHelper.devices[:output]
        assert_equal(:output, output.type)  
      end

    end

    context "Device.count" do

      setup do
        @inputs = UniMIDI::Input.all
      end

      should "count all of the inputs" do
        assert_equal @inputs.count, UniMIDI::Input.count
      end

    end

    context "Device.find_by_name" do

      setup do
        index = rand(0..(UniMIDI::Output.count-1))
        @output = UniMIDI::Output.all[index]
      end

      should "select the correct input" do
        result = UniMIDI::Output.find_by_name(@output.name)
        assert_equal @output, result
      end

    end

    context "Device.first" do

      setup do
        @output = UniMIDI::Output.all.first
      end

      should "open the output" do
        @output.expects(:open)
        output = UniMIDI::Output.first
        @output.unstub(:open)
      end

      should "return the correct output" do
        output = UniMIDI::Output.first
        assert_equal @output, output
      end
    end

  end
end
