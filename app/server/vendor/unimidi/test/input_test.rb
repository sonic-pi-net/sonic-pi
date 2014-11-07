require 'helper'

class UniMIDI::InputTest < Test::Unit::TestCase

  context "Input" do

    context "#buffer" do

      setup do
        sleep(1)
        @input = TestHelper.devices[:input].open
        @output = TestHelper.devices[:output].open
        @messages = TestHelper.numeric_messages
        @bytes = []
      end

      teardown do
        @input.close
        @output.close
      end

      should "add received messages to the buffer" do

        @input.buffer.clear

        @messages.each do |message|

          p "sending: #{message}"
          @output.puts(message)
          @bytes += message 
          sleep(1)
          buffer = @input.buffer.map { |m| m[:data] }.flatten
          p "received: #{buffer}"
          assert_equal(@bytes, buffer)

        end

        assert_equal(@bytes.length, @input.buffer.map { |m| m[:data] }.flatten.length)

      end

    end
  end
end
