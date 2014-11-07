require "helper"

class InputBufferTest < Test::Unit::TestCase

  context "CoreMIDI" do

    setup do
      sleep(1)
    end

    context "Source#buffer" do

      setup do
        @messages = TestHelper::VariousMIDIMessages
        @messages_arr = @messages.inject { |a,b| a+b }.flatten
        @received_arr = []
        @pointer = 0

        @output = $test_device[:output].open
        @input = $test_device[:input].open
        @input.buffer.clear
      end

      should "have the correct messages in the buffer" do
        bytes = []
        @messages.each do |message|
          puts "sending: #{message.inspect}"
          @output.puts(message)
          bytes += message 

          sleep(0.5)

          buffer = @input.buffer.map { |m| m[:data] }.flatten
          puts "received: #{buffer.to_s}"
          assert_equal(bytes, buffer)
        end
        assert_equal(bytes.length, @input.buffer.map { |m| m[:data] }.flatten.length)
      end
    end

  end
end
