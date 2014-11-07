require "helper"

class AlsaRawMIDI::IoTest < Test::Unit::TestCase

  # ** this test assumes that TestOutput is connected to TestInput
  context "AlsaRawMIDI" do

    setup do
      sleep(1)
      @input = TestHelper.input.open
      @output = TestHelper.output.open
      @input.buffer.clear
      @pointer = 0
    end

    context "full IO" do

      context "using Arrays" do

        setup do
          @messages = TestHelper.numeric_messages
          @messages_arr = @messages.inject(&:+).flatten
          @received_arr = []
        end

        teardown do
          @input.close
          @output.close
        end
        
        should "do IO" do
          @messages.each do |message|

            p "sending: #{message}"

            @output.puts(message)
            sleep(1)
            received = @input.gets.map { |m| m[:data] }.flatten

            p "received: #{received}"

            assert_equal(@messages_arr.slice(@pointer, received.length), received)
            @pointer += received.length
            @received_arr += received
          end
          assert_equal(@messages_arr.length, @received_arr.length)
        end
      end

      context "using byte Strings" do

        setup do
          @messages = TestHelper.string_messages
          @messages_str = @messages.join
          @received_str = ""
        end

        should "do IO" do
          @messages.each do |message|

            p "sending: #{message}"

            @output.puts(message)
            sleep(1)
            received = @input.gets_bytestr.map { |m| m[:data] }.flatten.join
            p "received: #{received}"

            assert_equal(@messages_str.slice(@pointer, received.length), received)
            @pointer += received.length
            @received_str += received
          end
          assert_equal(@messages_str, @received_str)
        end

      end

    end
  end

end
