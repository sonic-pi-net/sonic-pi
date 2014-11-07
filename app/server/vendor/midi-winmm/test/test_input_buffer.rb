#!/usr/bin/env ruby

require 'helper'

class InputBufferTest < Test::Unit::TestCase

  include MIDIWinMM
  include TestHelper
  include TestHelper::Config # before running these tests, adjust the constants in config.rb to suit your hardware setup
  # ** this test assumes that TestOutput is connected to TestInput
  
  def test_input_buffer
    sleep(1)

    messages = VariousMIDIMessages
    bytes = []

    TestOutput.open do |output|
      TestInput.open do |input|

        messages.each do |msg|

          $>.puts "sending: " + msg.inspect

          output.puts(msg)
          
          bytes += msg 
          
          sleep(0.5)
          
          buffer = input.buffer.map { |m| m[:data] }.flatten

          $>.puts "received: " + buffer.to_s
          
          assert_equal(bytes, buffer)

        end
        
        assert_equal(bytes.length, input.buffer.map { |m| m[:data] }.flatten.length)

      end
    end
  end

end