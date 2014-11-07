#!/usr/bin/env ruby

require 'helper'

class IoTest < Test::Unit::TestCase

  include MIDIWinMM
  include TestHelper
  include TestHelper::Config # before running these tests, adjust the constants in config.rb to suit your hardware setup
  # ** this test assumes that TestOutput is connected to TestInput

  def test_full_io
    sleep(1)
    messages = VariousMIDIMessages
    messages_arr = messages.inject { |a,b| a+b }.flatten
    received_arr = []
    pointer = 0
    TestOutput.open do |output|
      TestInput.open do |input|

        messages.each do |msg|

          $>.puts "sending: " + msg.inspect

          output.puts(msg)
          sleep(1)
          received = input.gets.map { |m| m[:data] }.flatten
          

          $>.puts "received: " + received.inspect

          assert_equal(messages_arr.slice(pointer, received.length), received)
          
          pointer += received.length
          
          received_arr += received
          
        end
        
        assert_equal(messages_arr.length, received_arr.length)

      end
    end
  end

  # ** this test assumes that TestOutput is connected to TestInput
  def test_full_io_bytestr
    sleep(1) # pause between tests

    messages = VariousMIDIByteStrMessages
    messages_str = messages.join
    received_str = ""
    pointer = 0

    TestOutput.open do |output|
      TestInput.open do |input|

        messages.each do |msg|

          $>.puts "sending: " + msg.inspect

          output.puts(msg)
          sleep(1)
          received = input.gets_bytestr.map { |m| m[:data] }.flatten.join
          $>.puts "received: " + received.inspect

          assert_equal(messages_str.slice(pointer, received.length), received)
          
          pointer += received.length
          
          received_str += received
          
        end
   
        assert_equal(messages_str, received_str)
        
      end
    end

  end

end