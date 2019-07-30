require 'test_helper'
require 'osc-ruby'
require 'date'

class FastOscTest < Minitest::Test
  def setup
    @path = "/thisisatest"
    @args = ["", 1, 2.0, "baz", "▁▃▅▇"]
    @args2 = [1]
    @args3 = ["somestring", 2]
    @args4 = [3, 5.0, "another_string"]
    @timestamp = Date.parse("1st Jan 1990").to_time
    @timestamp2 = Date.parse("21st Mar 2000").to_time
    @timestamp3 = Date.parse("22nd Jun 2010").to_time


    @msg0 = OSC::Message.new(@path).encode
    @encoded_msg0 = @msg0.encode
    @msg1 = OSC::Message.new(@path, *@args).encode
    @encoded_msg1 = @msg1.encode
    @msg2 = OSC::Message.new(@path, *@args2).encode
    @encoded_msg2 = @msg2.encode
    @msg3 = OSC::Message.new(@path, *@args3).encode
    @encoded_msg3 = @msg3.encode
    @msg4 = OSC::Message.new(@path, *@args4).encode
    @encoded_msg4 = @msg4.encode

    @bundle = OSC::Bundle.new(@timestamp, @msg1, @msg2)
    @encoded_bundle = @bundle.encode

    @bundle2 = OSC::Bundle.new(@timestamp2, @msg2, @bundle, @msg3)
    @encoded_bundle2 = @bundle2.encode

    @bundle3 = OSC::Bundle.new(@timestamp3, @msg4, @bundle2, @msg3, @msg2)
    @encoded_bundle3 = @bundle3.encode
  end

  def test_that_it_has_a_version_number
    refute_nil ::FastOsc::VERSION
  end

  def test_that_it_encodes_a_single_message
    msg = FastOsc.encode_single_message(@path)

    assert_equal msg, @encoded_msg0
  end

  def test_that_it_decodes_a_single_message
    FastOsc.decode(@encoded_msg0).each do |msg|
      _timestamp, osc_msgs = msg

      osc_msgs.each do |(path, args)|
        assert_equal @path, path
        assert_equal args, []
      end
    end
  end

  def test_that_it_encodes_a_single_message_with_args
    msg = FastOsc.encode_single_message(@path, @args)

    assert msg == @encoded_msg1
  end

  def test_that_it_decodes_a_single_message_with_args
    FastOsc.decode(@encoded_msg1).each do |msg|
      _timestamp, osc_msgs = msg
      path, args = osc_msgs[0]

      assert_equal path, @path
      assert_equal args, @args
      assert_equal osc_msgs.length, 1
    end
  end

  def test_that_it_decodes_a_single_message_with_args_knowing_there_are_no_bundles_using_the_decode_no_bundles_function
    path, args =  FastOsc.decode_no_bundles(@encoded_msg1)

    assert_equal path, @path
    assert_equal args, @args
  end


  def test_that_it_encodes_a_single_bundle
    bundle1 = OSC::Bundle.new(@timestamp, @msg1).encode
    bundle2 = FastOsc.encode_single_bundle(@timestamp, @path, @args)

    assert_equal bundle1, bundle2
  end

  def test_that_it_decodes_a_bundle
    msgs = FastOsc.decode(@encoded_bundle)

    timestamp, osc_msgs = msgs[0]
    assert_equal @timestamp, timestamp

    path, args = osc_msgs[0]
    assert_equal @path, path
    assert_equal @args, args

    path, args = osc_msgs[1]
    assert_equal @path, path
    assert_equal @args2, args

    assert_nil osc_msgs[2]
    assert_nil msgs[1]
  end

  def test_that_it_decodes_a_nested_bundle
    msgs = FastOsc.decode(@encoded_bundle2)

    # Example of how to process the message:
    # msgs.each do |timestamp, osc_messages|
    #  # These are the messages within this bundle
    #  puts "T: #{timestamp}, M: #{osc_messages}"
    #  osc_messages.each do |path, args|
    #   # And this is each message
    #    puts "P: #{path}, A: #{args}"
    #  end
    #end

    timestamp, osc_msgs = msgs[0]
    assert_equal @timestamp, timestamp

    path, args = osc_msgs[0]
    assert_equal @path, path
    assert_equal @args, args

    path, args = osc_msgs[1]
    assert_equal @path, path
    assert_equal @args2, args

    assert_nil osc_msgs[2]

    timestamp, osc_msgs = msgs[1]
    assert_equal @timestamp2, timestamp

    path, args = osc_msgs[0]
    assert_equal @path, path
    assert_equal @args2, args

    path, args = osc_msgs[1]
    assert_equal @path, path
    assert_equal @args3, args
    assert_nil osc_msgs[2]

    assert_nil msgs[2]
  end

  def test_that_it_decodes_another_nested_bundle
    msgs = FastOsc.decode(@encoded_bundle3)

    timestamp, osc_msgs = msgs[0]
    assert_equal @timestamp, timestamp

    path, args = osc_msgs[0]
    assert_equal @path, path
    assert_equal @args, args

    path, args = osc_msgs[1]
    assert_equal @path, path
    assert_equal @args2, args

    assert_nil osc_msgs[2]

    timestamp, osc_msgs = msgs[1]
    assert_equal @timestamp2, timestamp

    path, args = osc_msgs[0]
    assert_equal @path, path
    assert_equal @args2, args

    path, args = osc_msgs[1]
    assert_equal @path, path
    assert_equal @args3, args
    assert_nil osc_msgs[2]

    timestamp, osc_msgs = msgs[2]
    assert_equal @timestamp3, timestamp

    path, args = osc_msgs[0]
    assert_equal @path, path
    assert_equal @args4, args

    path, args = osc_msgs[1]
    assert_equal @path, path
    assert_equal @args3, args

    path, args = osc_msgs[2]
    assert_equal @path, path
    assert_equal @args2, args

    assert_nil osc_msgs[3]



    assert_nil msgs[3]

  end


  def test_that_it_encodes_a_single_bundle_with_fractional_time
    bundle1 = OSC::Bundle.new(@timestamp + 0.3343215, @msg1).encode
    bundle2 = FastOsc.encode_single_bundle(@timestamp + 0.3343215, @path, @args)

    assert_equal bundle1, bundle2
  end

  def test_that_single_messages_and_bundles_can_still_be_iterated_over
    multiple_decoded = FastOsc.decode(@encoded_bundle)
    single_decoded = FastOsc.decode(@msg1)

    timestamp1, msgs1 = multiple_decoded.first
    assert_equal msgs1.first.class, Array

    timestamp2, msgs2 = single_decoded.first
    assert_equal msgs2.first.class, Array
  end

  def test_that_it_encodes_a_single_bundle_with_special_immediate_time
    bundle1 = OSC::Bundle.new(nil, @msg1).encode
    bundle2 = FastOsc.encode_single_bundle(nil, @path, @args)

    assert_equal bundle1, bundle2
  end


  def _test_that_encoded_timestamps_line_up
    # this test is a bit convoluted but I found that fractional
    # seconds weren't working when I plugged this into Sonic Pi
    # This test ensures that the timestamp encoding matches to
    # at least a tolerance of 0.001 seconds which should catch any
    # major issues.
    start = Time.at(1463234577.4387462) - 1.0
    # assert_in_delta is 0.001 by default
    assert_in_delta 1.25, Time.at(OSC::OSCPacket.messages_from_network(FastOsc.encode_single_bundle(1463234577.688746, "/foo", []), []).first.time - 2208988800) - start
    assert_in_delta 1.5, Time.at(OSC::OSCPacket.messages_from_network(FastOsc.encode_single_bundle(1463234577.9387462, "/foo", []), []).first.time - 2208988800) - start
    assert_in_delta 1.75, Time.at(OSC::OSCPacket.messages_from_network(FastOsc.encode_single_bundle(1463234578.188746, "/foo", []), []).first.time - 2208988800) - start
    assert_in_delta 2.0,  Time.at(OSC::OSCPacket.messages_from_network(FastOsc.encode_single_bundle(1463234578.4387462, "/foo", []), []).first.time - 2208988800) - start
    assert_in_delta 2.25, Time.at(OSC::OSCPacket.messages_from_network(FastOsc.encode_single_bundle(1463234578.688746, "/foo", []), []).first.time - 2208988800) - start
    assert_in_delta 2.5,  Time.at(OSC::OSCPacket.messages_from_network(FastOsc.encode_single_bundle(1463234578.9387462, "/foo", []), []).first.time - 2208988800) - start
    assert_in_delta 2.75, Time.at(OSC::OSCPacket.messages_from_network(FastOsc.encode_single_bundle(1463234579.188746, "/foo", []), []).first.time - 2208988800) - start
  end
end
