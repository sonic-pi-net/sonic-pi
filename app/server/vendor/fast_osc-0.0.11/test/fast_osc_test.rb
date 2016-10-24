require 'test_helper'
require 'osc-ruby'
require 'date'

class FastOscTest < Minitest::Test
  def setup
    @path = "/thisisatest"
    @args = ["", 1, 2.0, "baz", "▁▃▅▇"]
    @timestamp = Date.parse("1st Jan 1990").to_time

    @msg0 = OSC::Message.new(@path).encode
    @encoded_msg0 = @msg0.encode
    @msg1 = OSC::Message.new(@path, *@args).encode
    @encoded_msg1 = @msg1.encode
  end

  def test_that_it_has_a_version_number
    refute_nil ::FastOsc::VERSION
  end

  def test_that_it_encodes_a_single_message
    msg = FastOsc.encode_single_message(@path)

    assert_equal msg, @encoded_msg0
  end

  def test_that_it_decodes_a_single_message
    path, args = FastOsc.decode_single_message(@encoded_msg0)

    assert path == @path
    assert args == []
  end

  def test_that_it_encodes_a_single_message_with_args
    msg = FastOsc.encode_single_message(@path, @args)

    assert msg == @encoded_msg1
  end

  def test_that_it_decodes_a_single_message_with_args
    path, args = FastOsc.decode_single_message(@encoded_msg1)

    assert_equal path, @path
    assert_equal args, @args
  end

  def test_that_it_encodes_a_single_bundle
    bundle1 = OSC::Bundle.new(@timestamp, @msg1).encode
    bundle2 = FastOsc.encode_single_bundle(@timestamp, @path, @args)

    assert_equal bundle1, bundle2
  end

  def test_that_it_encodes_a_single_bundle_with_fractional_time
    bundle1 = OSC::Bundle.new(@timestamp + 0.3343215, @msg1).encode
    bundle2 = FastOsc.encode_single_bundle(@timestamp + 0.3343215, @path, @args)

    assert_equal bundle1, bundle2
  end

  def test_that_it_encodes_a_single_bundle_with_special_immediate_time
    bundle1 = OSC::Bundle.new(nil, @msg1).encode
    bundle2 = FastOsc.encode_single_bundle(nil, @path, @args)

    assert_equal bundle1, bundle2
  end

  def test_that_it_encodes_and_decodes_messages_with_symbols
    path = "/s_new"
    args = ["sonic-pi-basic_mixer", 10, 0, 2, :amp, 1, :amp_slide, 0.1, :amp_slide_shape, 1, :amp_slide_curve, 0, "in_bus", 12, "amp", 0.3 , "out_bus", 10]
    outpath, outargs = FastOsc.decode_single_message(FastOsc.encode_single_message(path, args))

    assert_equal path, outpath
    # normalize symbols to strings, round floats to 5 places
    assert_equal args.map {|x| x.is_a?(Symbol) ? x.to_s : x}, outargs.map {|x| x.is_a?(Float) ? x.round(5) : x }
  end

  def test_that_it_encodes_and_decodes_messages_with_timestamps
    path = "/s_new"
    args = [Time.at(1463234577.488746)]
    #args = [Time.at(1463234577.0)]
    outpath, outargs = FastOsc.decode_single_message(FastOsc.encode_single_message(path, args))

    assert_equal path, outpath
    assert_equal args.first.to_f.round(5), outargs.first.to_f.round(5)
  end

  def test_that_encoded_timestamps_line_up
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
