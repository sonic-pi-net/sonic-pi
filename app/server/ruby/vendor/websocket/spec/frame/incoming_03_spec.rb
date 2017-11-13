# encoding: binary
require 'spec_helper'

describe 'Incoming frame draft 03' do
  let(:version) { 3 }
  let(:frame) { WebSocket::Frame::Incoming.new(:version => version, :data => encoded_text) }
  let(:encoded_text) { nil }
  let(:decoded_text) { nil }
  let(:frame_type) { nil }
  let(:error) { nil }
  subject { frame }

  it_should_behave_like 'valid_incoming_frame'

  context "should properly decode close frame" do
    let(:encoded_text) { "\x01\x05" + decoded_text }
    let(:frame_type) { :close }
    let(:decoded_text) { "Hello" }

    it_should_behave_like 'valid_incoming_frame'
  end

  context "should properly decode ping frame" do
    let(:encoded_text) { "\x02\x05" + decoded_text }
    let(:frame_type) { :ping }
    let(:decoded_text) { "Hello" }

    it_should_behave_like 'valid_incoming_frame'
  end

  context "should properly decode pong frame" do
    let(:encoded_text) { "\x03\x05" + decoded_text }
    let(:frame_type) { :pong }
    let(:decoded_text) { "Hello" }

    it_should_behave_like 'valid_incoming_frame'
  end

  context "should properly decode text frame" do
    let(:encoded_text) { "\x04\x05" + decoded_text }
    let(:decoded_text) { "Hello" }
    let(:frame_type) { :text }

    it_should_behave_like 'valid_incoming_frame'
  end

  context "should properly decode text frame with continuation" do
    let(:encoded_text) { "\x84\x03Hel\x00\x02lo" }
    let(:frame_type)   { :text }
    let(:decoded_text) { "Hello" }

    it_should_behave_like 'valid_incoming_frame'
  end

  context "should properly decode text frame in between of continuation" do
    let(:encoded_text) { "\x84\x03Hel\x03\x03abc\x00\x02lo" }
    let(:frame_type)   { [:pong, :text] }
    let(:decoded_text) { ["abc", "Hello"] }

    it_should_behave_like 'valid_incoming_frame'
  end

  context "should not return unfinished more frame" do
    let(:encoded_text) { "\x84\x03Hel\x03\x03abc" }
    let(:frame_type)   { :pong }
    let(:decoded_text) { "abc" }

    it_should_behave_like 'valid_incoming_frame'
  end

  context "should properly decode 256 bytes binary frame" do
    let(:encoded_text) { "\x05\x7E\x01\x00" + decoded_text }
    let(:frame_type) { :binary }
    let(:decoded_text) { "a" * 256 }

    it_should_behave_like 'valid_incoming_frame'
  end

  context "should properly decode 64KiB binary frame" do
    let(:encoded_text) { "\x05\x7F\x00\x00\x00\x00\x00\x01\x00\x00" + decoded_text }
    let(:frame_type) { :binary }
    let(:decoded_text) { "a" * 65536 }

    it_should_behave_like 'valid_incoming_frame'
  end

  context "should wait with incomplete frame" do
    let(:encoded_text) { "\x04\x06Hello" }
    let(:decoded_text) { nil }

    it_should_behave_like 'valid_incoming_frame'
  end

  context "should raise error with invalid opcode" do
    let(:encoded_text) { "\x09\x05Hello" }
    let(:decoded_text) { nil }
    let(:error) { WebSocket::Error::Frame::UnknownOpcode }

    it_should_behave_like 'valid_incoming_frame'
  end

  context "should raise error with too long frame" do
    let(:encoded_text) { "\x04\x7F" + "a" * WebSocket.max_frame_size }
    let(:decoded_text) { nil }
    let(:error) { WebSocket::Error::Frame::TooLong }

    it_should_behave_like 'valid_incoming_frame'
  end

  context "should raise error with continuation frame without more frame earlier" do
    let(:encoded_text) { "\x00\x05Hello" }
    let(:decoded_text) { nil }
    let(:error) { WebSocket::Error::Frame::UnexpectedContinuationFrame }

    it_should_behave_like 'valid_incoming_frame'
  end

end
