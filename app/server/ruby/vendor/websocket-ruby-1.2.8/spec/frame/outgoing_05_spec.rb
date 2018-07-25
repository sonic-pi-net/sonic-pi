# encoding: binary
# frozen_string_literal: true

require 'spec_helper'

RSpec.describe 'Outgoing frame draft 05' do
  subject { frame }

  let(:version) { 5 }
  let(:frame) { WebSocket::Frame::Outgoing.new(version: version, data: decoded_text, type: frame_type) }
  let(:decoded_text) { '' }
  let(:encoded_text) { "\x84\x00" }
  let(:frame_type) { :text }
  let(:require_sending) { true }
  let(:error) { nil }

  it_behaves_like 'valid_outgoing_frame'

  context 'should properly encode close frame' do
    let(:frame_type) { :close }
    let(:decoded_text) { 'Hello' }
    let(:encoded_text) { "\x81\x05" + decoded_text }
    let(:require_sending) { true }

    it_behaves_like 'valid_outgoing_frame'
  end

  context 'should properly encode ping frame' do
    let(:frame_type) { :ping }
    let(:decoded_text) { 'Hello' }
    let(:encoded_text) { "\x82\x05" + decoded_text }
    let(:require_sending) { true }

    it_behaves_like 'valid_outgoing_frame'
  end

  context 'should properly encode pong frame' do
    let(:frame_type) { :pong }
    let(:decoded_text) { 'Hello' }
    let(:encoded_text) { "\x83\x05" + decoded_text }
    let(:require_sending) { true }

    it_behaves_like 'valid_outgoing_frame'
  end

  context 'should properly encode text frame' do
    let(:decoded_text) { 'Hello' }
    let(:encoded_text) { "\x84\x05" + decoded_text }
    let(:require_sending) { true }

    it_behaves_like 'valid_outgoing_frame'
  end

  context 'should properly encode 256 bytes binary frame' do
    let(:frame_type) { :binary }
    let(:decoded_text) { 'a' * 256 }
    let(:encoded_text) { "\x85\x7E\x01\x00" + decoded_text }
    let(:require_sending) { true }

    it_behaves_like 'valid_outgoing_frame'
  end

  context 'should properly encode 64KiB binary frame' do
    let(:frame_type) { :binary }
    let(:decoded_text) { 'a' * 65_536 }
    let(:encoded_text) { "\x85\x7F\x00\x00\x00\x00\x00\x01\x00\x00" + decoded_text }
    let(:require_sending) { true }

    it_behaves_like 'valid_outgoing_frame'
  end

  context 'should return error for unknown frame type' do
    let(:frame_type) { :unknown }
    let(:decoded_text) { 'Hello' }
    let(:encoded_text) { nil }
    let(:error) { :unknown_frame_type }
    let(:require_sending) { false }

    it_behaves_like 'valid_outgoing_frame'
  end
end
