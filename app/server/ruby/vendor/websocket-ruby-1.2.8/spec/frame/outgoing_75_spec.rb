# encoding: binary
# frozen_string_literal: true

require 'spec_helper'

RSpec.describe 'Outgoing frame draft 75' do
  subject { frame }

  let(:version) { 75 }
  let(:frame) { WebSocket::Frame::Outgoing.new(version: version, data: decoded_text, type: frame_type) }
  let(:decoded_text) { '' }
  let(:encoded_text) { "\x00\xFF" }
  let(:frame_type) { :text }
  let(:require_sending) { true }
  let(:error) { nil }

  it_behaves_like 'valid_outgoing_frame'

  context 'should properly encode text frame' do
    let(:decoded_text) { 'abc' }
    let(:encoded_text) { "\x00abc\xFF" }
    let(:require_sending) { true }

    it_behaves_like 'valid_outgoing_frame'
  end

  context 'should properly encode close frame' do
    let(:frame_type) { :close }
    let(:decoded_text) { 'abc' }
    let(:encoded_text) { "\xFF\x00" }
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
