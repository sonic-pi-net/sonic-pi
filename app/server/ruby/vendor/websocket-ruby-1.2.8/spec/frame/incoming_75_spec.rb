# encoding: binary
# frozen_string_literal: true

require 'spec_helper'

RSpec.describe 'Incoming frame draft 75' do
  subject { frame }

  let(:version) { 75 }
  let(:frame) { WebSocket::Frame::Incoming.new(version: version, data: encoded_text) }
  let(:encoded_text) { nil }
  let(:decoded_text) { nil }
  let(:frame_type) { nil }
  let(:error) { nil }

  it_behaves_like 'valid_incoming_frame'

  context 'with valid text frame' do
    let(:encoded_text) { "\x00abc\xFF" }
    let(:decoded_text) { 'abc' }
    let(:frame_type) { :text }

    it_behaves_like 'valid_incoming_frame'
  end

  context 'with two frames' do
    let(:encoded_text) { "\x00abc\xFF\x00def\xFF" }
    let(:decoded_text) { %w[abc def] }
    let(:frame_type)   { %i[text text] }

    it_behaves_like 'valid_incoming_frame'
  end

  context 'with close frame' do
    let(:encoded_text) { "\xFF\x00" }
    let(:decoded_text) { '' }
    let(:frame_type) { :close }

    it_behaves_like 'valid_incoming_frame'
  end

  context 'with incomplete frame' do
    let(:encoded_text) { "\x00test" }
    let(:decoded_text) { nil }

    it_behaves_like 'valid_incoming_frame'
  end

  context 'with invalid frame' do
    let(:encoded_text) { 'invalid' }
    let(:error) { WebSocket::Error::Frame::Invalid }

    it_behaves_like 'valid_incoming_frame'
  end

  context 'with too long frame' do
    let(:encoded_text) { "\x00" + 'a' * WebSocket.max_frame_size + "\xFF" }
    let(:error) { WebSocket::Error::Frame::TooLong }

    it_behaves_like 'valid_incoming_frame'
  end
end
