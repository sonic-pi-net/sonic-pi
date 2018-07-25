# encoding: binary
# frozen_string_literal: true

require 'spec_helper'

RSpec.describe 'Masking frame draft 07' do
  it 'encodes and decode masked frame correctly' do
    outgoing_frame = WebSocket::Frame::Outgoing::Client.new(data: 'Hello World', type: 'text')
    outgoing_frame.to_s
    expect(outgoing_frame.error).to be_nil
    incoming_frame = WebSocket::Frame::Incoming::Server.new(data: outgoing_frame.to_s).next
    expect(incoming_frame).not_to be_nil
    expect(incoming_frame.class).to eql(WebSocket::Frame::Incoming::Server)
    expect(incoming_frame.error).to be_nil
    expect(incoming_frame.decoded?).to be true
    expect(incoming_frame.to_s).to eql('Hello World')
  end
end
