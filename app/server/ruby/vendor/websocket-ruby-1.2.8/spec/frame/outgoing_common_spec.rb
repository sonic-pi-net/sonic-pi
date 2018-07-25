# encoding: binary
# frozen_string_literal: true

require 'spec_helper'

RSpec.describe 'Outgoing common frame' do
  subject { WebSocket::Frame::Outgoing.new }

  it 'is version 13' do
    expect(subject.version).to be 13
  end

  it 'has no errors' do
    expect(subject.error?).to be false
  end

  it 'raises error on invalid version' do
    subject = WebSocket::Frame::Incoming.new(version: 70)
    expect(subject.error?).to be true
    expect(subject.error).to be :unknown_protocol_version
  end
end
