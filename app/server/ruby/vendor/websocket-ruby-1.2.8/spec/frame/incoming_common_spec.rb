# encoding: binary
# frozen_string_literal: true

require 'spec_helper'

RSpec.describe 'Incoming common frame' do
  subject { WebSocket::Frame::Incoming.new }

  it 'is version 13' do
    expect(subject.version).to be 13
  end

  it 'is not decoded' do
    expect(subject.decoded?).to be false
  end

  it 'has no errors' do
    expect(subject.error?).to be false
  end

  it 'allows adding data via <<' do
    expect(subject.data).to eql ''
    subject << 'test'
    expect(subject.data).to eql 'test'
  end

  it 'raises error on invalid version' do
    subject = WebSocket::Frame::Incoming.new(version: 70)
    expect(subject.error?).to be true
    expect(subject.error).to be :unknown_protocol_version
  end
end
