# encoding: binary
require 'spec_helper'

describe 'Outgoing common frame' do
  subject { WebSocket::Frame::Outgoing.new }

  its(:version) { should eql(13) }
  # Not implemented yet
  # its(:error?) { should be_false }

  it "should raise error on invalid version" do
    subject = WebSocket::Frame::Incoming.new(:version => 70)
    subject.error?.should be_true
    subject.error.should eql(:unknown_protocol_version)
  end
end
