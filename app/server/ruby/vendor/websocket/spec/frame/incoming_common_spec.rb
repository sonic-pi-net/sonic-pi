# encoding: binary
require 'spec_helper'

describe 'Incoming common frame' do
  subject { WebSocket::Frame::Incoming.new }

  its(:version) { should eql(13) }
  its(:decoded?) { should be_false }
  # Not implemented yet
  # its(:error?) { should be_false }

  it "should allow adding data via <<" do
    subject.data.should eql("")
    subject << "test"
    subject.data.should eql("test")
  end

  it "should raise error on invalid version" do
    subject = WebSocket::Frame::Incoming.new(:version => 70)
    subject.error?.should be_true
    subject.error.should eql(:unknown_protocol_version)
  end
end
