# encoding: binary
require 'spec_helper'

describe 'Masking frame draft 07' do
  it "should encode and decode masked frame correctly" do
    outgoing_frame = WebSocket::Frame::Outgoing::Client.new(:data => "Hello World", :type => "text")
    outgoing_frame.to_s
    outgoing_frame.error.should be_nil
    incoming_frame = WebSocket::Frame::Incoming::Server.new(:data => outgoing_frame.to_s).next
    incoming_frame.should_not be_nil
    incoming_frame.class.should eql(WebSocket::Frame::Incoming::Server)
    incoming_frame.error.should be_nil
    incoming_frame.decoded?.should be_true
    incoming_frame.to_s.should eql('Hello World')
  end
end
