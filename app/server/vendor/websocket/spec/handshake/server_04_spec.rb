require 'spec_helper'

describe 'Server draft 04 handshake' do
  let(:handshake) { WebSocket::Handshake::Server.new }
  let(:version) { 04 }
  let(:client_request) { client_handshake_04(@request_params || {}) }
  let(:server_response) { server_handshake_04(@request_params || {}) }

  it_should_behave_like 'all server drafts'

  it "should disallow request without Sec-WebSocket-Key" do
    handshake << client_request.gsub(/^Sec-WebSocket-Key:.*\n/, '')

    handshake.should be_finished
    handshake.should_not be_valid
    handshake.error.should eql(:invalid_handshake_authentication)
  end
end
