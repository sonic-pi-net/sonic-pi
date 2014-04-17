require 'spec_helper'

describe 'Client draft 76 handshake' do
  let(:handshake) { WebSocket::Handshake::Client.new({ :uri => 'ws://example.com/demo', :origin => 'http://example.com', :version => version }.merge(@request_params || {})) }

  let(:version) { 76 }
  let(:client_request) { client_handshake_76({ :key1 => handshake.handler.send(:key1), :key2 => handshake.handler.send(:key2), :key3 => handshake.handler.send(:key3) }.merge(@request_params || {})) }
  let(:server_response) { server_handshake_76({ :challenge => handshake.handler.send(:challenge) }.merge(@request_params || {})) }

  it_should_behave_like 'all client drafts'

  it "should disallow client with invalid challenge" do
    @request_params = { :challenge => "invalid" }
    handshake << server_response

    handshake.should be_finished
    handshake.should_not be_valid
    handshake.error.should eql(:invalid_handshake_authentication)
  end
end
