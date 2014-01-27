require 'spec_helper'

describe 'Client draft 4 handshake' do
  let(:handshake) { WebSocket::Handshake::Client.new({ :uri => 'ws://example.com/demo', :origin => 'http://example.com', :version => version }.merge(@request_params || {})) }

  let(:version) { 4 }
  let(:client_request) { client_handshake_04({ :key => handshake.handler.send(:key), :version => version }.merge(@request_params || {})) }
  let(:server_response) { server_handshake_04({ :accept => handshake.handler.send(:accept) }.merge(@request_params || {})) }

  it_should_behave_like 'all client drafts'

  it "should disallow client with invalid challenge" do
    @request_params = { :accept => "invalid" }
    handshake << server_response

    handshake.should be_finished
    handshake.should_not be_valid
    handshake.error.should eql(:invalid_handshake_authentication)
  end
end
