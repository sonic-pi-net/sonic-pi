require 'spec_helper'

describe 'Client draft 75 handshake' do
  let(:handshake) { WebSocket::Handshake::Client.new({ :uri => 'ws://example.com/demo', :origin => 'http://example.com', :version => version }.merge(@request_params || {})) }

  let(:version) { 75 }
  let(:client_request) { client_handshake_75(@request_params || {}) }
  let(:server_response) { server_handshake_75(@request_params || {}) }

  it_should_behave_like 'all client drafts'
end
