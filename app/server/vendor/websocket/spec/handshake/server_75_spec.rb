require 'spec_helper'

describe 'Server draft 75 handshake' do
  let(:handshake) { WebSocket::Handshake::Server.new }

  let(:version) { 75 }
  let(:client_request) { client_handshake_75(@request_params || {}) }
  let(:server_response) { server_handshake_75(@request_params || {}) }

  it_should_behave_like 'all server drafts'
end
