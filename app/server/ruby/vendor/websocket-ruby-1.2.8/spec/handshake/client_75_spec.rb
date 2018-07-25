# frozen_string_literal: true

require 'spec_helper'

RSpec.describe 'Client draft 75 handshake' do
  let(:handshake) { WebSocket::Handshake::Client.new({ uri: 'ws://example.com/demo', origin: 'http://example.com', version: version }.merge(@request_params || {})) }

  let(:version) { 75 }
  let(:client_request) { client_handshake_75(@request_params || {}) }
  let(:server_response) { server_handshake_75(@request_params || {}) }

  it_behaves_like 'all client drafts'

  context 'protocol header specified' do
    let(:handshake) { WebSocket::Handshake::Client.new(uri: 'ws://example.com/demo', origin: 'http://example.com', version: version, protocols: %w[binary]) }

    context 'supported' do
      it 'returns a valid handshake' do
        @request_params = { headers: { 'WebSocket-Protocol' => 'binary' } }
        handshake << server_response

        expect(handshake).to be_finished
        expect(handshake).to be_valid
      end
    end

    context 'unsupported' do
      it 'fails with an unsupported protocol error' do
        @request_params = { headers: { 'WebSocket-Protocol' => 'xmpp' } }
        handshake << server_response

        expect(handshake).to be_finished
        expect(handshake).not_to be_valid
        expect(handshake.error).to be(:unsupported_protocol)
      end
    end
  end
end
