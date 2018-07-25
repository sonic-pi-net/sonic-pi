# frozen_string_literal: true

require 'spec_helper'

RSpec.describe 'Client draft 4 handshake' do
  let(:handshake) { WebSocket::Handshake::Client.new({ uri: 'ws://example.com/demo', origin: 'http://example.com', version: version }.merge(@request_params || {})) }

  let(:version) { 4 }
  let(:client_request) { client_handshake_04({ key: handshake.handler.send(:key), version: version }.merge(@request_params || {})) }
  let(:server_response) { server_handshake_04({ accept: handshake.handler.send(:accept) }.merge(@request_params || {})) }

  it_behaves_like 'all client drafts'

  it 'disallows client with invalid challenge' do
    @request_params = { accept: 'invalid' }
    handshake << server_response

    expect(handshake).to be_finished
    expect(handshake).not_to be_valid
    expect(handshake.error).to be(:invalid_handshake_authentication)
  end

  context 'protocol header specified' do
    let(:handshake) { WebSocket::Handshake::Client.new(uri: 'ws://example.com/demo', origin: 'http://example.com', version: version, protocols: protocols) }

    context 'single protocol requested' do
      let(:protocols) { %w[binary] }

      it 'returns a valid handshake' do
        @request_params = { headers: { 'Sec-WebSocket-Protocol' => 'binary' } }
        handshake << server_response

        expect(handshake).to be_finished
        expect(handshake).to be_valid
      end
    end

    context 'multiple protocols requested' do
      let(:protocols) { %w[binary xmpp] }

      it 'returns with a valid handshake' do
        @request_params = { headers: { 'Sec-WebSocket-Protocol' => 'xmpp' } }
        handshake << server_response

        expect(handshake).to be_finished
        expect(handshake).to be_valid
      end
    end

    context 'unsupported protocol requested' do
      let(:protocols) { %w[binary xmpp] }

      it 'fails with an unsupported protocol error' do
        @request_params = { headers: { 'Sec-WebSocket-Protocol' => 'generic' } }
        handshake << server_response

        expect(handshake).to be_finished
        expect(handshake).not_to be_valid
        expect(handshake.error).to be(:unsupported_protocol)
      end
    end
  end
end
