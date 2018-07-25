# frozen_string_literal: true

require 'spec_helper'

RSpec.describe 'Server draft 04 handshake' do
  let(:handshake) { WebSocket::Handshake::Server.new }
  let(:version) { 4 }
  let(:client_request) { client_handshake_04(@request_params || {}) }
  let(:server_response) { server_handshake_04(@request_params || {}) }

  it_behaves_like 'all server drafts'

  it 'disallows request without Sec-WebSocket-Key' do
    handshake << client_request.gsub(/^Sec-WebSocket-Key:.*\n/, '')

    expect(handshake).to be_finished
    expect(handshake).not_to be_valid
    expect(handshake.error).to be(:invalid_handshake_authentication)
  end

  context 'protocol header specified' do
    let(:handshake) { WebSocket::Handshake::Server.new(protocols: %w[binary xmpp]) }

    context 'single protocol requested' do
      it 'returns with the same protocol' do
        @request_params = { headers: { 'Sec-WebSocket-Protocol' => 'binary' } }
        handshake << client_request

        expect(handshake.to_s).to match('Sec-WebSocket-Protocol: binary')
      end
    end

    context 'multiple protocols requested' do
      it 'returns with the first supported protocol' do
        @request_params = { headers: { 'Sec-WebSocket-Protocol' => 'xmpp, binary' } }
        handshake << client_request

        expect(handshake.to_s).to match('Sec-WebSocket-Protocol: xmpp')
      end
    end

    context 'unsupported protocol requested' do
      it 'reutrns with an empty protocol header' do
        @request_params = { headers: { 'Sec-WebSocket-Protocol' => 'generic' } }
        handshake << client_request

        expect(handshake.to_s).to match("Sec-WebSocket-Protocol: \r\n")
      end
    end
  end
end
