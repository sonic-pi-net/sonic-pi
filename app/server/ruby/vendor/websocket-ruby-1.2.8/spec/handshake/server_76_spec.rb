# frozen_string_literal: true

require 'spec_helper'

RSpec.describe 'Server draft 76 handshake' do
  let(:handshake) { WebSocket::Handshake::Server.new }
  let(:version) { 76 }
  let(:client_request) { client_handshake_76(@request_params || {}) }
  let(:server_response) { server_handshake_76(@request_params || {}) }

  it_behaves_like 'all server drafts'

  it 'disallows request without spaces in key 1' do
    @request_params = { key1: '4@146546xW%0l15' }
    handshake << client_request

    expect(handshake).to be_finished
    expect(handshake).not_to be_valid
    expect(handshake.error).to be(:invalid_handshake_authentication)
  end

  it 'disallows request without spaces in key 2' do
    @request_params = { key2: '129985Y31.P00' }
    handshake << client_request

    expect(handshake).to be_finished
    expect(handshake).not_to be_valid
    expect(handshake.error).to be(:invalid_handshake_authentication)
  end

  it 'disallows request with invalid number of spaces or numbers in key 1' do
    @request_params = { key1: '4 @1   46546xW%0l 1 5' }
    handshake << client_request

    expect(handshake).to be_finished
    expect(handshake).not_to be_valid
    expect(handshake.error).to be(:invalid_handshake_authentication)
  end

  it 'disallows request with invalid number of spaces or numbers in key 2' do
    @request_params = { key2: '12998  5 Y3 1  .P00' }
    handshake << client_request

    expect(handshake).to be_finished
    expect(handshake).not_to be_valid
    expect(handshake.error).to be(:invalid_handshake_authentication)
  end

  context 'protocol header specified' do
    let(:handshake) { WebSocket::Handshake::Server.new(protocols: %w[binary]) }

    context 'supported' do
      it 'returns with the same protocol' do
        @request_params = { headers: { 'Sec-WebSocket-Protocol' => 'binary' } }
        handshake << client_request

        expect(handshake.to_s).to match('Sec-WebSocket-Protocol: binary')
      end
    end

    context 'unsupported' do
      it 'returns with an empty protocol header' do
        @request_params = { headers: { 'Sec-WebSocket-Protocol' => 'xmpp' } }
        handshake << client_request

        expect(handshake.to_s).to match("Sec-WebSocket-Protocol: \r\n")
      end
    end
  end
end
