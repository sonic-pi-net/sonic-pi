# frozen_string_literal: true

RSpec.shared_examples_for 'all client drafts' do
  def validate_request
    expect(handshake.to_s).to eql(client_request)

    handshake << server_response

    expect(handshake.error).to be_nil
    expect(handshake).to be_finished
    expect(handshake).to be_valid
  end

  it 'is valid' do
    handshake << server_response

    expect(handshake.error).to be_nil
    expect(handshake).to be_finished
    expect(handshake).to be_valid
  end

  it 'returns valid version' do
    expect(handshake.version).to eql(version)
  end

  it 'returns valid host' do
    @request_params = { host: 'www.test.cc' }
    expect(handshake.host).to eql('www.test.cc')
  end

  it 'returns valid path' do
    @request_params = { path: '/custom' }
    expect(handshake.path).to eql('/custom')
  end

  it 'returns valid query' do
    @request_params = { query: 'aaa=bbb' }
    expect(handshake.query).to eql('aaa=bbb')
  end

  it 'returns valid port' do
    @request_params = { port: 123 }
    expect(handshake.port).to be(123)
  end

  it 'returns valid headers' do
    @request_params = { headers: { 'aaa' => 'bbb' } }
    expect(handshake.headers).to eql('aaa' => 'bbb')
  end

  it 'parses uri' do
    @request_params = { uri: 'ws://test.example.org:301/test_path?query=true' }
    expect(handshake.host).to eql('test.example.org')
    expect(handshake.port).to be(301)
    expect(handshake.path).to eql('/test_path')
    expect(handshake.query).to eql('query=true')
  end

  it 'parses url' do
    @request_params = { url: 'ws://test.example.org:301/test_path?query=true' }
    expect(handshake.host).to eql('test.example.org')
    expect(handshake.port).to be(301)
    expect(handshake.path).to eql('/test_path')
    expect(handshake.query).to eql('query=true')
  end

  it 'resolves correct path with root server provided' do
    @request_params = { url: 'ws://test.example.org' }
    expect(handshake.path).to eql('/')
  end

  it 'returns valid response' do
    validate_request
  end

  it 'allows custom path' do
    @request_params = { path: '/custom' }
    validate_request
  end

  it 'allows query in path' do
    @request_params = { query: 'test=true' }
    validate_request
  end

  it 'allows custom port' do
    @request_params = { port: 123 }
    validate_request
  end

  it 'allows custom headers' do
    @request_params = { headers: { 'aaa' => 'bbb' } }
    validate_request
  end

  it 'recognizes unfinished requests' do
    handshake << server_response[0..-20]

    expect(handshake).not_to be_finished
    expect(handshake).not_to be_valid
  end

  it 'disallows requests with invalid request method' do
    handshake << server_response.gsub('101', '404')

    expect(handshake).to be_finished
    expect(handshake).not_to be_valid
    expect(handshake.error).to be(:invalid_status_code)
  end
end
