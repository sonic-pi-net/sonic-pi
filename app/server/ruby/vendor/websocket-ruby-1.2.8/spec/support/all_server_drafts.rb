# frozen_string_literal: true

require 'webrick'

RSpec.shared_examples_for 'all server drafts' do
  def validate_request
    handshake << client_request

    expect(handshake.error).to be_nil
    expect(handshake).to be_finished
    expect(handshake).to be_valid
    expect(handshake.to_s).to eql(server_response)
  end

  it 'is valid' do
    handshake << client_request

    expect(handshake.error).to be_nil
    expect(handshake).to be_finished
    expect(handshake).to be_valid
  end

  it 'returns valid version' do
    handshake << client_request

    expect(handshake.version).to eql(version)
  end

  it 'returns valid host' do
    @request_params = { host: 'www.test.cc' }
    handshake << client_request

    expect(handshake.host).to eql('www.test.cc')
  end

  it 'returns valid path' do
    @request_params = { path: '/custom' }
    handshake << client_request

    expect(handshake.path).to eql('/custom')
  end

  it 'returns valid query' do
    @request_params = { path: '/custom?aaa=bbb' }
    handshake << client_request

    expect(handshake.query).to eql('aaa=bbb')
  end

  it 'returns valid port' do
    @request_params = { port: 123 }
    handshake << client_request

    expect(handshake.port).to eql('123')
  end

  it 'returns valid response' do
    validate_request
  end

  it 'allows custom path' do
    @request_params = { path: '/custom' }
    validate_request
  end

  it 'allows query in path' do
    @request_params = { path: '/custom?test=true' }
    validate_request
  end

  it 'allows custom port' do
    @request_params = { port: 123 }
    validate_request
  end

  it 'recognizes unfinished requests' do
    handshake << client_request[0..-10]

    expect(handshake).not_to be_finished
    expect(handshake).not_to be_valid
  end

  it 'disallows requests with invalid request method' do
    handshake << client_request.gsub('GET', 'POST')

    expect(handshake).to be_finished
    expect(handshake).not_to be_valid
    expect(handshake.error).to be(:get_request_required)
  end

  it 'parses a rack request' do
    request = WEBrick::HTTPRequest.new(ServerSoftware: 'rspec')
    expect(request.parse(StringIO.new(client_request))).to be true
    rest = client_request.slice((request.to_s.length..-1))

    handshake.from_rack(request.meta_vars.merge(
                          'rack.input' => StringIO.new(rest),
                          :random_key => :random_value
    ))
    validate_request
  end

  it 'parses a hash request' do
    request = WEBrick::HTTPRequest.new(ServerSoftware: 'rspec')
    expect(request.parse(StringIO.new(client_request))).to be true
    body = client_request.slice((request.to_s.length..-1))

    path = request.path
    query = request.query_string
    headers = request.header.each_with_object({}) do |header, hash|
      hash[header[0]] = header[1].first if header[0] && header[1]
    end

    handshake.from_hash(headers: headers,
                        path: path,
                        query: query,
                        body: body)

    validate_request
  end
end
