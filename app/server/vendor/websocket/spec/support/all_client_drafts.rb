shared_examples_for 'all client drafts' do
  def validate_request
    handshake.to_s.should eql(client_request)

    handshake << server_response

    handshake.error.should be_nil
    handshake.should be_finished
    handshake.should be_valid
  end

  it "should be valid" do
    handshake << server_response

    handshake.error.should be_nil
    handshake.should be_finished
    handshake.should be_valid
  end

  it "should return valid version" do
    handshake.version.should eql(version)
  end

  it "should return valid host" do
    @request_params = { :host => "www.test.cc" }
    handshake.host.should eql('www.test.cc')
  end

  it "should return valid path" do
    @request_params = { :path => "/custom" }
    handshake.path.should eql('/custom')
  end

  it "should return valid query" do
    @request_params = { :query => "aaa=bbb" }
    handshake.query.should eql("aaa=bbb")
  end

  it "should return valid port" do
    @request_params = { :port => 123 }
    handshake.port.should eql(123)
  end

  it "should parse uri" do
    @request_params = { :uri => "ws://test.example.org:301/test_path?query=true" }
    handshake.host.should eql('test.example.org')
    handshake.port.should eql(301)
    handshake.path.should eql('/test_path')
    handshake.query.should eql('query=true')
  end

  it "should parse url" do
    @request_params = { :url => "ws://test.example.org:301/test_path?query=true" }
    handshake.host.should eql('test.example.org')
    handshake.port.should eql(301)
    handshake.path.should eql('/test_path')
    handshake.query.should eql('query=true')
  end

  it "should resolve correct path with root server provided" do
    @request_params = { :url => "ws://test.example.org" }
    handshake.path.should eql('/')
  end

  it "should return valid response" do
    validate_request
  end

  it "should allow custom path" do
    @request_params = { :path => "/custom" }
    validate_request
  end

  it "should allow query in path" do
    @request_params = { :query => "test=true" }
    validate_request
  end

  it "should allow custom port" do
    @request_params = { :port => 123 }
    validate_request
  end

  it "should recognize unfinished requests" do
    handshake << server_response[0..-20]

    handshake.should_not be_finished
    handshake.should_not be_valid
  end

  it "should disallow requests with invalid request method" do
    handshake << server_response.gsub('101', '404')

    handshake.should be_finished
    handshake.should_not be_valid
    handshake.error.should eql(:invalid_status_code)
  end
end
