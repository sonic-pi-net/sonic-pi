require 'rspec'
require 'webrick'

require 'websocket'
Dir["#{File.dirname(__FILE__)}/support/**/*.rb"].each {|f| require f}

RSpec.configure do |config|
  config.before(:suite) do
    WebSocket.max_frame_size = 100 * 1024 # 100kb
  end
end
