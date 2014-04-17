module WebSocket
  module Handshake
    module Handler

      autoload :Base,     "#{::WebSocket::ROOT}/websocket/handshake/handler/base"

      autoload :Client,   "#{::WebSocket::ROOT}/websocket/handshake/handler/client"
      autoload :Client01, "#{::WebSocket::ROOT}/websocket/handshake/handler/client01"
      autoload :Client04, "#{::WebSocket::ROOT}/websocket/handshake/handler/client04"
      autoload :Client75, "#{::WebSocket::ROOT}/websocket/handshake/handler/client75"
      autoload :Client76, "#{::WebSocket::ROOT}/websocket/handshake/handler/client76"

      autoload :Server,   "#{::WebSocket::ROOT}/websocket/handshake/handler/server"
      autoload :Server04, "#{::WebSocket::ROOT}/websocket/handshake/handler/server04"
      autoload :Server75, "#{::WebSocket::ROOT}/websocket/handshake/handler/server75"
      autoload :Server76, "#{::WebSocket::ROOT}/websocket/handshake/handler/server76"

    end
  end
end
