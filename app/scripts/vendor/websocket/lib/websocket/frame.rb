module WebSocket
  module Frame

    autoload :Base,     "#{::WebSocket::ROOT}/websocket/frame/base"
    autoload :Data,     "#{::WebSocket::ROOT}/websocket/frame/data"
    autoload :Handler,  "#{::WebSocket::ROOT}/websocket/frame/handler"
    autoload :Incoming, "#{::WebSocket::ROOT}/websocket/frame/incoming"
    autoload :Outgoing, "#{::WebSocket::ROOT}/websocket/frame/outgoing"

  end
end
