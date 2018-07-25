# frozen_string_literal: true

module WebSocket
  module Frame
    module Handler
      autoload :Base,      "#{::WebSocket::ROOT}/websocket/frame/handler/base"

      autoload :Handler03, "#{::WebSocket::ROOT}/websocket/frame/handler/handler03"
      autoload :Handler04, "#{::WebSocket::ROOT}/websocket/frame/handler/handler04"
      autoload :Handler05, "#{::WebSocket::ROOT}/websocket/frame/handler/handler05"
      autoload :Handler07, "#{::WebSocket::ROOT}/websocket/frame/handler/handler07"
      autoload :Handler75, "#{::WebSocket::ROOT}/websocket/frame/handler/handler75"
    end
  end
end
