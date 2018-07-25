# frozen_string_literal: true

# WebSocket protocol implementation in Ruby
# This module does not provide a WebSocket server or client, but is made for using
# in http servers or clients to provide WebSocket support.
# @author Bernard "Imanel" Potocki
# @see http://github.com/imanel/websocket-ruby main repository
module WebSocket
  # Default WebSocket version to use
  DEFAULT_VERSION = 13
  ROOT = __dir__

  autoload :Error,            "#{ROOT}/websocket/error"
  autoload :ExceptionHandler, "#{ROOT}/websocket/exception_handler"
  autoload :Frame,            "#{ROOT}/websocket/frame"
  autoload :Handshake,        "#{ROOT}/websocket/handshake"
  autoload :NiceInspect,      "#{ROOT}/websocket/nice_inspect"

  # Limit of frame size payload in bytes
  def self.max_frame_size
    @max_frame_size ||= 20 * 1024 * 1024 # 20MB
  end

  # Set limit of frame size payload in bytes
  def self.max_frame_size=(val)
    @max_frame_size = val
  end

  # If set to true error will be raised instead of setting `error` method.
  # All errors inherit from WebSocket::Error.
  def self.should_raise
    @should_raise ||= false
  end

  # Should protocol errors raise ruby errors? If false then `error` flag is set instead.
  def self.should_raise=(val)
    @should_raise = val
  end
end

# Try loading websocket-native if available
begin
  require 'websocket-native'
rescue LoadError => e
  raise unless e.message =~ /websocket-native/
end
