# WebSocket Ruby

- Travis CI build: [![](https://travis-ci.org/imanel/websocket-ruby.png)](http://travis-ci.org/imanel/websocket-ruby)
- Autobahn tests: [server](http://imanel.github.com/websocket-ruby/autobahn/server/), [client](http://imanel.github.com/websocket-ruby/autobahn/client/)

Universal Ruby library to handle WebSocket protocol. It focuses on providing abstraction layer over [WebSocket API](http://dev.w3.org/html5/websockets/) instead of providing server or client functionality.

Currently WebSocket Ruby supports all existing drafts of WebSocket, which include:

- [hixie-75](http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-75)
- [hixie-76](http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-76)
- [all hybi drafts (00-13)](http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-17)
- [RFC 6455](http://datatracker.ietf.org/doc/rfc6455/)

## Installation

WebSocket Ruby has no external dependencies, so it can be installed from source or directly from rubygems:

```
gem install "websocket"
```

or via Gemfile:

```
gem "websocket"
```

## Server handshake

``` ruby
@handshake = WebSocket::Handshake::Server.new

# Parse client request
@handshake << <<EOF
GET /demo HTTP/1.1\r
Upgrade: websocket\r
Connection: Upgrade\r
Host: example.com\r
Sec-WebSocket-Origin: http://example.com\r
Sec-WebSocket-Version: 13\r
Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r
\r
EOF

# All data received?
@handshake.finished?

# No parsing errors?
@handshake.valid?

# Create response
@handshake.to_s # HTTP/1.1 101 Switching Protocols
                # Upgrade: websocket
                # Connection: Upgrade
                # Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
```

## Client handshake

``` ruby
@handshake = WebSocket::Handshake::Client.new(:url => 'ws://example.com')

# Create request
@handshake.to_s # GET /demo HTTP/1.1
                # Upgrade: websocket
                # Connection: Upgrade
                # Host: example.com
                # Sec-WebSocket-Origin: http://example.com
                # Sec-WebSocket-Version: 13
                # Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==

# Parse server response
@handshake << <<EOF
HTTP/1.1 101 Switching Protocols\r
Upgrade: websocket\r
Connection: Upgrade\r
Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r
\r
EOF

# All data received?
@handshake.finished?

# No parsing errors?
@handshake.valid?
```

## Parsing and constructing frames

``` ruby
# Prepare frame for sending
frame = WebSocket::Frame::Outgoing::Server.new(:version => @handshake.version, :data => "Hello", :type => :text)
frame.to_s # "\x81\x05\x48\x65\x6c\x6c\x6f"

# Parse incoming frames
frame = WebSocket::Frame::Incoming::Server.new(:version => @handshake.version)
frame << "\x81\x05\x48\x65\x6c\x6c\x6f\x81\x06\x77\x6f\x72\x6c\x64\x21"
frame.next # "Hello"
frame.next # "world!""
```

## Examples & Projects using WebSocket-Ruby

- [WebSocket-EventMachine-Client](https://github.com/imanel/websocket-eventmachine-client) - client based on EventMachine
- [WebSocket-EventMachine-Server](https://github.com/imanel/websocket-eventmachine-server) - server based on EventMachine (drop-in replacement for EM-WebSocket)
- [Selenium-WebDriver](https://rubygems.org/gems/selenium-webdriver) - tool for writing automated tests of websites
- [Rubame](https://github.com/saward/Rubame) - websocket game server

## Native extension

WebSocket gem is written in pure Ruby, without any dependencies or native extensions and still is one of the fastest implementations of WebSocket API. However, if you want to increase it's speed even further, I created additional gem with dedicated native extensions for both C and Java. Those extensions provide 20-30% increase in speed(more accurate comparison can be found in autobahn test results)

In order to use native extension just install [websocket-native](http://github.com/imanel/websocket-ruby-native) gem or add it to Gemfile - WebSocket will automatically detect and load it.

## License

(The MIT License)

Copyright © 2012 Bernard Potocki

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the ‘Software’), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED ‘AS IS’, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
