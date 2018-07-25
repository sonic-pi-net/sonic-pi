# Changelog

## 1.2.8

- restore support for Ruby 2.0+

## 1.2.7

- fix bug in previous version for Ruby 2.3

## 1.2.6

- duplicate variables passed in initializers to avoid changing them

## 1.2.5

- make handshake server resilient to non-string Rack env keys

## 1.2.4

- add subprotocol handling for both server and client

## 1.2.3

- fix for draft 76 when challenge might sometimes fail
- multiple small optimizations

## 1.2.2

- fix handshake for draft 11+ sending Sec-WebSocket-Origin instead of Origin

## 1.2.1

- fix error for draft 76 when leftovers are empty

## 1.2.0

- Remove support for Ruby 1.8
- Add support for sending custom headers for Client
- Better detection and handling of draft 76
- Multiple small fixes and optimizations

## 1.1.4

- verify valid close codes according to spec
- return error on invalid UTF-8 payload
- expose error message

## 1.1.3

- fix close code support

## 1.1.2

- fix support for rack input that is blocking (i.e. Passenger)

## 1.1.1

- fix handling close code for frames version 5+

## 1.1.0

- allow raising ruby errors instead of setting `error` flag
- allow access to handshake headers
- add from_rack method
- add from_hash method
- stop extending handlers - it should improve performance for opening connection

## 1.0.7

- fix requiring url under Ruby 1.9.1
- support for Ruby 2.0.0

## 1.0.6

- support text frame types instead of only symbol ones
- support for sending masked frames

## 1.0.5

- add support for close codes

## 1.0.4

- nicer inspect - handful during debugging

## 1.0.3

- improve pure ruby implementation performance by ~30%
- add support for native extension

## 1.0.2

- allow configuration of max frame size via WebSocket.max_frame_size option
- much better documentation
- remove handler-specific methods from public list
- refactor code for easier use
- make parsers return more consistent values
- fix server handshake #to_s when no version was found
- add #uri to server handshake

## 1.0.1

- allow creating client with :uri and :url options
- prevent strange results when header is mailformed
- set client path to '/' when :uri option is provided but without trailing slash

## 1.0.0

- initial release
