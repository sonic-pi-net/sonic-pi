# Changelog

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
