# FastOsc

A Ruby wrapper around [rtosc](https://github.com/fundamental/rtosc/) to encode and decode OSC messages.

This also includes a fallback implementation in pure Ruby in the case that the compiled version doesn't load properly. This can be forced by setting an environment variable of `FAST_OSC_USE_FALLBACK=1` where needed.

## Installation

Add this line to your application's Gemfile:

    gem 'fast_osc'

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install fast_osc

## Usage

Everything (single messages and bundles) can be parsed with a single method -
`FastOsc.decode`. This outputs an array of bundles, with each bundle containing
a timestamp and an array of messages. Each message contains a path and,
optionally, some arguments.

```ruby
# encode an OSC message with the established Ruby OSC libary
@msg = OSC::Message.new("/testpath", ["some", "args", 1, 2.0]).encode
@encoded_msg = @msg1.encode

FastOsc.decode(@encoded_msg0).each do |bundle|
  _timestamp, osc_msgs = bundle

  osc_msgs.each do |(path, args)|
    puts @path # "/testpath"
    puts args # ["some", "args", 1, 2.0]
  end
end
```

```
>> FastOsc.encode_single_message("/foo", ["baz", 1, 2.0])
=> "/foo\x00\x00\x00\x00,sif\x00\x00\x00\x00baz\x00\x00\x00\x00\x01@\x00\x00\x00"
>> res = _
>> FastOsc.decode_no_bundles(res)
=> ["/foo", ["baz", 1, 2.0]]
>> FastOsc.encode_single_bundle(Time.now.to_i, "/foo", ["baz", 1, 2.0])
=> "#bundle\x00\x00\x00\x00\x00W*1\x7F\x00\x00\x00\x1C/foo\x00\x00\x00\x00,sif\x00\x00\x00\x00baz\x00\x00\x00\x00\x01@\x00\x00\x00"
```

 A timestamp of `nil` is a special case meaning "immediately".

### A note on types

To represent the `blob` type tag from the OSC spec, FastOsc uses strings with
the `ASCII-8BIT` encoding. UTF-8 strings remain as normal string tags.

For examples of this, see the test suite.

## Is it fast?

Let's see...

Key:

* `fast_osc` - this gem
* `osc` - [`osc-ruby`](https://github.com/aberant/osc-ruby)
* `samsosc` - `OSC` classes from Sonic Pi (which are optimised pure Ruby based on `pack` and `unpack`)

### Encoding Benchmark

```
$ WITH_BENCHMARKS=1 rake test
Warming up --------------------------------------
            fast_osc    94.043k i/100ms
             samsosc    41.231k i/100ms
            osc-ruby    17.476k i/100ms
Calculating -------------------------------------
            fast_osc      1.186M (± 3.7%) i/s -      5.925M in   5.004014s
             samsosc    458.561k (± 4.1%) i/s -      2.309M in   5.043860s
            osc-ruby    182.051k (± 4.6%) i/s -    908.752k in   5.003313s
DECODING TEST
```

### Decoding Bencmark

```
$ WITH_BENCHMARKS=1 rake test
Warming up --------------------------------------
            fast_osc   208.209k i/100ms
             samsosc    38.760k i/100ms
            osc-ruby     6.844k i/100ms
Calculating -------------------------------------
            fast_osc      3.679M (± 3.8%) i/s -     18.531M in   5.044888s
             samsosc    430.488k (± 3.0%) i/s -      2.171M in   5.046837s
            osc-ruby     70.998k (± 3.1%) i/s -    355.888k in   5.017493s
```

Benchmarks are now part of this repo - run `WITH_BENCHMARKS=1 rake test` to see the results for yourself.

## Still todo

- [x] Make a pure ruby fallback available
- [x] Implement more types
- [x] Bring benchmarks into the repo
- [ ] Work out cross compilation story for easier packaging
- [x] Implement multi message/nested bundles
- [ ] More documentation
- [x] Travis, Appveyor

## Development notes

This project uses Bundler v2 - get this with

    gem install bundler

On linux, the only deps are `apt-get install build-essentials ruby-devel`. On OS X you may need XCode build tooling or similar.

    bundle install
    rake compile

On Windows, using RubyInstaller and setup the MSYS2 toolchain. You then need to include devkit in the compile step like so:

    bundle install
    bundle exec rake compile -rdevkit

### Running the test suite

```
$ gem install minitest # or bundle install
$ rake clean && rake clobber && rake compile && rake test && FAST_OSC_USE_FALLBACK=true rake test
```

https://gist.github.com/xavriley/507eff0a75d4552fa56e

## Contributing

1. Fork it ( http://github.com/<my-github-username>/fast_osc/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
