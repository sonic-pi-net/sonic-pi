# FastOsc

`WARNING - Work in progress. Probably not safe for production yet`

A Ruby wrapper around [rtosc](https://github.com/fundamental/rtosc/) to encode and decode OSC messages.

This also includes a fallback implementation in pure Ruby in the case that the compiled version doesn't load properly. This can be forced by setting an environment variable of `FAST_OSC_USE_FALLBACK=1` where needed.

## Installation

Add this line to your application's Gemfile:

    gem 'fast_osc'

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install fast_osc

## Is it fast?

Let's see...

Key:

* `fast_osc` - this gem
* `osc` - [`osc-ruby`](https://github.com/aberant/osc-ruby)
* `samsosc` - `OSC` classes from Sonic Pi (which are optimised pure Ruby based on `pack` and `unpack`)

### Encoding Benchmark

```
Warming up --------------------------------------
            fast_osc    64.995k i/100ms
             samsosc    23.371k i/100ms
            osc-ruby     7.691k i/100ms
Calculating -------------------------------------
            fast_osc    797.673k (±15.0%) i/s -      3.900M in   5.043770s
             samsosc    258.331k (±12.8%) i/s -      1.285M in   5.063755s
            osc-ruby     83.203k (±12.6%) i/s -    415.314k in   5.073578s
```

## Decoding Bencmark

```
Warming up --------------------------------------
            fast_osc   102.344k i/100ms
             samsosc    20.770k i/100ms
            osc-ruby     3.145k i/100ms
Calculating -------------------------------------
            fast_osc      1.650M (±14.5%) i/s -      8.085M in   5.017162s
             samsosc    234.951k (±14.0%) i/s -      1.163M in   5.049167s
            osc-ruby     34.266k (±13.3%) i/s -    169.830k in   5.048509s
```

Benchmarks are now part of this repo - run `rake test` to see the results for yourself.

## Usage

```
>> FastOsc.encode_single_message("/foo", ["baz", 1, 2.0])
=> "/foo\x00\x00\x00\x00,sif\x00\x00\x00\x00baz\x00\x00\x00\x00\x01@\x00\x00\x00"
>> res = _
>> FastOsc.decode_single_message(res)
=> ["/foo", ["baz", 1, 2.0]]
>> FastOsc.encode_single_bundle(Time.now.to_i, "/foo", ["baz", 1, 2.0])
=> "#bundle\x00\x00\x00\x00\x00W*1\x7F\x00\x00\x00\x1C/foo\x00\x00\x00\x00,sif\x00\x00\x00\x00baz\x00\x00\x00\x00\x01@\x00\x00\x00"
```

See the test suite for additional methods regarding bundles with timestamps. Bundles are only supported with a single message at present. A timestamp of `nil` is a special case meaning "immediately".

## Running the test suite

```
$ gem install minitest # or bundle install
$ rake clean && rake clobber && rake compile && rake test
```

## Still todo

-[x] Implement more types
-[x] Bring benchmarks into the repo
-[ ] Work out cross compilation story for easier packaging
-[ ] Implement multi message/nested bundles
-[ ] Documentation
-[ ] Travis

## Development notes

    bundle install
    rake compile

https://gist.github.com/xavriley/507eff0a75d4552fa56e

## Contributing

1. Fork it ( http://github.com/<my-github-username>/fast_osc/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
