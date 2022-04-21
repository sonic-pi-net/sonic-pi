# Tomlrb

[![Code Climate](https://codeclimate.com/github/fbernier/tomlrb/badges/gpa.svg)](https://codeclimate.com/github/fbernier/tomlrb) [![Build Status](https://travis-ci.org/fbernier/tomlrb.svg)](https://travis-ci.org/fbernier/tomlrb) [![Gem Version](https://badge.fury.io/rb/tomlrb.svg)](http://badge.fury.io/rb/tomlrb)

A Racc based [TOML](https://github.com/toml-lang/toml) Ruby parser supporting the 1.0.0 version of the spec.


## TODO

* Dumper

## Installation

Add this line to your application's Gemfile:

```ruby
gem 'tomlrb'
```

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install tomlrb

## Usage

```ruby
Tomlrb.parse("[toml]\na = [\"array\", 123]")
```

or

```ruby
Tomlrb.load_file('my_file', symbolize_keys: true)
```

## Benchmark

You can run the benchmark against the only other v0.5.0 compliant parser to my knowledge with `ruby benchmarks/bench.rb`.

Here are the results on my machine:

```
Warming up --------------------------------------
      emancu/toml-rb     1.000  i/100ms
     fbernier/tomlrb    17.000  i/100ms
Calculating -------------------------------------
      emancu/toml-rb     10.630  (±28.2%) i/s -     49.000  in   5.035997s
     fbernier/tomlrb    181.585  (±24.8%) i/s -    850.000  in   5.019273s

Comparison:
     fbernier/tomlrb:      181.6 i/s
      emancu/toml-rb:       10.6 i/s - 17.08x  (± 0.00) slower
```

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `bin/console` for an interactive prompt that will allow you to experiment.

Do not forget to regenerate the parser when you modify rules in the `parser.y` file using `rake compile`.

Run the tests using `rake test`.

To install this gem onto your local machine, run `bundle exec rake install`. To release a new version, update the version number in `version.rb`, and then run `bundle exec rake release` to create a git tag for the version, push git commits and tags, and push the `.gem` file to [rubygems.org](https://rubygems.org).

## Contributing

1. Fork it ( https://github.com/[my-github-username]/tomlrb/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

## Thanks

Thanks to [@jpbougie](https://github.com/jpbougie) for the crash course on  the Chomsky hierarchy and general tips.
