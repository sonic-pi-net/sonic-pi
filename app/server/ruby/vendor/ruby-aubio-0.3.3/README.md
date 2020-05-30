# Aubio
## warning: pre-alpha subject to change

A Ruby binding for the `aubio` library.

# What is aubio?

In their own words...

> aubio is a tool designed for the extraction of annotations from audio signals. Its features include segmenting a sound file before each of its attacks, performing pitch detection, tapping the beat and producing midi streams from live audio.
- http://aubio.org/

## Prerequisites

`brew install aubio --with-libsndfile --with-fftw --with-libsamplerate`

## Using `ffi_gen` to autogenerate bindings

```
brew install aubio --with-libsndfile --with-fftw --with-libsamplerate
brew install llvm --with-clang --enable-shared
# clone this repo and cd into the root folder, then run
LD_LIBRARY_PATH="/usr/local/opt/llvm35/lib/llvm-3.5/lib" ruby aubio-ffi-generator.rb
```

## Installation

Add this line to your application's Gemfile:

```ruby
gem 'aubio'
```

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install aubio

## Usage

`Aubio` just needs a path to a sound file:

```
my_file = Aubio.open("/path/to/file")
```

From there you can access the following:

```
my_file.onsets # list of extracted onsets
my_file.pitches # list of extracted pitches
my_file.beats # where one would tap their foot
# NOT YET IMPLEMENTED # my_file.notes # list of onsets with pitches
# NOT YET IMPLEMENTED # my_file.silences # list of silent regions
# NOT YET IMPLEMENTED # my_file.mel_frequency_cepstral_coefficients # list of mfccs
```

All of these are Ruby `Enumerator`s which means they'll respond to `.next`, and so on. If you want the whole list all at once call `.to_a` on it.

### Avoiding memory leaks

The underlying C library allocates some memory so to clean this up you'll need to run

    my_file.close

to free it up.

## Data format

Each "event" that `aubio` describes is represented as a `Hash` like so:

```
my_file.onsets.first #=> {s: 0.0, ms: 0.0, start: 1, end: 0}
```

`s` and `ms` refer to seconds and milliseconds respectively.

`start: 1` and `end: 1` are special events that describe the start and the end of the audio file respectively. Whilst the `start` event at `0.0s` is usually an onset, the `end` is a convenience added by the Ruby wrapper to allow for easier slicing of sound files into samples.

### Still to implement

`relative` is the point at which the event occurs relative to the overall length of the original sound file, scaled between `0` and `1` (i.e. `relative: 0.5` is exactly halfway through).

`confidence` is a score returned from `aubio` which may be useful for threshold type operations.

## Optional params

The `Aubio#open` method can take a list of optional params like so:

```
:sample_rate
# Fetch the input source, resampled at the given sampling rate. The rate should be specified in Hertz as an integer. If 0, the sampling rate of the original source will be used. Defaults to 0.
:bufsize
The size of the buffer to analyze, that is the length of the window used for spectral and temporal computations. Defaults to 512.
:hopsize
```

e.g.

```
Aubio.open("/path/to/audio/file", sample_rate: 44100)
```

## Bugs / Still to do

* better tests
* use `Onsets` class as a basis to implement the other functions
* improve accuracy of bpm - seems to be consistently too fast on things I've tried
* look into streaming results for live inputs

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake test` to run the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

To install this gem onto your local machine, run `bundle exec rake install`. To release a new version, update the version number in `version.rb`, and then run `bundle exec rake release`, which will create a git tag for the version, push git commits and tags, and push the `.gem` file to [rubygems.org](https://rubygems.org).

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/xavriley/aubio. This project is intended to be a safe, welcoming space for collaboration, and contributors are expected to adhere to the [Contributor Covenant](http://contributor-covenant.org) code of conduct.


## License

The gem is available as open source under the terms of the [MIT](http://opensource.org/licenses/MIT).

