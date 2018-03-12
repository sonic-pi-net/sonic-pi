A pure Ruby gem for reading and writing sound files in Wave format (*.wav).

You can use this gem to create Ruby programs that work with audio, such as a [command-line drum machine](http://beatsdrummachine.com). Since it is written in pure Ruby (as opposed to wrapping an existing C library), you can use it without having to compile a separate extension.

For more info, check out the website: <http://wavefilegem.com/>

# Example Usage

This is a short example that shows how to append three separate Wave files into a single file:

```ruby
require 'wavefile'
include WaveFile

FILES_TO_APPEND = ["file1.wav", "file2.wav", "file3.wav"]

Writer.new("append.wav", Format.new(:stereo, :pcm_16, 44100)) do |writer|
  FILES_TO_APPEND.each do |file_name|
    Reader.new(file_name).each_buffer do |buffer|
      writer.write(buffer)
    end
  end
end
```

More examples can be found at <http://wavefilegem.com/examples>.


# Features

This gem lets you read and write audio data! You can use it to create Ruby programs that work with sound.

* Read and write Wave files with any number of channels, in PCM (8/16/24/32 bits per sample) or IEEE Floating Point (32/64 bits per sample) format.
* Seamlessly convert between sample formats. Read sample data from a file into any format supported by this gem, regardless of how the sample data is stored in the actual file. Or, create sample data in one format (such as floats between -1.0 and 1.0), but write it to a file in a different format (such as 16-bit PCM).
* Automatic file management, similar to how `IO.open` works. That is, you can open a file for reading or writing, and if a block is given, the file will automatically be closed when the block exits.
* Query metadata about Wave files (sample rate, number of channels, number of sample frames, etc.), including files that are in a format this gem can't read or write.
* Written in pure Ruby, so it's easy to include in your program. There's no need to compile a separate extension in order to use it.


# Current Release: v0.8.1

Released on January 31, 2017, this version fixes an error when frozen string literals are enabled in Ruby 2.3 or higher. (At the time of this release, that means Ruby 2.3 or 2.4). The gem should now work properly when the `--enable-frozen-string-literal` Ruby option is enabled. Thanks to [@samaaron](https://github.com/samaaron) for finding and fixing this!

# Release v0.8.0

Released on January 29, 2017, this version includes these changes:

* Wave files using WAVEFORMATEXTENSIBLE format (format code 65534) can now be read.
  * Notes/Limitations
    * The same formats supported in "vanilla" Wave files are supported when reading WAVEFORMATEXTENSIBLE files. That is, PCM (8/16/24/32 bits per sample) or IEEE_FLOAT (32/64 bits per sample).
    * The channel speaker mapping field is not exposed.
    * The number of valid bits per sample must match the sample container size. For example, if a file has a sample container size of 24 bits and each sample is 24 bits, then it can be read. If the container size is 32 bits and each sample is 24 bits, it _can't_ be read.
    * Writing files using WAVEFORMATEXTENSIBLE format is not supported - all files will be written as a "vanilla" file regardless of the number of channels or sample format.
* `Reader` and `Writer` can now be constructed using an open `IO` instance, to allow reading/writing using an arbitrary `IO`-like object (`File`, `StringIO`, etc). Previously, they could only be constructed from a file name (given by a String). Thanks to [@taf2](https://github.com/taf2) for suggesting this feature and providing an example pull request.
* The buffer size in `Reader.each_buffer()` is now optional. If not given, a default buffer size will be used.
* Two `Duration` objects will now evaluate to equal if they represent the same amount of time, due to an overridden definition of `==`. Thanks to [Christopher Smith](https://github.com/chrylis) for suggesting this improvement.
* A `ReaderClosedError` is now raised (instead of `IOError`) when attempting to read from a closed `Reader` instance. However, `ReaderClosedError` extends `IOError`.
* A `WriterClosedError` is now raised (instead of `IOError`) when attempting to read from a closed `Writer` instance. However, `ReaderClosedError` extends `IOError`.
* **Backwards Incompatible Changes**
  * `Reader.file_name` and `Writer.file_name` have been removed. When a `Reader` or `Writer` instance is constructed from an `IO` instance, this field wouldn't necessarily have a sensible value. Since I don't know of an obvious use-case for these fields, going ahead and removing them altogether.
  * The long deprecated ability to provide the sample format for a `Format` instance as an integer (implying PCM format) has been removed. For example, this is no longer valid: `Format.new(:mono, 16, 44100)`. Instead, use `Format.new(:mono, :pcm_16, 44100)`.


# Compatibility

WaveFile has been tested with these Ruby versions, and appears to be compatible with them:

* MRI 2.4.0, 2.3.3, 2.2.6, 2.1.10, 2.0, 1.9.3

1.9.3 is the minimum supported Ruby version.

If you find any compatibility issues, please let me know by opening a GitHub issue.


# Installation

First, install the WaveFile gem from rubygems.org:

    gem install wavefile

...and include it in your Ruby program:

    require 'wavefile'

Note that if you're installing the gem into the default Ruby that comes pre-installed on MacOS (as opposed to a Ruby installed via [RVM](http://rvm.io/) or [rbenv](https://github.com/sstephenson/rbenv/)), you should used `sudo gem install wavefile`. Otherwise you might run into a file permission error.


# Dependencies

WaveFile has no external dependencies when used as a gem. It is written in pure Ruby, and is entirely self-contained.

However, it does have dependencies for local development, in order to run the tests. See below in section "Local Development".


# Local Development

## Running the Tests

First, install the required development/test dependencies:

    bundle install

Then, to run the tests:

    bundle exec rake test

## Generating test fixtures

If you want to change one of the fixture `*.wav` files under `/test/fixtures`, edit the appropriate `*.yml` file defined in `/tools`, and then run this:

    rake test:create_fixtures

## Generating RDoc Documentation

    rake rdoc


# Contributing

1. Fork my repo
2. Create a branch for your changes
3. Add your changes, and please include tests
4. Make sure the tests pass by running `rake test`
5. Create a pull request
