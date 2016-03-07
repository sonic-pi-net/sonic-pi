A pure Ruby gem for reading and writing sound files in Wave format (*.wav).

You can use this gem to create Ruby programs that work with audio, such as [drum machine](http://beatsdrummachine.com). Since it is written in pure Ruby (as opposed to wrapping an existing C library), you can use it without having to compile a separate extension.

For more info, check out the website: <http://wavefilegem.com/>

# Example Usage

This is a short example that shows how to append three separate Wave files into a single file:

    require 'wavefile'
    include WaveFile
    
    FILES_TO_APPEND = ["file1.wav", "file2.wav", "file3.wav"]
    SAMPLES_PER_BUFFER = 4096

    Writer.new("append.wav", Format.new(:stereo, :pcm_16, 44100)) do |writer|
      FILES_TO_APPEND.each do |file_name|
        Reader.new(file_name).each_buffer(SAMPLES_PER_BUFFER) do |buffer|
          writer.write(buffer)
        end
      end
    end

More examples can be [found on the wiki](https://github.com/jstrait/wavefile/wiki).


# Features

* Ability to read and write Wave files with any number of channels in the following formats:
  * PCM (8, 16, 24, and 32 bits per sample)
  * Floating Point (32 and 64 bits per sample)
* Ability to read sample data from a file in any of the supported formats, regardless of the file's actual sample format

            # Sample data will be returned as 32-bit floating point samples,
            # regardless of the actual sample format in the file.
            Reader.new("some_file.wav", Format.new(:mono, :float_32, 44100))

* Automatic file management, similar to how `IO.open` works. That is, you can open a file for reading or writing, and if a block is given, the file will automatically be closed when the block exits.

        Writer.new("some_file.wav", Format.new(:mono, :pcm_16, 44100)) do |writer|
          # write some sample data
        end
        # At this point, the writer will automatically be closed, no need to do it manually

* Ability to query metadata about Wave files (sample rate, number of channels, number of sample frames, etc.), including files that are in a format this gem can't read or write.
* Pure Ruby, so no need to compile a separate extension in order to use it.


# Current Release: v0.7.0

Released on March 6, 2016, this version includes these changes:

* The minimum supported Ruby version is now 1.9.3 - earlier versions are no longer supported.
* New method: `Reader.native_format`. Returns a `Format` instance with information about the underlaying format of the Wave file being read, which is not necessarily the same format the sample data is being converted to as it's being read.
* `Reader.info()` has been removed. Instead, construct a new `Reader` instance and use `Reader.native_format()` - this will return a `Format` instance with the same info that would have been returned by `Reader.info()`.
* Similarly, the `Info` class has been removed, due to `Reader.info()` being removed.
* Constructing a `Reader` instance will no longer raise an exception if the file is valid Wave file, but in a format unsupported by this gem. The purpose of this is to allow calling `Reader.native_format()` on this instance, to get format information for files not supported by this gem.
* New method: `Reader.readable_format?` returns true if the file is a valid format that the gem can read, false otherwise.
* `Reader.read()` and `Reader.each_buffer()` will now raise an exception if the file is a valid Wave file, but not a format that the gem can read. Or put differently, if `Reader.readable_format?` returns `false`, any subsequent calls to `Reader.read()` or `Reader.each_buffer()` will raise an exception.
* Some constants have been made private since they are intended for internal use.
* Bug fix: Files will now be read/written correctly on big-endian platforms. Or in other words, sample data is always read as little-endian, regardless of the native endianness of the platform.


# Compatibility

WaveFile has been tested with these Ruby versions, and appears to be compatible with them:

* MRI 2.3, 2.2, 2.1, 2.0, 1.9.3

1.9.3 is the minimum supported Ruby version.

If you find any compatibility issues, please let me know by opening a GitHub issue.


# Dependencies

WaveFile has no external dependencies. It is written in pure Ruby, and is entirely self-contained.


# Installation

First, install the WaveFile gem from rubygems.org:

    gem install wavefile

...and include it in your Ruby program:

    require 'wavefile'

Note that if you're installing the gem into the default Ruby that comes pre-installed on MacOS (as opposed to a Ruby installed via [RVM](http://rvm.io/) or [rbenv](https://github.com/sstephenson/rbenv/)), you should used `sudo gem install wavefile`. Otherwise you might run into a file permission error.


# Contributing

1. Fork my repo
2. Create a branch for your changes
3. Add your changes, and please include tests
4. Make sure the tests pass by running `rake test`
5. Create a pull request
