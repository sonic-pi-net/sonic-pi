A pure Ruby gem for reading and writing sound files in Wave format (*.wav).

You can use this gem to create Ruby programs that produce audio, such as [drum machine](http://beatsdrummachine.com). Since it is written in pure Ruby (as opposed to wrapping an existing C library), you can use it without having to compile a separate extension.

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


# Current Release: v0.6.0

This release includes these improvements:

* Support for reading and writing Wave files containing 24-bit PCM sample data, and the ability to convert buffers containing 24-bit PCM sample data to/from other formats. (Thanks to [Rich Orton](https://github.com/richorton) for suggesting this).
* Reading files with 2 or more channels is now faster.
* Converting buffers from one format to another is now faster in certain cases.
* Bug fix: Files containing certain chunks with an odd size are now read properly. According to the Wave file spec, all chunks should be aligned to an even number of bytes. If the chunk has an odd size, a padding byte should be appended to bring the chunk to an even size. The `Reader` class now properly takes this expected padding byte into account for all chunks when reading files. (Previously it just took this into account for the main `data` chunk). (Thanks to [Andrew Kuklewicz](https://github.com/kookster) for reporting this).


# Compatibility

WaveFile has been tested with these Ruby versions, and appears to be compatible with them:

* MRI 2.0.0, 1.9.3, 1.9.2, 1.9.1, 1.8.7
* JRuby 1.7.8
* Rubinius 1.2.4
* MacRuby 0.12

If you find any compatibility issues, please let me know by opening a GitHub issue.


# Dependencies

WaveFile has no external dependencies. It is written in pure Ruby, and is entirely self-contained.


# Installation

First, install the WaveFile gem from rubygems.org:

    gem install wavefile

...and include it in your Ruby program:

    require 'wavefile'

Note that if you're installing the gem into the default Ruby that comes pre-installed on MacOS (as opposed to a Ruby installed via [RVM](http://beginrescueend.com/) or [rbenv](https://github.com/sstephenson/rbenv/)), you should used `sudo gem install wavefile`. Otherwise you might run into a file permission error.


# Contributing

1. Fork my repo
2. Create a branch for your changes
3. Add your changes, and please include tests
4. Make sure the tests pass by running `rake test`
5. Create a pull request
