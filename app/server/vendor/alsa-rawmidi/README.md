# alsa-rawmidi

#### Realtime MIDI IO with Ruby for Linux.

Access the ALSA RawMIDI API with Ruby.

Note that in the interest of allowing people on other platforms to utilize your code, you should consider using [unimidi](http://github.com/arirusso/unimidi).  Unimidi is a platform independent wrapper that implements this gem and has a similar API. 

## Features

* Simplified API
* Input and output on multiple devices concurrently
* Generalized handling of different MIDI Message types (including SysEx)
* Timestamped input events

## Requirements

* [ffi](http://github.com/ffi/ffi)
* libasound, libasound-dev packages

## Installation

If you're using Bundler, add this line to your application's Gemfile:

`gem "alsa-rawmidi"`

Otherwise

`gem install alsa-rawmidi`
	
## Usage

* [input](http://github.com/arirusso/alsa-rawmidi/blob/master/examples/input.rb)
* [output](http://github.com/arirusso/alsa-rawmidi/blob/master/examples/output.rb)

## Documentation

* [rdoc](http://rdoc.info/gems/alsa-rawmidi)
		
## Author 

[Ari Russo](http://github.com/arirusso) <ari.russo at gmail.com>
		
## License

Apache 2.0, See the file LICENSE

Copyright (c) 2010-2014 Ari Russo  
