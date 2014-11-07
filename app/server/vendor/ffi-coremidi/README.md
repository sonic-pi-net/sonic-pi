# ffi-coremidi

#### Realtime MIDI IO with Ruby for OSX

Access the [Apple Core MIDI framework API](https://developer.apple.com/library/mac/#documentation/MusicAudio/Reference/CACoreMIDIRef/MIDIServices/) with Ruby.

Note that in the interest of allowing people on other platforms to utilize your code, please consider using [UniMIDI](http://github.com/arirusso/unimidi).  UniMIDI is a platform independent wrapper which implements this library and has a similar API.

### Features

* Simplified API
* Input and output on multiple devices concurrently
* Generalized handling of different MIDI Message types (including SysEx)
* Timestamped input events
* Patch MIDI via software to other programs using IAC 

### Requirements

* [ffi](http://github.com/ffi/ffi)

### Installation

If you're using Bundler, add this line to your application's Gemfile:

`gem "ffi-coremidi"`
  
Otherwise

`gem install ffi-coremidi`
  
### Documentation

[rdoc](http://rubydoc.info/github/arirusso/ffi-coremidi)

### Author

[Ari Russo](http://github.com/arirusso) <ari.russo at gmail.com>

### Credits

This library began with some coremidi/ffi binding code for MIDI output by [Colin Harris](http://github.com/aberant) contained in [his fork of MIDIator](http://github.com/aberant/midiator) and a [blog post](http://aberant.tumblr.com/post/694878119/sending-midi-sysex-with-core-midi-and-ruby-ffi).

[MIDIator](http://github.com/bleything/midiator) is (c)2008 by Ben Bleything and Topher Cyll and released under the MIT license (see LICENSE.midiator and LICENSE.prp)

Also thank you to [Jeremy Voorhis](http://github.com/jvoorhis) for some useful debugging.

### License

Apache 2.0, See the file LICENSE

Copyright (c) 2011-2014 [Ari Russo](http://github.com/arirusso)
