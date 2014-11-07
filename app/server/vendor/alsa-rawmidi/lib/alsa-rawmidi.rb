#
# Modules and classes to interact with the ALSA Driver Interface
#
# Ari Russo
# (c) 2010-2014
# Licensed under Apache 2.0

# libs
require "ffi"

# modules
require "alsa-rawmidi/api"
require "alsa-rawmidi/device"

# class
require "alsa-rawmidi/input"
require "alsa-rawmidi/output"
require "alsa-rawmidi/soundcard"

module AlsaRawMIDI

  VERSION = "0.3.1"

end
