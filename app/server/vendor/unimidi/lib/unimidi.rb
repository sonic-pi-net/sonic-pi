#
# Realtime MIDI IO for Ruby
#
# (c)2010-2014 Ari Russo and licensed under the Apache 2.0 License
#

# modules
require "unimidi/command"
require "unimidi/device"
require "unimidi/loader"
require "unimidi/platform"
require "unimidi/type_conversion"

# classes
require "unimidi/input"
require "unimidi/output"

module UniMIDI

  VERSION = "0.4.5"

  Platform.bootstrap

end
