require 'wavefile/buffer'
require 'wavefile/duration'
require 'wavefile/format'
require 'wavefile/info'
require 'wavefile/reader'
require 'wavefile/writer'

module WaveFile
  VERSION = "0.6.0"

  WAVEFILE_FORMAT_CODE = "WAVE"    # :nodoc:
  FORMAT_CHUNK_BYTE_LENGTH = {:pcm => 16, :float => 18}    # :nodoc:
  FORMAT_CODES = {:pcm => 1, :float => 3}    # :nodoc:
  CHUNK_IDS = {:riff         => "RIFF",
               :format       => "fmt ",
               :data         => "data",
               :fact         => "fact",
               :silence      => "slnt",
               :cue          => "cue ",
               :playlist     => "plst",
               :list         => "list",
               :label        => "labl",
               :labeled_text => "ltxt",
               :note         => "note",
               :sample       => "smpl",
               :instrument   => "inst" }    # :nodoc:

  PACK_CODES = {:pcm => {8 => "C*", 16 => "s*", 24 => "C*", 32 => "l*"},
                :float => { 32 => "e*", 64 => "E*"}}    # :nodoc:

  UNSIGNED_INT_16 = "v"    # :nodoc:
  UNSIGNED_INT_32 = "V"    # :nodoc:
end

