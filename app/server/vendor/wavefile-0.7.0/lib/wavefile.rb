require 'wavefile/buffer'
require 'wavefile/chunk_readers'
require 'wavefile/duration'
require 'wavefile/format'
require 'wavefile/reader'
require 'wavefile/unvalidated_format'
require 'wavefile/writer'

module WaveFile
  VERSION = "0.7.0"

  WAVEFILE_FORMAT_CODE = "WAVE"    # :nodoc:
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

  PACK_CODES = {:pcm   => { 8  => "C*", 16 => "s<*", 24 => "C*", 32 => "l<*"},
                :float => { 32 => "e*", 64 => "E*"}}    # :nodoc:

  UNSIGNED_INT_16 = "v"    # :nodoc:
  UNSIGNED_INT_32 = "V"    # :nodoc:
end
