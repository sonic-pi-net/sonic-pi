#! /usr/bin/env ruby
#
# usage: strings.rb [midi_file]
#
# Prints all strings (track names, etc.) found in the MIDI file.
#

# Start looking for MIDI module classes in the directory above this one.
# This forces us to use the local copy, even if there is a previously
# installed version out there somewhere.
$LOAD_PATH[0, 0] = File.join(File.dirname(__FILE__), '..', 'lib')

require 'midilib/sequence'
require 'midilib/consts'

DEFAULT_MIDI_TEST_FILE = 'NoFences.mid'

seq = MIDI::Sequence.new()
File.open(ARGV[0] || DEFAULT_MIDI_TEST_FILE, 'rb') do |file|
  # The block we pass in to Sequence.read is called at the end of every
  # track read. It is optional, but is useful for progress reports.
  seq.read(file) do |track, num_tracks, i|
    puts "read track #{track ? track.name : ''} (#{i} of #{num_tracks})"
  end
end

include MIDI
seq.each do |track|
  track.each do |event|
    puts event.data if event.kind_of?(MIDI::MetaEvent) &&
    [META_TEXT, META_COPYRIGHT, META_SEQ_NAME, META_INSTRUMENT,
      META_LYRIC, META_CUE, META_MARKER].include?(event.meta_type)
  end
end
