#! /usr/bin/env ruby
#
# usage: split.rb [-x] [midi_file]
#
# This script splits a MIDI file into muliple files, one for each track. The
# output files are named with the track's names. Each file contains a copy of
# the 0th track, which contains tempo information.
#
# If -x is specified, the 0th temp track is not included in each file.
# Instead, it is output in a separate file named 'tempo_track.mid'.

# Start looking for MIDI module classes in the directory above this one.
# This forces us to use the local copy, even if there is a previously
# installed version out there somewhere.
$LOAD_PATH[0, 0] = File.join(File.dirname(__FILE__), '..', 'lib')

require 'midilib/sequence'

DEFAULT_MIDI_TEST_FILE = 'NoFences.mid'

# Command line argument processing
filename = ARGV[0]
include_tempo_track = true
if filename == '-x'
  include_tempo_track = false
  filename = ARGV[1]
end

# Read from MIDI file
seq = MIDI::Sequence.new()

File.open(filename || DEFAULT_MIDI_TEST_FILE, 'rb') do |file|
  # The block we pass in to Sequence.read is called at the end of every
  # track read. It is optional, but is useful for progress reports.
  seq.read(file) do |track, num_tracks, i|
    puts "read track #{track ? track.name : ''} (#{i} of #{num_tracks})"
  end
end

t0 = seq.tracks[0]
unless include_tempo_track
  s = MIDI::Sequence.new
  s.tracks << t0
  File.open("tempo_track.mid", 'wb') { | file | s.write(file) }
end
seq.each_with_index do |track, i|
  next unless i > 0
  s = MIDI::Sequence.new
  s.tracks << t0 if include_tempo_track
  s.tracks << track
  File.open("#{track.name}.mid", 'wb') { | file | s.write(file) }
end
