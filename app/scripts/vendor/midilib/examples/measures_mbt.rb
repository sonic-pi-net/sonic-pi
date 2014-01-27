#!/usr/bin/env ruby
#
# usage: measure_mbt.rb midi_file
#
# This program loads a sequences and prints out all start-of-notes
# in a "sequencer-style" manner:
#    Measure:Beat:Tick   Channel: Note-name

# Start looking for MIDI module classes in the directory above this one.
# This forces us to use the local copy, even if there is a previously
# installed version out there somewhere.
$LOAD_PATH[0, 0] = File.join(File.dirname(__FILE__), '..', 'lib')

require 'midilib/sequence'

seq = MIDI::Sequence.new()
File.open(ARGV[0], 'rb') { | file | seq.read(file) }

# Get all measures, so events can be mapped to measures:
measures = seq.get_measures

seq.each { |track|
  track.each { |e|
    if e.kind_of?(MIDI::NoteOn) then
      # Print out start of notes
      e.print_note_names = true
      puts measures.to_mbt(e) + "  ch #{e.channel}:  #{e.note_to_s}"
    end
  }
}
