#!/usr/bin/env ruby

dir = File.dirname(File.expand_path(__FILE__))
$LOAD_PATH.unshift dir + "/../lib"

require "alsa-rawmidi"

# Selects the first MIDI output and sends some arpeggiated chords to it

notes = [36, 40, 43] # C E G
octaves = 5
duration = 0.1

# AlsaRawMIDI::Device.all.to_s will list your midi devices
# or amidi -l from the command line

puts "Press Control-C to exit..."

AlsaRawMIDI::Output.first.open do |output|

  loop do
    (0..((octaves-1)*12)).step(12) do |oct|

      notes.each do |note|

        output.puts(0x90, note + oct, 100) # note on
        sleep(duration)				     # wait
        output.puts(0x80, note + oct, 100) # note off
        sleep(duration)

      end

    end
  end

end
