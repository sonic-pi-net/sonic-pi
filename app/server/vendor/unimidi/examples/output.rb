#!/usr/bin/env ruby

dir = File.dirname(File.expand_path(__FILE__))
$LOAD_PATH.unshift dir + "/../lib"

require "unimidi"

# Prompts the user to select a midi output
# Sends some arpeggiated chords to the output

notes = [36, 40, 43] # C E G
octaves = 5
duration = 0.1

# Prompt the user to select an output
output = UniMIDI::Output.gets

# using their selection...
(0..((octaves-1)*12)).step(12) do |oct|

  notes.each do |note|

    output.puts(0x90, note + oct, 100) # note on
    sleep(duration) # wait
    output.puts(0x80, note + oct, 100) # note off

  end

end
