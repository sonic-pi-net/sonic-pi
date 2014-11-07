dir = File.dirname(File.expand_path(__FILE__))
$LOAD_PATH.unshift dir + '/../lib'

require 'midi-winmm'

# this program selects the first midi output and sends some arpeggiated chords to it

notes = [36, 40, 43] # C E G
octaves = 6
duration = 0.1

# MIDIWinMM::Output.all.to_s will list your midi outputs
  
MIDIWinMM::Output.first.open do |output|

  (0..((octaves-1)*12)).step(12) do |oct|
    notes.each do |note|
      output.puts(0x90, note + oct, 100)
      sleep(duration)
      output.puts(0x80, note + oct, 100)
    end
  end
  
end