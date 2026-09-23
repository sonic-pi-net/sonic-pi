# hz_to_midi and midi_to_hz convert between frequency and MIDI note
use_bpm 120   # at twice the tempo: specs/lang/hz_to_midi.rb
puts hz_to_midi(440)
puts hz_to_midi(261.63)
puts midi_to_hz(69)
puts midi_to_hz(:c4)
play hz_to_midi(330)
