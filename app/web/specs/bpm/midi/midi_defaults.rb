# use_midi_defaults, with_midi_defaults and merged defaults set port, channel and velocity; rests send nothing
use_bpm 120   # at twice the tempo: specs/midi/midi_defaults.rb
use_midi_defaults channel: 5, port: "p"
midi_note_on 70
with_midi_defaults channel: 6 do
  midi_note_off 70
end
use_merged_midi_defaults vel: 20
midi_note_on 71
midi_note_on :rest
midi_cc 1, 2, on: false
use_midi_logging false
midi_cc 1, 2
puts current_midi_defaults
use_transpose 12
midi 60, sustain: 0.25
