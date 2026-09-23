# MIDI out: each verb sends its messages at logical time, to every port and channel unless told
midi_note_on :e3, 50
midi_note_off 52
midi_note_on 60, vel_f: 0.5, channel: 3, port: "synth"
midi 62, sustain: 0.5
sleep 0.5
midi_cc 10, 64
midi_cc 11, val_f: 0.25, channel: [1, 2]
midi_pc 5
midi_pitch_bend 0.75
midi_pitch_bend delta_midi: 100
midi_channel_pressure 90
midi_poly_pressure 60, 30
midi_raw 0xb0, 7, 127
midi_sysex 0xf0, 0x7e, 0x7f, 0xf7
midi_all_notes_off
midi_sound_off
midi_reset
midi_local_control_off
midi_local_control_on
midi_mode :mono, num_chans: 4
sleep 0.5
midi_clock_tick
midi_start
midi_stop
midi_continue
midi_clock_beat
