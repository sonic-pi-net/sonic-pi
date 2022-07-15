#! /usr/bin/env python

# Simple demo to extract notes from a sound file, and store them in a midi file
# using mido.
#
# Install mido: `pip instal mido`
#
# Documentation: https://mido.readthedocs.io/

import sys
from aubio import source, notes
from mido import Message, MetaMessage, MidiFile, MidiTrack, second2tick, bpm2tempo

if len(sys.argv) < 3:
    print("Usage: %s <filename> <output> [samplerate]" % sys.argv[0])
    sys.exit(1)

filename = sys.argv[1]
midioutput = sys.argv[2]

downsample = 1
samplerate = 44100 // downsample
if len( sys.argv ) > 3: samplerate = int(sys.argv[3])

win_s = 512 // downsample # fft size
hop_s = 256 // downsample # hop size

s = source(filename, samplerate, hop_s)
samplerate = s.samplerate

tolerance = 0.8

notes_o = notes("default", win_s, hop_s, samplerate)

print("%8s" % "time","[ start","vel","last ]")

# create a midi file
mid = MidiFile()
track = MidiTrack()
mid.tracks.append(track)

ticks_per_beat = mid.ticks_per_beat # default: 480
bpm = 120 # default midi tempo

tempo = bpm2tempo(bpm)
track.append(MetaMessage('set_tempo', tempo=tempo))
track.append(MetaMessage('time_signature', numerator=4, denominator=4))

def frames2tick(frames, samplerate=samplerate):
    sec = frames / float(samplerate)
    return int(second2tick(sec, ticks_per_beat, tempo))

last_time = 0

# total number of frames read
total_frames = 0
while True:
    samples, read = s()
    new_note = notes_o(samples)
    if (new_note[0] != 0):
        note_str = ' '.join(["%.2f" % i for i in new_note])
        print("%.6f" % (total_frames/float(samplerate)), new_note)
        delta = frames2tick(total_frames) - last_time
        if new_note[2] > 0:
            track.append(Message('note_off', note=int(new_note[2]),
                velocity=127, time=0)
                )
        track.append(Message('note_on',
            note=int(new_note[0]),
            velocity=int(new_note[1]),
            time=delta)
            )
        last_time = frames2tick(total_frames)
    total_frames += read
    if read < hop_s: break

mid.save(midioutput)
