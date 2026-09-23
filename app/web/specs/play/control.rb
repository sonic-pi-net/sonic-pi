# control changes a running synth, a sample or a chord; with no node it controls the last one triggered; kill stops one
load_sample :bd_haus
n = play 60, release: 2, cutoff: 80
sleep 0.5
control n, note: 65, cutoff: 100
sleep 0.5
control note: 67
s = sample :bd_haus, rate: 0.5
sleep 0.25
control s, rate: 1
c = play chord(:e3, :minor), release: 2
sleep 0.25
control c, notes: [:a3, :c4, :e4], amp: 0.6
sleep 0.25
kill n
use_bpm 120
m = play 50, release: 4, note_slide: 1
control m, note: 55, note_slide: 2
control m, on: false, note: 40
