## BLOCKGAME
## CODED BY DJ_DAVE

use_bpm 130

live_loop :met1 do
  sleep 1
end

cmaster1 = 130
cmaster2 = 130

define :pattern do |pattern|
  return pattern.ring.tick == "x"
end

live_loop :main, sync: :met1 do
  stop
  a = 1.5
  main =  "/Volumes/dave/DJ_DAVE/samples/blockgame/main_synth.wav"
  sample main, amp: a, rate: 1.5275
  sleep 16
end

live_loop :bis_saw_bass, sync: :met1 do
  stop
  bsb = "/Volumes/dave/DJ_DAVE/samples/blockgame/big_saw_bass.wav"
  sample bsb, rate: 1.5275
  sleep 16
end

##| with_fx :reverb, mix: 0.4, room: 1 do
##| with_fx :echo do
live_loop :compute, sync: :met1 do
  stop
  a = 0.5
  compute = "/Volumes/dave/DJ_DAVE/samples/blockgame/compute.wav"
  sample compute, amp: a, rate: 1.5275
  sleep 32
end
##| end
##| end

live_loop :clap, sync: :met1 do
  ##| stop
  clap = "/Volumes/dave/DJ_DAVE/samples/blockgame/clap.wav"
  sleep 1
  sample clap, cutoff: cmaster1
  sleep 1
end

live_loop :hhc1, sync: :met1 do
  ##| stop
  a = 0.3
  p = [-0.5, 0.5].choose
  hhc = "/Volumes/dave/DJ_DAVE/samples/blockgame/hhc2.wav"
  sample hhc, amp: a, pan: p, cutoff: cmaster2 if pattern "x-x-x-x-x-x-x-x-xxx-x-x-x-x-x-x-"
  sleep 0.125
end

live_loop :hhc2, sync: :met1 do
  ##| stop
  hhc = "/Volumes/dave/DJ_DAVE/samples/blockgame/hhc.wav"
  sleep 0.5
  sample hhc, cutoff: cmaster1, start: 0.5
  sleep 0.5
end

live_loop :crash, sync: :met1 do
  stop
  a = 3
  crash = "/Volumes/dave/DJ_DAVE/samples/blockgame/crash.wav"
  sleep 14.5
  sample crash, amp: a
  sleep 0.75
  sample crash, amp: a
  sleep 0.75
end

live_loop :kick2, sync: :met1 do
  with_fx :reverb, room: 1 do
    ##| stop
    a = 1
    kick = "/Volumes/dave/DJ_DAVE/samples/blockgame/kick.wav"
    sample kick, amp: a, cutoff: cmaster1
    sleep 8
  end
end

live_loop :kick, sync: :met1 do
  ##| stop
  a = 1.5
  kick = "/Volumes/dave/DJ_DAVE/samples/blockgame/kick2.wav"
  sample kick, amp: a, cutoff: cmaster1# if pattern "x--x--x---x--x--"
  sleep 1#0.25
end

with_fx :reverb, mix: 0.7 do
  live_loop :arp, sync: :met1 do
    ##| stop
    a = 0.8#(line 0.2, 0.8, steps: 64*2*2).mirror.tick
    r = 0.25
    c = 130#(line 60, 130, steps: 64*2*2).mirror.tick
    p = (line -0.7, 0.7, steps: 64).mirror.tick
    at = 0.01
    use_synth  :beep
    tick
    notes = (scale :g4, :major_pentatonic).shuffle
    play notes.look, amp: a, release: r, cutoff: c, pan: p, attack: at
    sleep 0.75
  end
end

with_fx :panslicer, mix: 0.4 do
  with_fx :reverb, mix: 0.75 do
    live_loop :synthbass, sync: :met1 do
      ##| stop
      s = 4
      r = 2
      c = 60
      a = 0.6
      use_synth :tech_saws
      play :g3, sustain: 6, cutoff: c, amp: a
      sleep 6
      play :d3, sustain: 2, cutoff: c, amp: a
      sleep 2
      play :e3, sustain: 8, cutoff: c, amp: a
      sleep 8
    end
  end
end

