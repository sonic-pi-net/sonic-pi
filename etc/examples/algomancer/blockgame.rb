# Coded by DJ_Dave

use_bpm 130

live_loop :met1 do
  sleep 1
end

cmaster1 = 130
cmaster2 = 130

define :pattern do |pattern|
  return pattern.ring.tick == "x"
end

live_loop :kick, sync: :met1 do
  ##| stop
  a = 1.5
  sample :bd_tek, amp: a, cutoff: cmaster1 if pattern "x--x--x---x--x--"
  sleep 0.25
end

with_fx :echo, mix: 0.2 do
  with_fx :reverb, mix: 0.2, room: 0.5 do
    live_loop :clap, sync: :met1 do
      ##| stop
      a = 0.75
      sleep 1
      sample :drum_snare_hard, rate: 2.5, cutoff: cmaster1, amp: a
      sample :drum_snare_hard, rate: 2.2, start: 0.02, cutoff: cmaster1, pan: 0.2, amp: a
      sample :drum_snare_hard, rate: 2, start: 0.04, cutoff: cmaster1, pan: -0.2, amp: a
      sleep 1
    end
  end
end

with_fx :reverb, mix: 0.2 do
  with_fx :panslicer, mix: 0.2 do
    live_loop :hhc1, sync: :met1 do
      ##| stop
      a = 0.75
      p = [-0.3, 0.3].choose
      sample :drum_cymbal_closed, amp: a, rate: 2.5, finish: 0.5, pan: p, cutoff: cmaster2 if pattern "x-x-x-x-x-x-x-x-xxx-x-x-x-x-x-x-"
      sleep 0.125
    end
  end
end

live_loop :hhc2, sync: :met1 do
  ##| stop
  a = 1.25
  sleep 0.5
  sample :drum_cymbal_closed, cutoff: cmaster2, rate: 1.2, start: 0.01, finish: 0.5, amp: a
  sleep 0.5
end

with_fx :reverb, mix: 0.7 do
  live_loop :crash, sync: :met1 do
    ##| stop
    a = 0.1
    c = cmaster2-10
    r = 1.5
    f = 0.25
    crash = :drum_splash_soft
    sleep 14.5
    sample crash, amp: a, cutoff: c, rate: r, finish: f
    sample crash, amp: a, cutoff: c, rate: r-0.2, finish: f
    sleep 1
    sample crash, amp: a, cutoff: c, rate: r, finish: f
    sample crash, amp: a, cutoff: c, rate: r-0.2, finish: f
    sleep 0.5
  end
end

with_fx :reverb, mix: 0.7 do
  live_loop :arp, sync: :met1 do
    with_fx :echo, phase: 1, mix: (line 0.1, 1, steps: 128).mirror.tick do
      ##| stop
      a = 0.6
      r = 0.25
      c = 130
      p = (line -0.7, 0.7, steps: 64).mirror.tick
      at = 0.01
      use_synth :beep
      tick
      notes = (scale :g4, :major_pentatonic).shuffle
      play notes.look, amp: a, release: r, cutoff: c, pan: p, attack: at
      sleep 0.75
    end
  end
end

with_fx :panslicer, mix: 0.4 do
  with_fx :reverb, mix: 0.75 do
    live_loop :synthbass, sync: :met1 do
      ##| stop
      s = 4
      r = 2
      c = 60
      a = 0.75
      at = 0
      use_synth :tech_saws
      play :g3, sustain: 6, cutoff: c, amp: a, attack: at
      sleep 6
      play :d3, sustain: 2, cutoff: c, amp: a, attack: at
      sleep 2
      play :e3, sustain: 8, cutoff: c, amp: a, attack: at
      sleep 8
    end
  end
end
