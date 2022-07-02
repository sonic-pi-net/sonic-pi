# Coded by Sam Aaron

use_bpm 50
notes = (scale :c1, :minor_pentatonic, num_octaves: 1).shuffle

live_loop :lorezzed do
  with_fx :compressor, pan: -0.3, amp: 2 do
    tick_reset
    t = 0.04
    sleep -t
    with_fx :lpf, cutoff: rrand(100, 130) do
      with_fx :krush, amp: 1.8  do
        s = synth :dsaw, note: :c3, sustain: 4, note_slide: t, release: 0
        7.times do
          sleep 0.25
          control s, note: notes.tick
        end
      end
    end
    sleep t
  end
end

with_fx :reverb, room: 1 do
  live_loop :synth_attack do
    tick
    with_fx :compressor, amp: 1.5, pan: 0.3 do
      with_fx :lpf, cutoff: (line 70, 131, steps: 32).look do
        [1, 1, 1, 1, 1, 1, 1, 4].look.times do
          with_fx :krush, reps: 4, amp: 3 do
            synth :fm, note: :c3, amp: 1, release: rrand(0.05, 0.3), pan: rrand(-0.5, 0.5)
            sleep [0.25, 0.125].choose
          end
        end
      end
    end
  end
end

with_fx :compressor do
  live_loop :industry do
    sample :loop_industrial, beat_stretch: 1, lpf: 110, rate: 1, amp: 3
    sleep 1
  end
end

with_fx :compressor do
  live_loop :drive do
    synth :fm, note: :c1, release: (knit 0.1, 8, 1, 8).tick, amp: 4, divisor: 1, depth: 1
    sample :bd_haus, amp: 5, lpf: 130, start: 0.14
    sleep 0.5
  end
end

live_loop :swirls do
  tick
  sample :ambi_lunar_land, rate: [-0.5, 0.5].look, amp: rrand(1, 2.5)
  with_fx :compressor do
    with_fx :lpf, cutoff: (range 100, 130, steps: 8).look do
      with_fx :krush, amp: 1.5 do
        synth :rodeo, note: (chord :c4, :minor7), release: 8, amp: 5
        synth :square, note: :c4, release: 4
      end
    end
  end
  sleep 16
end


##|                                             ,~~~
##|                                             +~~~~~
##|                                             ?~~~~+~
##|                                             8~~~~~+~,
##|                                             O~~~~~=+==
##|                                             8?~~~~=+=~~
##|                                             NI=~~~~~~~~~
##|                                             DO?~~=7I~~~~=
##|                                             Z8?~~~~~~:+~?$I,           ,,,,,
##|                                             ZNOI~~~~~~~~~~~.~7$$Z7:~~ZZZOSCZZ+,
##|                                             ZDO7:~~~=~~~~~~~         $MZCSOZZ,~
##|                                             ZDOOI~~?~~~~=====        ~DZO7
##|                                             ~NDOO?~~~~~~~~~~~        ~7:$
##|                                             :I OO$+~~~~==~~==~      ~? Z
##|                                             DDDD8D?~~~~~~~====~    ~: I$
##|                                            D DDDN8Z=~~~~~===~~~~  +   I`
##|                                           ? DDDDN8O7~~~~~~~~====.7   $$
##|                                            N888DNMOO?~~~~~===~~~$?  .?
##|                                        O88+DM ~NNN88OI~~~~~=~~~+~~?/$
##|                                         D  OVERTONE88I~~~~~===~~~,~
##|                                         D  ~ INC.NNN888I~~~~~=~~==,
##|                                         :I   ND~ DDN888O~~~?======,
##|                                          8  8DDD ~ M88887:I~~=====
##|                                          D 8DDDDND   D 88$~~~===+z
##|                                          :D8MDDDNN     DD87~~I~+=
##|                                           88DDDDDN       DD7?~~
##|                                           $CLOJURE         DD`
##|                                           $8DSDDDD
##|                                           DDDCDDDN
##|                                        ::7OLIBRARY$$$77I$IZ7
