A.14 Amplitude Modulation

# Amplitude Modulation

This month we're going to take a deep dive into one of Sonic Pi's most
powerful and flexible audio FX - the `:slicer`. By the end of this
article you will have learned how to manipulate the overall volume of
parts of our live coded sound in powerful new ways. This will allow you
to create new rhythmic and timbral structures and broaden your sonic
possibilities.

## Slice that Amp

So, what does the `:slicer` FX actually do?  One way to think about it
is that it's just like having someone play around with the volume
control on your TV or home hi-fi. Let's take a look but first, listen to
the deep growl of the following code which triggers the `:prophet`
synth:

```
synth :prophet, note: :e1, release: 8, cutoff: 70
synth :prophet, note: :e1 + 4, release: 8, cutoff: 80
```

Now, let's pipe it through the `:slicer` FX:

```

with_fx :slicer do
  synth :prophet, note: :e1, release: 8, cutoff: 70
  synth :prophet, note: :e1 + 4, release: 8, cutoff: 80
end
```

Hear how the slicer acts like it's muting and unmuting the audio with a
regular beat. Also, notice how the `:slicer` affects all the audio
generated between the `do`/`end` blocks. You can control the speed at which
it turns the audio on and off with the `phase:` opt which is short for
phase duration. Its default value is `0.25` which means 4 times a second
at the default BPM of 60. Let's make it faster:

```
with_fx :slicer, phase: 0.125 do
  synth :prophet, note: :e1, release: 8, cutoff: 70
  synth :prophet, note: :e1 + 4, release: 8, cutoff: 80
end
```

Now, play with different `phase:` durations yourself. Try longer and
shorter values. See what happens when you choose a really short
value. Also, try different synths such as `:beep` or `:dsaw` and
different notes. Take a look at the following diagram to see how
different `phase:` values change the number of amplitude changes per
beat.

![Phase Durations](../../../etc/doc/images/tutorial/articles/A.14-amplitude-modulation/slicer_phase_durations.png)

Phase duration is the length of time for one on/off cycle. Therefore
smaller values will make the FX switch on and off much faster than
larger values. Good values to start playing with are `0.125`, `0.25`,
`0.5` and `1`.


## Control Waves

By default, the `:slicer` FX uses a square wave to manipulate the
amplitude through time. This is why we hear the amplitude on for a
period, then immediately off for a period, then back on again. It turns
out that the square wave is just one of 4 different control waves that
are supported by `:slicer`. The others are saw, triangle and
(co)sine. Take a look at the diagram below to see what these look
like. We can also hear what they sound like. For example, the following
code uses (co)sine as the control wave. Hear how the sound doesn't turn
on and off abruptly but instead smoothly fades in and out:

```
with_fx :slicer, phase: 0.5, wave: 3 do
  synth :dsaw, note: :e3, release: 8, cutoff: 120
  synth :dsaw, note: :e2, release: 8, cutoff: 100
end
```

Have a play with the different wave forms by changing the `wave:` opt to
`0` for saw, `1` for square, `2` for triangle and `3` for sine. See how
different waves sound with different `phase:` opts too.

Each of these waves can be inverted with the `invert_wave:` opt which
flips it on the y axis. For example, in a single phase the saw wave
typically starts high, and slowly goes down before jumping back to the
top. With `invert_wave: 1` it will start low and slowly go up before
jumping back down again. Additionally, the control wave can be started
at different points with the `phase_offset:` opt which should be a value
between `0` and `1`. By playing around with `phase:`, `wave:`,
`invert_wave:` and `phase_offset` opts you can dramatically change how
the amplitude is modified through time.

![Phase Durations](../../../etc/doc/images/tutorial/articles/A.14-amplitude-modulation/slicer_control_waves.png)


## Setting your levels

By default, `:slicer` switches between amplitude values `1` (fully loud)
and `0` (silent). This can be changed with the `amp_min:` and `amp_max:`
opts. You can use this alongside the sine wave setting to create a
simple tremolo effect:

```
with_fx :slicer, amp_min: 0.25, amp_max: 0.75, wave: 3, phase: 0.25 do
  synth :saw, release: 8
end
```

This is just like grabbing the volume knob on your hi-fi and moving it
up and down just a little so the sound 'wobbles' in and out.


## Probabilities

One of `:slicer`'s powerful features is its ability to use probability
to choose whether or not to turn the slicer on or off. Before the
`:slicer` FX starts a new phase it rolls a dice and based on the result
either uses the selected control wave or keeps the amplitude off. Let's
take a listen:

```
with_fx :slicer, phase: 0.125, probability: 0.6  do
  synth :tb303, note: :e1, cutoff_attack: 8, release: 8
  synth :tb303, note: :e2, cutoff_attack: 4, release: 8
  synth :tb303, note: :e3, cutoff_attack: 2, release: 8
end
```

Hear how we now have an interesting rhythm of pulses. Try changing the
`probability:` opt to a different value between `0` and `1`. Values
closer to `0` will have more space between each sound due to the
likelihood of the sound being triggered being much lower.

Another thing to notice is that the probability system in the FX is just
like the randomisation system accessible via fns such as `rand` and
`shuffle`. They are both completely deterministic. This means that each
time you hit Run you'll hear exactly the same rhythm of pulses for a
given probability. If you would like to change things around you can use
the `seed:` opt to select a different starting seed. This works exactly
the same as `use_random_seed` but only affects that particular FX.

Finally, you can change the 'resting' position of the control wave when
the probability test fails from `0` to any other position with the
`prob_pos:` opt:

```
with_fx :slicer, phase: 0.125, probability: 0.6, prob_pos: 1  do
  synth :tb303, note: :e1, cutoff_attack: 8, release: 8
  synth :tb303, note: :e2, cutoff_attack: 4, release: 8
  synth :tb303, note: :e3, cutoff_attack: 2, release: 8
end
```

## Slicing Beats

One really fun thing to do is to use `:slicer` to chop a drum beat in
and out:

```
with_fx :slicer, phase: 0.125 do
  sample :loop_mika
end
```

This allows us to take any sample and create new rhythmical possibilites
which is a lot of fun. However, one thing to be careful about is to make
sure that the tempo of the sample matches the current BPM in Sonic Pi
otherwise the slicing will sound totally off. For example, try swapping
`:loop_mika` with the `loop_amen` sample to hear how bad this can sound
when the tempos don't align.

## Changing tempo

As we have already seen, changing the default BPM with `use_bpm` will
make all the sleep times and synth envelope durations grow or shrink to
match the beat. The `:slicer` FX honours this too, as the `phase:` opt
is actually measured in beats not seconds. We can therefore fix the
issue with `loop_amen` above by changing the BPM to match the sample:

```
use_sample_bpm :loop_amen

with_fx :slicer, phase: 0.125 do
  sample :loop_amen
end
```

## Bringing it all together

Let's apply all these ideas into a final example that only uses the
`:slicer` FX to create an interesting combination. Go ahead, start
changing it and make it into your own piece!

```
live_loop :dark_mist do
  co = (line 70, 130, steps: 8).tick
  with_fx :slicer, probability: 0.7, prob_pos: 1 do
    synth :prophet, note: :e1, release: 8, cutoff: co
  end
  
  with_fx :slicer, phase: [0.125, 0.25].choose do
    sample :guit_em9, rate: 0.5
  end
  sleep 8
end

live_loop :crashing_waves do
  with_fx :slicer, wave: 0, phase: 0.25 do
    sample :loop_mika, rate: 0.5
  end
  sleep 16
end
```




