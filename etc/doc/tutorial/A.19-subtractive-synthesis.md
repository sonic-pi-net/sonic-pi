A.19 Sound Design - Subtractive Synthesis

# Subtractive Synthesis

This is the second in a series of articles on how to use Sonic Pi for
sound design. Last month we looked at additive synthesis which we
discovered was the simple act of playing multiple sounds at the same
time to make a new combined sound. For example we could combine
different sounding synths or even the same synth at different pitches to
build a new complex sound from simple ingredients. This month we'll look
at a new technique commonly called _subtractive synthesis_ which is
simply the act of taking an existing complex sound and removing parts of
it to create something new. This is a technique which is commonly
associated with the sound of analog synthesisers of the 1960s and 1970s
but also with the recent renaissance of modular analog synths through
popular standards such as Eurorack.

Despite this sounding like a particularly complicated and advanced
technique, Sonic Pi makes it surprisingly simple and easy - so let's
dive right in.

## Complex Source Signal

For a sound to work well with subtractive synthesis, it typically needs
to be fairly rich and interesting. This doesn't mean we need something
hugely complex - in fact, just a standard `:square` or `:saw` wave will
do:

```
synth :saw, note: :e2, release: 4
```

Notice that this sound is already pretty interesting and contains many
different frequencies above `:e2` (the second E on a piano) which add to
create the timbre. If that didn't make much sense to you, try comparing
it with the `:beep`:

```
synth :beep, note: :e2, release: 4
```

As the `:beep` synth is just a sine wave, you'll hear a much purer tone
and only at `:e2` and none of the high crispy/buzzy sounds which you
heard in the `:saw`. It's this buzziness and variation from a pure sine
wave that we can play with when we use subtractive synthesis.

## Filters

Once we have our raw source signal, the next step is to pass it through
a filter of some kind which will modify the sound by removing or
reducing parts of it. One of the most common filters used for
subtractive synthesis is something called a low pass filter. This will
allow all the low parts of the sound through but will reduce or remove
the higher parts. Sonic Pi has a powerful yet simple to use FX system
that includes a low pass filter, called `:lpf`. Let's play with it:

```
with_fx :lpf, cutoff: 100 do
  synth :saw, note: :e2, release: 4
end
```

If you listen carefully you'll hear how some of that buzziness and
crispiness has been removed. In fact, all the frequencies in the sound
above note `100` have been reduced or removed and only the ones below are
still present in the sound. Try changing that `cutoff:` point to
lower notes, say `70` and then `50` and compare the sounds.

Of course, the `:lpf` isn't the only filter you can use to manipulate
the source signal. Another important FX is the high pass filter referred
to as `:hpf` in Sonic Pi. This does the opposite to `:lpf` in that it
lets the high parts of the sound through and cuts off the low parts.

```
with_fx :hpf, cutoff: 90 do
  synth :saw, note: :e2, release: 4
end
```

Notice how this sounds much more buzzy and raspy now that all the low
frequency sounds have been removed. Play around with the cutoff value -
notice how lower values let more of the original bass parts of the
source signal through and higher values sound increasingly tinny and
quiet.

## Low Pass Filter

The low pass filter is such an important part of every subtractive
synthesis toolkit that it's worth taking a deeper look at how it
works. This diagram shows the same sound wave (the `:prophet` synth)
with varying amounts of filtering. At the top, section A shows the audio
wave with no filtering. Notice how the wave form is very pointy and
contains lots of sharp edges. It is these hard, sharp angles that
produce the high crispy/buzzy parts of the sound. Section B shows the low
pass filter in action - notice how it is less pointy and more rounded
than the wave form above. This means that the sound will have fewer high
frequencies giving it a more mellow rounded feel. Section C shows the
low pass filter with a fairly low cutoff value - this means that even
more of the high frequencies have been removed from the signal resulting
in an even softer, rounder wave form. Finally, notice how the size of
the wave form, which represents the amplitude, decreases as we move from
A to C. Subtractive synthesis works by removing parts of the signal
which means that the overall amplitude is reduced as the amount of
filtering that is taking place increases.


## Filter Modulation

So far we've just produced fairly static sounds. In other words, the
sound doesn't change in any way for the entirety of its duration. Often
you might want some movement in the sound to give the timbre some
life. One way to achieve this is via filter modulation - changing the
filter's options through time. Luckily Sonic Pi gives you powerful tools
to manipulate an FX's opts through time. For example, you can set a
slide time to each modulatable opt to specify how long it should take
for the current value to linearly slide to the target value:

```
with_fx :lpf, cutoff: 50 do |fx|
  control fx, cutoff_slide: 3, cutoff: 130
  synth :prophet, note: :e2, sustain: 3.5
end
```

Let's take a quick look at what's going on here. Firstly we start an
`:lpf` FX block as normal with an initial `cutoff:` of a very low
`20`. However, the first line also finishes with the strange `|fx|` at
the end. This is an optional part of the `with_fx` syntax which allows
you to directly name and control the running FX synth. Line 2 does
exactly this and controls the FX to set the `cutoff_slide:` opt to 4 and
the new target `cutoff:` to be `130`. The FX will now start sliding the
`cutoff:` opt's value from `50` to `130` over a period of 3
beats. Finally we also trigger a source signal synth so we can hear the
effect of the modulated low pass filter.


## Bringing it all together

This is just a very basic taster of what's possible when you use filters
to modify and change a source sound. Try playing with Sonic Pi's many
built-in FX to see what crazy sounds you can design. If your sound feels
too static, remember you can start modulating the options to create some
movement.

Let's finish by designing a function which will play a new sound created
with subtractive synthesis. See if you can figure out what's going on
here - and for the advanced Sonic Pi readers out there - see if you can
work out why I wrapped everything inside a call to `at` (please send
answers to @samaaron on Twitter).

```
define :subt_synth do |note, sus|
  at do
    with_fx :lpf, cutoff: 40, amp: 2 do |fx|
      control fx, cutoff_slide: 6, cutoff: 100
      synth :prophet, note: note, sustain: sus
    end
    with_fx :hpf, cutoff_slide: 0.01 do |fx|
      synth :dsaw, note: note + 12, sustain: sus
      (sus * 8).times do
        control fx, cutoff: rrand(70, 110)
        sleep 0.125
      end
    end
  end
end

subt_synth :e1, 8
sleep 8
subt_synth :e1 - 4, 8
```
