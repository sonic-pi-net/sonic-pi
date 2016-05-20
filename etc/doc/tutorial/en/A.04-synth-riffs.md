A.4 Synth Riffs

# Synth Riffs

Whether it's the haunting drift of rumbling oscillators or the detuned
punch of saw waves piercing through the mix, the lead synth plays an
essential role on any electronic track. In last month's edition of this
tutorial series we covered how to code our beats. In this tutorial we'll
cover how to code up the three core components of a synth riff - the
timbre, melody and rhythm.

OK, so power up your Raspberry Pi, crack open Sonic Pi v2.6+ and let's
make some noise!


## Timbral Possibilities

An essential part of any synth riff is changing and playing with the
timbre of the sounds.  We can control the timbre in Sonic Pi in two ways
- choosing different synths for a dramatic change and setting the
various synth opts for more subtle modifications. We can also use FX,
but that's for another tutorial...

Let's create a simple live loop where we continually change the current
synth:

```
live_loop :timbre do
  use_synth (ring :tb303, :blade, :prophet, :saw, :beep, :tri).tick
  play :e2, attack: 0, release: 0.5, cutoff: 100
  sleep 0.5
end
```

Take a look at the code. We're simply ticking through a ring of synth
names (this will cycle through each of these in turn repeating the list
over and over). We pass this synth name to the `use_synth` fn (function)
which will change the `live_loop`'s current synth. We also play note
`:e2` (E at the second octave), with a release time of 0.5 beats (half a
second at the default BPM of 60) and with the `cutoff:` opt set to 100.

Hear how the different synths have very different sounds even though
they're all playing the same note. Now experiment and have a
play. Change the release time to bigger and smaller values. For example,
change the `attack:` and `release:` opts to see how different fade
in/out times have a huge impact on the sound. Finally change the
`cutoff:` opt to see how different cutoff values also massively
influence the timbre (values between 60 and 130 are good). See how many
different sounds you can create by just changing a few values. Once
you've mastered that, just head to the Synths tab in the Help system for
a full list of all the synths and all the available opts each individual
synth supports to see just how much power you have under your coding
fingertips.

## Timbre

Timbre is just a fancy word describing the sound of a sound. If you play
the same note with different instruments such as a violin, guitar, or
piano, the pitch (how high or low it sounds) would be the same, but the
sound quality would be different. That sound quality - the thing which
allows you to tell the difference between a piano and a guitar is the
timbre.


## Melodic Composition

Another important aspect to our lead synth is the choice of notes we
want to play. If you already have a good idea, then you can simply
create a ring with your notes in and tick through them:

```
live_loop :riff do
  use_synth :prophet
  riff = (ring :e3, :e3, :r, :g3, :r, :r, :r, :a3)
  play riff.tick, release: 0.5, cutoff: 80
  sleep 0.25
end
```
    
Here, we've defined our melody with a ring which includes both notes
such as `:e3` and rests represented by `:r`. We're then using `.tick` to
cycle through each note to give us a repeating riff.

## Auto Melody

It's not always easy to come up with a nice riff from scratch. Instead
it's often easier to ask Sonic Pi for a selection of random riffs and to
choose the one you like the best. To do that we need to combine three
things: rings, randomisation and random seeds. Let's look at an example:

```
live_loop :random_riff do
  use_synth :dsaw
  use_random_seed 3
  notes = (scale :e3, :minor_pentatonic).shuffle
  play notes.tick, release: 0.25, cutoff: 80
  sleep 0.25
end
```

There's a few things going on - let's look at them in turn. First, we
specify that we're using random seed 3. What does this mean? Well, the
useful thing is that when we set the seed, we can predict what the next
random value is going to be - it's the same as it was last time we set
the seed to 3! Another useful thing to know is that shuffling a ring of
notes works in the same way. In the example above we're essentially
asking for the 'third shuffle' in the standard list of shuffles - which
is also the same every time as we're always setting the random seed to
the same value right before the shuffle. Finally we're just ticking
through our shuffled notes to play the riff.

Now, here's where the fun starts. If we change the random seed value to
another number, say 3000, we get an entirely different shuffling of the
notes. So now it's extremely easy to explore new melodies. Simply choose
the list of notes we want to shuffle (scales are a great starting point)
and then choose the seed we want to shuffle with. If we don't like the
melody, just change one of those two things and try again. Repeat until
you like what you hear!


## Pseudo Randomisation

Sonic Pi's randomisation is not actually random it's what's called
pseudo random.  Imagine if you were to roll a dice 100 times and write
down the result of each roll onto a piece of paper. Sonic Pi has the
equivalent of this list of results which it uses when you ask for a
random value. Instead of rolling an actual dice, it just picks the next
value from the list. Setting the random seed is just jumping to a
specific point in that list.
 
## Finding your Rhythm

Another important aspect to our riff is the rhythm - when to play a note
and when not to. As we saw above we can use `:r` in our rings to insert
rests. Another very powerful way is to use spreads which we'll cover in
a future tutorial. Today we'll use randomisation to help us find our
rhythm. Instead of playing every note we can use a conditional to play a
note with a given probability. Let's take a look:

```
live_loop :random_riff do
  use_synth :dsaw
  use_random_seed 30
  notes = (scale :e3, :minor_pentatonic).shuffle
  16.times do
    play notes.tick, release: 0.2, cutoff: 90 if one_in(2)
    sleep 0.125
  end
end
```

A really useful fn to know is `one_in` which will give us a
`true` or `false` value with the specified probability. Here, we're
using a value of 2 so on average one time every two calls to `one_in` it
will return `true`. In other words, 50% of the time it will return
`true`. Using higher values will make it return `false` more often
introducing more space into the riff.

Notice that we've added some iteration in here with `16.times`. This is
because we only want to reset our random seed value every 16 notes so
our rhythm repeats every 16 times. This doesn't affect the shuffling as
that is still done immediately after the seed is set. We can use the
iteration size to alter the length of the riff. Try changing the 16 to 8
or even 4 or 3 and see how it affects the rhythm of the riff.

## Bringing it all together

OK, so let's combine everything we've learned together into one final
example. See you next time!

```
live_loop :random_riff do
  #  uncomment to bring in:
  #  synth :blade, note: :e4, release: 4, cutoff: 100, amp: 1.5
  use_synth :dsaw
  use_random_seed 43
  notes = (scale :e3, :minor_pentatonic, num_octaves: 2).shuffle.take(8)
  8.times do
    play notes.tick, release: rand(0.5), cutoff: rrand(60, 130) if one_in(2)
    sleep 0.125
  end
end
 
live_loop :drums do
  use_random_seed 500
  16.times do
    sample :bd_haus, rate: 2, cutoff: 110 if rand < 0.35
    sleep 0.125
  end
end
 
live_loop :bd do
  sample :bd_haus, cutoff: 100, amp: 3
  sleep 0.5
end
```
