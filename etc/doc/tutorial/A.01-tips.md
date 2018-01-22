A.1 Tips for Sonic Pi

# Five Top Tips

## 1. There are no mistakes

The most important lesson to learn with Sonic Pi is that there really
are no mistakes. The best way to learn is to just try and try and
try. Try lots of different things out, stop worrying whether your code
sounds good or not and start experimenting with as many different
synths, notes, FX and opts as possible. You'll discover a lot of things
that make you laugh because they sound just awful and some real gems
that sound truly amazing. Simply drop the things you don't like and keep
the things you do. The more 'mistakes' you allow yourself to make the
quicker you'll learn and discover your personal coding sound.


## 2. Use the FX

Say you've already mastered the Sonic Pi basics of making sounds with
`sample`, `play`? What's next? Did you know that Sonic Pi supports over
27 studio FX to change the sound of your code? FX are like fancy image
filters in drawing programs except that instead of blurring or making
something black and white, you can add things like reverb, distortion
and echo to your sound. Think of it like sticking the cable from your
guitar to an effects pedal of your choice and then into the
amplifier. Luckily, Sonic Pi makes using FX really easy and requires no
cables! All you need to do is to choose which section of your code you'd
like the FX added to and wrap it with the FX code. Let's look at an
example. Say you had the following code:

```
sample :loop_garzul

16.times do
  sample :bd_haus
  sleep 0.5
end
```

If you wanted to add FX to the `:loop_garzul` sample, you'd just tuck it
inside a `with_fx` block like this:

```
with_fx :flanger do
  sample :loop_garzul
end

16.times do
  sample :bd_haus
  sleep 0.5
end
```

Now, if you wanted to add FX to the bass drum, go and wrap that with
`with_fx` too:

```
with_fx :flanger do
  sample :loop_garzul
end

with_fx :echo do
  16.times do
    sample :bd_haus
    sleep 0.5
  end
end
```

Remember, you can wrap *any* code within `with_fx` and any sounds
created will pass through that FX. 


## 3. Parameterise your synths

In order to really discover your coding sound you'll soon want to know
how to modify and control synths and FX. For example, you might want to
change the duration of a note, add more reverb, or change the time
between echoes. Luckily, Sonic Pi gives you an amazing level of control
to do exactly this with special things called optional parameters or
opts for short. Let's take a quick look. Copy this code into a workspace
and hit run:

```
sample :guit_em9
```

Ooh, a lovely guitar sound! Now, let's start playing with it. How about
changing its rate?

```
sample :guit_em9, rate: 0.5
```

Hey, what's that `rate: 0.5` bit I just added at the end? That's called
an opt. All of Sonic Pi's synths and FX support them and there's loads
to play around with. They're also available for FX too. Try this:

```
with_fx :flanger, feedback: 0.6 do
  sample :guit_em9
end
```

Now, try increasing that feedback to 1 to hear some crazy sounds! Read the
docs for full details on all the many opts available to you.


## 4. Live Code

The best way to quickly experiment and explore Sonic Pi is to live
code. This allows you to start off some code and continually change and
tweak it whilst it's still playing. For example, if you don't know what
the cutoff parameter does to a sample, just play around. Let's have a try!
Copy this code into one of your Sonic Pi workspaces:

```
live_loop :experiment do
  sample :loop_amen, cutoff: 70
  sleep 1.75
end
```

Now, hit run and you'll hear a slightly muffled drum break. Now, change
the `cutoff:` value to `80` and hit run again. Can you hear the
difference? Try `90`, `100`, `110`...

Once you get the hang of using `live_loop`s you'll not turn
back. Whenever I do a live coding gig I rely on `live_loop` as much as a
drummer relies on their sticks. For more information about live coding
check out Section 9 of the built-in tutorial.

## 5. Surf the random streams

Finally, one thing I love doing is cheating by getting Sonic Pi to
compose things for me.  A really great way to do this is using
randomisation. It might sound complicated but it really isn't. Let's
take a look. Copy this into a spare workspace:

```
live_loop :rand_surfer do
  use_synth :dsaw
  notes = (scale :e2, :minor_pentatonic, num_octaves: 2)
  16.times do
    play notes.choose, release: 0.1, cutoff: rrand(70, 120)
    sleep 0.125
  end
end
```

Now, when you play this, you'll hear a constant stream of random notes
from the scale `:e2 :minor_pentatonic` played with the `:dsaw`
synth. "Wait, wait! That's not a melody", I hear you shout! Well, here's
the first part of the magic trick. Every time we go round the
`live_loop` we can tell Sonic Pi to reset the random stream to a known
point. This is a bit like going back in time in the TARDIS with the
Doctor to a particular point in time and space. Let's try it - add the
line `use_random_seed 1` to the `live_loop`:

```
live_loop :rand_surfer do
  use_random_seed 1
  use_synth :dsaw
  notes = (scale :e2, :minor_pentatonic, num_octaves: 2)
  16.times do
    play notes.choose, release: 0.1, cutoff: rrand(70, 120)
    sleep 0.125
  end
end
```

Now, every time the `live_loop` loops around, the random stream is
reset. This means it chooses the same 16 notes every time. Hey presto!
An instant melody. Now, here's the really exciting bit. Change the seed
value from `1` to another number. Say `4923`. Wow! Another melody! So,
just by changing one number (the random seed), you can explore as many
melodic combinations as you can imagine! Now, that's the magic of code.
