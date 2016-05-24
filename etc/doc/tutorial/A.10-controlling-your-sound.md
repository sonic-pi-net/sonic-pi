A.10 Control

# Controlling Your Sound

So far during this series we've focussed on triggering sounds. We've
discovered that we can trigger the many synths built into Sonic Pi with
`play` or `synth` and how to trigger pre-recorded samples with
`sample`. We've also looked at how we can wrap these triggered sounds
within studio FX such as reverb and distortion using the `with_fx`
command. Combine this with Sonic Pi's incredibly accurate timing system
and you can produce a vast array of sounds, beats and riffs. However,
once you've carefully selected a particular sound's options and
triggered it, there's no ability to mess with it whilst it's playing
right? Wrong! Today you're going to learn something very powerful - how
to control running synths.

## A Basic Sound

Let's create a nice simple sound. Fire up Sonic Pi and in a fresh buffer
type the following:

```
synth :prophet, note: :e1, release: 8, cutoff: 100
```

Now press the Run button at the top left to hear a lovely rumbling synth
sound. Go ahead, press it again a few times to get a feel for it. OK,
done? Let's start controlling it!

## Synth Nodes

A little known feature in Sonic Pi is that the fns `play`, `synth` and
`sample`, return something called a `SynthNode` which represents a
running sound. You can capture one of these `SynthNode`s using a
standard variable and then **control** it at a later point in time. For
example, let's change the value of the `cutoff:` opt after 1 beat:

```
sn = synth :prophet, note: :e1, release: 8, cutoff: 100
sleep 1
control sn, cutoff: 130
```

Let's look at each line in turn: 

Firstly we trigger the `:prophet` synth using the `synth` fn as
normal. However we also capture the result in a variable called `sn`. We
could have called this variable something completely different such as
`synth_node` or `jane` - the name doesn't matter. However, it's
important to choose a name that's meaningful to you for your
performances and for people reading your code. I chose `sn` as it's a nice
short mnemonic for synth node.

On line 2 we have a standard `sleep` command. This does nothing special
- it just asks the computer to wait for 1 beat before moving onto the
next line.

Line 3 is where the control fun starts. Here, we use the `control` fn to
tell our running `SynthNode` to change the cutoff value to `130`. If you
hit the **Run** button, you'll hear the `:prophet` synth start playing
as before, but after 1 beat it will shift to sound a lot brighter.

Modulatable Options

Most of Sonic Pi's synths and FX opts may be changed after being
triggered. However, this isn't the case for all of them. For example,
the envelope opts `attack:`, `decay:`, `sustain:` and `release:` can
only be set when triggering the synth. Figuring out which opts can and
can't be changed is simple - just head to the documentation for a given
synth or FX and then scroll down to the individual option documentation
and look for the phrases "May be changed whilst playing" or "Can not be
changed once set". For example, the documentation for the `:beep`
synth's `attack:` opt makes it clear that it's not possible to change
it:

* Default: 0 
* Must be zero or greater 
* Can not be changed once set 
* Scaled with current BPM value 

## Multiple Changes

Whilst a synth is running you're not limited to changing it only once -
you're free to change it as many times as you like. For example, we can
turn our `:prophet` into a mini arpeggiator with the following:

```
notes = (scale :e3, :minor_pentatonic)
sn = synth :prophet, note: :e1, release: 8, cutoff: 100
sleep 1
16.times do
  control sn, note: notes.tick
  sleep 0.125
end
```

In this snippet of code we just added a couple of extra things. First we
defined a new variable called `notes` which contains the notes we'd like
to cycle through (an arpeggiator is just a fancy name for something that
cycles through a list of notes in order). Secondly we replaced our
single call to `control` with an iteration calling it 16 times. In each
call to `control` we `.tick` through our ring of `notes` which will
automatically repeat once we get to the end (thanks to the fabulous power
of Sonic Pi's rings). For a bit of variety try replacing `.tick` with
`.choose` and see if you can hear the difference.

Note that we can change multiple opts simultaneously. Try changing the
control line to the following and listen for the difference:

```
control sn, note: notes.tick, cutoff: rrand(70, 130)
```

## Sliding 

When we control a `SynthNode`, it responds exactly on time and instantly
changes the value of the opt to the new one as if you'd pressed a button
or flicked a switch requesting the change. This can sound rhythmical and percussive -
especially if the opt controls an aspect of the timbre such as
`cutoff:`. However, sometimes you don't want the change to happen
instantaneously. Instead, you might want to smoothly move from the
current value to the new one as if you'd moved a slider or dial. Of
course, Sonic Pi can also do this too using the `_slide:` opts.

Each opt that can be modified also has a special corresponding `_slide:`
opt that allows you to specify a slide time. For example, `amp:` has
`amp_slide:` and `cutoff:` has `cutoff_slide:`. These slide opts work
slightly differently than all the other opts in that they tell the synth
note how to behave **next time they are controlled**. Let's take a look:

```
sn = synth :prophet, note: :e1, release: 8, cutoff: 70, cutoff_slide: 2
sleep 1
control sn, cutoff: 130
```

Notice how this example is exactly the same as before except with the
addition of `cutoff_slide:`. This is saying that next time this synth
has its `cutoff:` opt controlled, it will take 2 beats to slide from the
current value to the new one. Therefore, when we use `control` you can
hear the cutoff slide from 70 to 130. It creates an interesting dynamic
feel to the sound. Now, try changing the `cutoff_slide:` time to a
shorter value such as 0.5 or a longer value such as 4 to see how it
changes the sound. Remember, you can slide any of the modifiable opts in
exactly this way and each `_slide:` value can be totally different so
you can have the cutoff sliding slowly, the amp sliding fast and the pan
sliding somewhere in between if that's what you're looking to create...

## Bringing it all together

Let's look at a short example which demonstrates the power of
controlling synths after they've been triggered. Notice that you can
also slide FX just like synths although with a slightly different
syntax. Check out section 7.2 of the built-in tutorial for more
information on controlling FX.

Copy the code into a spare buffer and take a listen. Don't stop there
though - play around with the code. Change the slide times, change the
notes, the synth, the FX and the sleep times and see if you can turn it
into something completely different!

```
live_loop :moon_rise do
  with_fx :echo, mix: 0, mix_slide: 8 do |fx|
    control fx, mix: 1
    notes = (scale :e3, :minor_pentatonic, num_octaves: 2).shuffle
    sn = synth :prophet , sustain: 8, note: :e1, cutoff: 70, cutoff_slide: 8
    control sn, cutoff: 130
    sleep 2
    32.times do
      control sn, note: notes.tick, pan: rrand(-1, 1)
      sleep 0.125
    end
  end
end
```
