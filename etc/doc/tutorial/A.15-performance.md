A.15 Five Live Coding Techniques

# Five Live Coding Techniques

In this month's Sonic Pi tutorial we're going to take a look at how you
can start treating Sonic Pi like a real instrument. We therefore need to
start thinking of code in a completely different way. Live coders think
of code in a similar way to how violinists think of their bow. In fact,
just like a violinist can apply various bowing techniques to create
different sounds (long slow motions vs short fast hits) we will explore
five of the basic live coding techniques that Sonic Pi enables. By the
end of this article you'll be able to start practicing for your own live
coded performances.

## 1. Memorise the Shortcuts

The first tip to live coding with Sonic Pi is to start using the
shortcuts. For example, instead of wasting valuable time reaching for
the mouse, moving it over to the Run button and clicking, you can simply
press `alt` and `r` at the same time which is much faster and keeps your
fingers at the keyboard ready for the next edit. You can find out the
shortcuts for the main buttons at the top by hovering the mouse
over them. See section 10.2 of the built-in tutorial for the full list of
shortcuts.

When performing, one fun thing to do is to add a bit of flair with your
arm motion when hitting shortcuts. For example, it's often good to
communicate to the audience when you're about to make a change - so
embellish your movement when hitting `alt-r` just like a guitarist would
do when hitting a big power chord.

## 2. Manually Layer your Sounds

Now you can trigger code instantly with the keyboard, you can instantly
apply this skill for our second technique which is to layer your sounds
manually. Instead of 'composing' using lots of calls to `play`, and
`sample` separated by calls to `sleep` we will have one call to `play`
which we will manually trigger using `alt-r`. Let's try it. Type the
following code into a fresh buffer:

```
synth :tb303, note: :e2 - 0, release: 12, cutoff: 90

```

Now, hit `Run` and whilst the sound is playing, modify the code in order
to drop down four notes by changing it to the following:


```
synth :tb303, note: :e2 - 4, release: 12, cutoff: 90

```

Now, hit `Run` again, to hear both sounds playing at the same time. This
is because Sonic Pi's `Run` button doesn't wait for any previous code to
finish, but instead starts the code running at the same time. This means
you can easily layer lots of sounds manually with minor or major
modifications between each trigger. For example, try changing both the
`note:` and the `cutoff:` opts and then re-trigger.


You can also try this technique with long abstract samples. For example:

```
sample :ambi_lunar_land, rate: 1
```

Try starting the sample off, and then progressively halving the `rate:`
opt between hitting `Run` from `1` to `0.5` to `0.25` to `0.125` and then
even try some negative values such as `-0.5`. Layer the sounds together
and see where you can take it. Finally, try adding some FX.

When performing, working with simple lines of code in this way means
that an audience new to Sonic Pi has a good chance to follow what you're
doing and relate the code that they can read to the sounds they are
hearing.


## 3. Master Live Loops

When working with more rhythmic music, it can often be hard to manually
trigger everything and keep good time. Instead, it is often better to
use a `live_loop`. This provides repetition for your code whilst also
giving the ability to edit the code for the next time round the
loop. They also will run at the same time as other `live_loop`s which
means you can layer them together both with each other and manual code
triggers. Take a look at section 9.2 of the built-in tutorial for more
information about working with live loops.

When performing, remember to make use of `live_loop`'s `sync:` opt to
allow you to recover from accidental runtime mistakes which stop the
live loop running due to an error. If you already have the `sync:` opt
pointing to another valid `live_loop`, then you can quickly fix the
error and re-run the code to re-start things without missing a beat.

## 4. Use the Main Mixer

One of Sonic Pi's best kept secrets is that it has a main mixer
through which all sound flows. This mixer has both a low pass filter and
a high pass filter built-in, so you can easily perform global
modifications to the sound. The main mixer's functionality can be
accessed via the fn `set_mixer_control!`. For example, whilst some code
is running and making sound, enter this into a spare buffer and hit
`Run`:

`
set_mixer_control! lpf: 50
`

After you run this code, all existing and new sounds will have a low
pass filter applied to them and will therefore sound more muffled. Note
that this means that the new mixer values stick until they are changed
again. However, if you want, you can always reset the mixer back to its
default state with `reset_mixer!`. Some of the currently supported opts
are: `pre_amp:`, `lpf:` `hpf:`, and `amp:`. For the full list, see the
built-in docs for `set_mixer_control!`.

Use the mixer's `*_slide` opts to slide one or many opts values over
time. For example, to slowly slide the mixer's low pass filter down from
the current value to 30, use the following:

```
set_mixer_control! lpf_slide: 16, lpf: 30
```

You can then slide quickly back to a high value with:

```
set_mixer_control! lpf_slide: 1, lpf: 130
```

When performing, it's often useful to keep a buffer free for working
with the mixer like this.

## 5. Practice

The most important technique for live coding is practice. The most
common attribute across professional musicians of all kinds is that they
practice playing with their instruments - often for many hours a
day. Practice is just as important for a live coder as a
guitarist. Practice allows your fingers to memorise certain patterns and
common edits so you can type and work with them more fluently. Practice
also gives you opportunities to explore new sounds and code constructs.

When performing, you'll find the more practice you do, the easier it
will be for you to relax into the gig. Practice will also give you a
wealth of experience to draw from. This can help you understand which
kinds of modifications will be interesting and also work well with the
current sounds.

## Bringing it all together

This month, instead of giving you a final example that combines all the
things discussed, let's part by setting down a challenge. See if you can
spend a week practicing one of these ideas every day. For example, one
day practice manual triggers, the next do some basic `live_loop` work
and the following day play around with the main mixer. Then
repeat. Don't worry if things feel slow and clunky at first - just keep
practicing and before you know it you'll be live coding for a real
audience.
