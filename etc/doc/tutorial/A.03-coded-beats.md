A.3 Coded Beats

# Coded Beats

One of the most exciting and disrupting technical developments in modern
music was the invention of samplers. These were boxes that allowed you
to record any sound into them and then manipulate and play back those
sounds in many interesting ways. For example, you could take an old
record, find a drum solo (or break), record it into your sampler and
then play it back on repeat at half-speed to provide the foundation for
your latest beats. This is how early hip-hop music was born and today
it's almost impossible to find electronic music that doesn't incorporate
samples of some kind. Using samples is a really great way of easily
introducing new and interesting elements into your live coded
performances.

So where can you get a sampler? Well you already have one - it's your
Raspberry Pi! The built-in live coding app Sonic Pi has an extremely
powerful sampler built into its core. Let's play with it!

## The Amen Break

One of the most classic and recognisable drum break samples is called
the Amen Break. It was first performed in 1969 in the song "Amen
Brother" by the Winstons as part of a drum break. However, it was when
it was discovered by early hip-hop musicians in the 80s and used in
samplers that it started being heavily used in a wide variety of other
styles such as drum and bass, breakbeat, hardcore techno and breakcore.

I'm sure you're excited to hear that it's also built right into Sonic
Pi. Clear up a buffer and throw in the following code:

```
sample :loop_amen
```

Hit *Run* and boom! You're listening to one of the most influential
drum breaks in the history of dance music. However, this sample wasn't famous
for being played as a one-shot, it was built for being looped. 


## Beat Stretching 

Let's loop the Amen Break by using our old friend the `live_loop`
introduced in this tutorial last month:

```
live_loop :amen_break do
  sample :loop_amen
  sleep 2
end
```

OK, so it is looping, but there's an annoying pause every time
round. That is because we asked it to sleep for `2` beats and with
the default BPM of 60 the `:loop_amen` sample only lasts for `1.753`
beats. We therefore have a silence of `2 - 1.753 = 0.247` beats. Even
though it's short, it's still noticeable.

To fix this issue we can use the `beat_stretch:` opt to ask Sonic Pi to
stretch (or shrink) the sample to match the specified number of beats.

Sonic Pi's `sample` and `synth` fns give you a lot
of control via optional parameters such as `amp:`, `cutoff:` and
`release:`. However, the term optional parameter is a real mouthful so
we just call them *opts* to keep things nice and simple. 

```
live_loop :amen_break do
  sample :loop_amen, beat_stretch: 2
  sleep 2
end  
```

Now we're dancing! Although, perhaps we want to speed it up or slow it down
to suit the mood.

## Playing with Time

OK, so what if we want to change styles to old school hip hop or
breakcore? One simple way of doing this is to play with time - or in
other words mess with the tempo. This is super easy in Sonic Pi - just
throw in a `use_bpm` into your live loop:

```
live_loop :amen_break do
  use_bpm 30
  sample :loop_amen, beat_stretch: 2
  sleep 2
end 
```

Whilst you're rapping over those slow beats, notice that we're still
sleeping for 2 and our BPM is 30, yet everything is in time. The
`beat_stretch` opt works with the current BPM to make sure everything just works. 

Now, here's the fun part. Whilst the loop is still live, change the `30`
in the `use_bpm 30` line to `50`. Woah, everything just got faster yet *kept
in time*! Try going faster - up to 80, to 120, now go crazy and punch in
200!


## Filtering

Now we can live loop samples, let's look at some of the most fun opts
provided by the `sample` synth. First up is `cutoff:` which controls the
cutoff filter of the sampler. By default this is disabled but you can
easily turn it on:

```
live_loop :amen_break do
  use_bpm 50
  sample :loop_amen, beat_stretch: 2, cutoff: 70
  sleep 2
end  
```

Go ahead and change the `cutoff:` opt. For example, increase it to 100,
hit *Run* and wait for the loop to cycle round to hear the change in the
sound. Notice that low values like 50 sound mellow and bassy and high
values like 100 and 120 are more full-sounding and raspy. This is
because the `cutoff:` opt will chop out the high frequency parts of the
sound just like a lawn-mower chops off the top of the grass. The
`cutoff:` opt is like the length setting - determining how much grass is
left over.


## Slicing    

Another great tool to play with is the slicer FX. This will chop (slice)
the sound up. Wrap the `sample` line with the FX code like this:

```
live_loop :amen_break do
  use_bpm 50
  with_fx :slicer, phase: 0.25, wave: 0, mix: 1 do
    sample :loop_amen, beat_stretch: 2, cutoff: 100
  end
  sleep 2
end
```

Notice how the sound bounces up and down a little more. (You can hear
the original sound without the FX by changing the `mix:` opt to `0`.)
Now, try playing around with the `phase:` opt. This is the rate (in
beats) of the slicing effect. A smaller value like `0.125` will slice
faster and larger values like `0.5` will slice more slowly. Notice that
successively halving or doubling the `phase:` opts val tends to always
sound good. Finally, change the `wave:` opt to one of 0, 1, or 2 and
hear how it changes the sound. These are the various wave shapes. 0 is a
saw wave, (hard in, fade out) 1 is a square wave (hard in, hard out) and
2 is a triangle wave (fade in, fade out).


## Bringing it all together

Finally, let's go back in time and revisit the early Bristol drum and
bass scene with this month's example. Don't worry too much about what
all this means, just type it in, hit Run, then start live coding it by
changing opt numbers and see where you can take it. Please do share what
you create! See you next time...

```
use_bpm 100

live_loop :amen_break do
  p = [0.125, 0.25, 0.5].choose
  with_fx :slicer, phase: p, wave: 0, mix: rrand(0.7, 1) do
    r = [1, 1, 1, -1].choose
    sample :loop_amen, beat_stretch: 2, rate: r, amp: 2
  end
  sleep 2
end

live_loop :bass_drum do
  sample :bd_haus, cutoff: 70, amp: 1.5
  sleep 0.5
end

live_loop :landing do
  bass_line = (knit :e1, 3, [:c1, :c2].choose, 1)
  with_fx :slicer, phase: [0.25, 0.5].choose, invert_wave: 1, wave: 0 do
    s = synth :square, note: bass_line.tick, sustain: 4, cutoff: 60
    control s, cutoff_slide: 4, cutoff: 120
  end
  sleep 4
end
```
