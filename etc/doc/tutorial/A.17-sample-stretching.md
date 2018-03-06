A.17 Sample Stretching

# Sample Stretching

When people discover Sonic Pi, one of the first things they learn is how
simple it is to play pre-recorded sounds using the `sample`
function. For example, you can play an industrial drum loop, hear the
sound of a choir or even listen to a vinyl scratch all via a single line
of code. However, many people don't realise that you can actually vary
the speed that the sample is played back at for some powerful effects and a
whole new level of control over your recorded sounds. So, fire up a copy of
Sonic Pi and let's get started stretching some samples!

## Slowing Samples Down

To modify the playback rate of a sample we need to use the `rate:` opt:

    sample :guit_em9, rate: 1
    
If we specify a `rate:` of `1` then the sample is played back at the
normal rate. If we want to play it back at half speed we simply
use a `rate:` of `0.5`:


    sample :guit_em9, rate: 0.5
    
Notice that this has two effects on the audio. Firstly the sample sounds
lower in pitch and secondly it takes twice as long to play back (see the
sidebar for an explanation of why this is the case). We can even choose
lower and lower rates moving towards `0`, so a `rate:` of `0.25` is a
quarter speed, `0.1` is a tenth of the speed, etc. Try playing with some
low rates and see if you can turn the sound into a low rumble.

## Speeding Samples Up

In addition to making the sound longer and lower using a small rate, we
can use higher rates to make the sound shorter and higher. Let's play
with a drum loop this time. First, take a listen to how it sounds at the
default rate of `1`:

    sample :loop_amen, rate: 1


Now, let's speed it up a little:

    sample :loop_amen, rate: 1.5
    
Ha! We just moved musical genres from old-skool techno to jungle. Notice
how the pitch of each drum hit is higher as well as how the whole rhythm
speeds up. Now, try even higher rates and see how high and short you
can make the drum loop. For example, if you use a rate of `100`, the
drum loop turns into a click!

## Reverse Gear

Now, I'm sure many of you are thinking the same thing right now... "what
if you use a negative number for the rate?". Great question! Let's think
about this for a moment. If our `rate:` opt signifies the speed with
which the sample is played back, `1` being normal speed, `2` being
double speed, `0.5` being half speed, `-1` must mean backwards! Let's
try it on a snare. First, play it back at the normal rate:

    sample :elec_filt_snare, rate: 1
    
Now, play it backwards:    

    sample :elec_filt_snare, rate: -1
    
Of course, you can play it backwards twice as fast with a rate of `-2`
or backwards at half speed with a rate of `-0.5`. Now, play around with
different negative rates and have fun. It's particularly amusing with
the `:misc_burp` sample!


## Sample, Rate and Pitch

One of the effects of rate modification on samples is that faster rates
result in the sample sounding higher in pitch and slower rates result in
the sample sounding lower in pitch. Another place you may
have heard this effect in every day life is when you're cycling or
driving past a beeping pedestrian crossing - as you're heading towards
the sound source the pitch is higher than when you're moving away from the
sound - the so-called Doppler effect. Why is this?

Let's consider a simple beep which is represented by a sine wave. If we
use an oscilloscope to plot a beep, we'll see something like Figure A.
If we plot a beep an octave higher, we'll see Figure B and an octave
lower will look like Figure C. Notice that the waves of higher notes are
more compact and the waves of lower notes are more spread out. 

A sample of a beep is nothing more than a lot of numbers (x, y,
coordinates) which when plotted onto a graph will re-draw the original
curves. See figure D where each circle represents a coordinate. To turn
the coordinates back into audio, the computer works through each x value
and sends the corresponding y value to the speakers. The trick here is
that the rate at which the computer works through the x numbers does not
have to be the same as the rate with which they were recorded. In other
words, the space (representing an amount of time) between each circle
can be stretched or compressed. So, if the computer walks through the x
values faster than the original rate, it will have the effect of
squashing the circles closer together which will result in a higher
sounding beep. It will also make the beep shorter as we will work
through all the circles faster. This is shown in Figure E.

Finally, one last thing to know is that a mathematician called Fourier
proved that any sound is actually lots and lots of sine waves all
combined together. Therefore, when we compress and stretch any recorded
sound we're actually stretching and compressing many sine waves all at
the same time in exactly this manner.

## Pitch Bending

As we've seen, using a faster rate will make the sound higher in pitch
and a slower rate will make the sound lower in pitch. A very simple and
useful trick is to know that doubling the rate actually results in the
pitch being an octave higher and inversely halving the rate results in
the pitch being an octave lower. This means that for melodic samples,
playing it alongside itself at double/half rates actually sounds rather
nice:

```
sample :bass_trance_c, rate: 1
sample :bass_trance_c, rate: 2
sample :bass_trance_c, rate: 0.5
```
    
However, what if we just want to alter the rate such that the pitch goes
up one semitone (one note up on a piano)? Sonic Pi makes this very easy
via the `rpitch:` opt:

```
sample :bass_trance_c
sample :bass_trance_c, rpitch: 3
sample :bass_trance_c, rpitch: 7
```
    
If you take a look at the log on the right, you'll notice that an
`rpitch:` of `3` actually corresponds to a rate of `1.1892` and a
`rpitch:` of `7` corresponds to a rate of `1.4983`. Finally, we can even
combine `rate:` and `rpitch:` opts:

```
sample :ambi_choir, rate: 0.25, rpitch: 3
sleep 3
sample :ambi_choir, rate: 0.25, rpitch: 5
sleep 2
sample :ambi_choir, rate: 0.25, rpitch: 6
sleep 1
sample :ambi_choir, rate: 0.25, rpitch: 1
```
    

## Bringing it all together    

Let's take a look at a simple piece which combines these ideas. Copy it
into an empty Sonic Pi buffer, hit play, listen to it for a while and
then use it as a starting point for your own piece. See how much fun it
is to manipulate the playback rate of samples. As an added exercise try
recording your own sounds and play around with the rate to see what
crazy sounds you can make.

```
live_loop :beats do
  sample :guit_em9, rate: [0.25, 0.5, -1].choose, amp: 2
  sample :loop_garzul, rate: [0.5, 1].choose
  sleep 8
end
 
live_loop :melody do
  oct = [-1, 1, 2].choose * 12
  with_fx :reverb, amp: 2 do
    16.times do
      n = (scale 0, :minor_pentatonic).choose
      sample :bass_voxy_hit_c, rpitch: n + 4 + oct
      sleep 0.125
    end
  end
end
```







