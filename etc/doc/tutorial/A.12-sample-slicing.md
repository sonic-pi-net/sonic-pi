A.12 Sample Slicing

# Sample Slicing

Way back in episode 3 of this Sonic Pi series we looked at how to loop,
stretch and filter one of the most famous drum breaks of all time - the
Amen Break. In this tutorial we're going to take this one step further
and learn how to slice it up, shuffle the slices and glue it back
together in a completely new order. If that sounds a bit crazy to you,
don't worry, it will all become clear and you'll soon master a powerful
new tool for your live coded sets.

## Sound as Data

Before we get started let's just take a brief moment to understand how
to work with samples. By now, you've all hopefully played with Sonic
Pi's powerful sampler. If not, there's no time like the present! Boot up
your Raspberry Pi, launch Sonic Pi from the Programming menu, type the
following into a fresh buffer and then hit the Run button to hear a
pre-recorded drum beat:

```
sample :loop_amen
```

A recording of a sound is simply represented as data - lots of numbers 
between -1 and 1 which represent the peaks and troughs of the sound 
wave. If we play those numbers back in order, we get the original 
sound. However, what's to stop us from playing them back in a different 
order and creating a new sound?

How are samples actually recorded? It's actually pretty simple once you 
understand the basic physics of sound. When you make a sound - for 
example by hitting a drum, the noise travels through the air in a 
similar fashion to how the surface of a lake ripples when you throw a 
pebble into it. When those ripples reach your ears, your eardrum moves 
sympathetically and converts those movements into the sound you hear. 
If we wish to record and play back the sound, we therefore need a way 
of capturing, storing and reproducing those ripples. One way is to use 
a microphone which acts like an eardrum and moves back and forth as the 
sound ripples hit it. The microphone then converts its position into a 
tiny electric signal which is then measured many times a second. These 
measurements are then represented as a series of numbers between -1 and 
1.

If we were to plot a visualisation of the sound it would be a simple 
graph of data with time on the x axis and microphone/speaker position 
as a value between -1 and 1 on the y axis. You can see an example of 
such a graph at the top of the diagram.

## Playing Part of a Sample

So, how do we code Sonic Pi to play a sample back in a different order?
To answer this question we need to take a look at the `start:` and
`finish:` opts for `sample`. These let us control the start and finish
positions of our playback of the numbers which represent the sound. The
values for both of these opts are represented as a number between `0` and
`1` where `0` represents the start of the sample and `1` is the end. So,
to play the first half of the Amen Break, we just need to specify a
`finish:` of `0.5`:

```
sample :loop_amen, finish: 0.5
```

We can add in a `start:` value to play an even smaller section of the sample:

```
sample :loop_amen, start: 0.25, finish: 0.5
```

For fun, you can even have the `finish:` opt's value be *before*
`start:` and it will play the section backwards:

```
sample :loop_amen, start: 0.5, finish: 0.25
```

## Re-ordering Sample Playback

Now that we know that a sample is simply a list of numbers that can be
played back in any order and also how to play a specific part of a
sample we can now start having fun playing a sample back in the 'wrong'
order.

![Amen Slices](../../../etc/doc/images/tutorial/articles/A.12-sample-slicing/amen_slice.png)

Let's take our Amen Break and chop it up into 8 equally-sized slices and
then shuffle the pieces around. Take a look at the diagram: at the top
A) represents the graph of our original sample data. Chopping it into 8
slices gives us B) - notice that we've given each slice a different
colour to help distinguish them. You can see each slice's start and
finish values at the top. Finally C) is one possible re-ordering of the
slices. We can then play this back to create a new beat. Take a look at
the code to do this:

```
live_loop :beat_slicer do
  slice_idx = rand_i(8)
  slice_size = 0.125
  s = slice_idx * slice_size
  f = s + slice_size
  sample :loop_amen, start: s, finish: f
  sleep sample_duration :loop_amen, start: s, finish: f
end
```

1. we choose a random slice to play which should be a random number
   between 0 and 7 (remember that we start counting at 0). Sonic Pi has
   a handy function for exactly this: `rand_i(8)`. We then store this
   random slice index in the variable `slice_idx`.
   
2. We define our `slice_size` which is 1/8 or 0.125. The `slice_size` is
   necessary for us to convert our `slice_idx` into a value between 0
   and 1 so we can use it as our `start:` opt.

3. We calculate the start position `s` by multiplying the `slice_idx` by
   the `slice_size`.
 
4. We calculate the finish position `f` by adding the `slice_size` to
   the start position `s`.

5. We can now play the sample slice by plugging in the `s` and `f`
   values into the `start:` and `finish:` opts for `sample`.

6. Before we play the next slice we need to know how long to `sleep`
   which should be the duration of the sample slice.  Luckily, Sonic Pi
   has us covered with `sample_duration` which accepts all the same opts
   as `sample` and simply returns the duration. Therefore, by passing
   `sample_duration` our `start:` and `finish:` opts, we can find out
   the duration of a single slice.

7. We wrap all of this code in a `live_loop` so that we continue to pick
   new random slices to play.


## Bringing it all together

Let's combine everything we've seen so far into a final example which
demonstrates how we can take a similar approach to combine randomly
sliced beats with some bass to create the start of an interesting
track. Now it's your turn - take the code below as a starting point and
see if you can take it in your own direction and create something new...

```
live_loop :sliced_amen do
  n = 8
  s =  line(0, 1, steps: n).choose
  f = s + (1.0 / n)
  sample :loop_amen, beat_stretch: 2, start: s, finish: f
  sleep 2.0  / n
end

live_loop :acid_bass do
  with_fx :reverb, room: 1, reps: 32, amp: 0.6 do
    tick
    n = (octs :e0, 3).look - (knit 0, 3 * 8, -4, 3 * 8).look
    co = rrand(70, 110)
    synth :beep, note: n + 36, release: 0.1, wave: 0, cutoff: co
    synth :tb303, note: n, release: 0.2, wave: 0, cutoff: co
    sleep (ring 0.125, 0.25).look
  end
end
```
