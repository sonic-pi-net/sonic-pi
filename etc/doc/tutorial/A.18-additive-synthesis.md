A.18 Sound Design - Additive Synthesis

# Additive Synthesis

This is the first of a short series of articles on how to use Sonic Pi
for sound design. We'll be take a quick tour of a number of different
techniques available for you to craft your own unique sound. The first
technique we'll look at is called _additive synthesis_. This may sound
complicated - but if we expand each word slightly the meaning pops right
out. Firstly, additive means a combination of things and secondly
synthesis means to create sound. Additive synthesis therefore means
nothing more complicated than _combining existing sounds to create new
ones_.  This synthesis technique dates back a very long time - for
example, pipe organs in the middle ages had lots of slightly different
sounding pipes which you could enable or disable with stops. Pulling out
the stop for a given pipe 'added it to the mix' making the sound richer
and more complex. Now, let's see how we can pull out all the stops with
Sonic Pi.


## Simple Combinations

Let's start with the most basic sound there is - the humble pure-toned
sine wave:

```
synth :sine, note: :d3
```

Now, let's see how this sounds combined with a square wave:

```
synth :sine, note: :d3
synth :square, note: :d3
```

Notice how the two sounds combine to form a new, richer sound. Of
course, we don't have to stop there, we can add as many sounds as we
need. However, we need to be careful with how many sounds we add
together. Just like when we mix paints to create new colours, adding too
many colours will result in a messy brown, similarly - adding too many
sounds together will result in a muddy sound.


## Blending

Let's add something to make it sound a little brighter. We could
use a triangle wave at an octave higher (for that high bright sound) yet
only play it at amp `0.4` so it adds something extra to the sound rather
than taking it over:

```
synth :sine, note: :d3
synth :square, note: :d3
synth :tri, note: :d4, amp: 0.4
```

Now, try creating your own sounds by combining 2 or more synths at
different octaves and amplitudes. Also, note that you can play around
with each synth's opts to modify each source sound before it is mixed in
for even more combinations of sounds.


## Detuning

So far, when combining our different synths we've used either the same
pitch or switched octave. How might it sound if we didn't stick to
octaves but instead chose a slightly higher or lower note? Let's try it:

```
detune = 0.7
synth :square, note: :e3
synth :square, note: :e3 + detune
```

If we detune our square waves by 0.7 notes we hear something that
perhaps doesn't sound in tune or correct - a 'bad' note. However, as we
move closer to 0 it will sound less and less out of tune as the pitches
of the two waves get closer and more similar. Try it for yourself!
Change the `detune:` opt value from `0.7` to `0.5` and listen to the new
sound. Try `0.2`, `0.1`, `0.05`, `0`. Each time you change the value,
take a listen and see if you can hear how the sound is changing. Notice
that low detune values such as `0.1` produce a really nice 'thick'
sound, with both slightly different pitches interacting with each other
in interesting, often surprising, ways.

Some of the built-in synths already include a detune option that do
exactly this in one synth. Try playing with the `detune:` opt of
`:dsaw`, `:dpulse` and `:dtri`.


## Amplitude shaping

Another way we can finely craft our sound is to use a different envelope
and options for each synth trigger. For example this will allow you to
make some aspects of the sound percussive and other aspects ring out for
a period of time.

```
detune = 0.1
synth :square, note: :e1, release: 2
synth :square, note: :e1 + detune, amp: 2, release: 2
synth :gnoise, release: 2, amp: 1, cutoff: 60
synth :gnoise, release: 0.5, amp: 1, cutoff: 100
synth :noise, release: 0.2, amp: 1, cutoff: 90
```

In the example above I have mixed in a noisy percussive element to the
sound along with some more persistent background rumbling. This was
achieved firstly by using two noise synths with middling cutoff values
(`90` and `100`) using short release times along with a noise with a
longer release time but with a low cutoff value (which makes the noise
less crisp and more rumbly.)

## Bringing it all together

Let's combine all these techniques to see if we can use additive
synthesis to re-create a basic bell sound. I've broken this example into
four sections. Firstly we have the 'hit' section which is the initial
onset part of the bell sound - so uses a short envelope (e.g. a
`release:` of around `0.1`). Next we have the long ringing section in
which I'm using the pure sound of the sine wave. Notice that I'm often
increasing the note by roughly `12` and `24` which are the number of
notes in one and two octaves. I have also thrown in a couple of low sine
waves to give the sound some bass and depth. Finally, I used `define` to
wrap my code in a function which I can then use to play a melody. Try
playing your own melody and also messing around with the contents of the
`:bell` function until you create your own crazy sound to play with!

```
define :bell do |n|
  # Triangle waves for the 'hit'
  synth :tri, note: n - 12, release: 0.1
  synth :tri, note: n + 0.1, release: 0.1
  synth :tri, note: n - 0.1, release: 0.1
  synth :tri, note: n, release: 0.2

  # Sine waves for the 'ringing'
  synth :sine, note: n + 24, release: 2
  synth :sine, note: n + 24.1, release: 2
  synth :sine, note: n + 24.2, release: 0.5
  synth :sine, note: n + 11.8, release: 2
  synth :sine, note: n, release: 2

  # Low sine waves for the bass
  synth :sine, note: n - 11.8, release: 2
  synth :sine, note: n - 12, release: 2
end

# Play a melody with our new bell!
bell :e3
sleep 1
bell :c2
sleep 1
bell :d3
sleep 1
bell :g2
```
