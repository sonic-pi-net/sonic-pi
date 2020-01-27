A.5 Acid Bass

# Acid Bass

It's impossible to look through the history of electronic dance music
without seeing the enormous impact of the tiny Roland TB-303
synthesiser. It's the secret sauce behind the original acid bass
sound. Those classic squealing and squelching TB-303 bass riffs can be
heard from the early Chicago House scene through to more recent
electronic artists such as Plastikman, Squarepusher and Aphex Twin.

Interestingly, Roland never intended for the TB-303 to be used in dance
music. It was originally created as a practice aid for guitarists. They
imagined that people would program them to play bass lines to jam along
to. Unfortunately there were a number of problems: they were a little
fiddly to program, didn't sound particularly good as a bass-guitar
replacement and were pretty expensive to buy. Deciding to cut their
losses, Roland stopped making them after 10,000 units were sold and
after a number of years sitting on guitarist's shelves, they soon could
be found in the windows of second hand shops. These lonely discarded
TB-303s were waiting to be discovered by a new generation of
experimenters who started using them in ways that Roland didn't imagine
to create new crazy sounds. Acid House was born.

Although getting your hands on an original TB-303 is not so easy you
will be pleased to know that you can turn your Raspberry Pi into one
using the power of Sonic Pi. Behold, fire up Sonic Pi and throw this
code into an empty buffer and hit Run:

```
use_synth :tb303
play :e1
```
    
Instant acid bass! Let's play around...

## Squelch that Bass

First, let's build a live arpeggiator to make things fun. In the last
tutorial we looked at how riffs can just be a ring of notes that we tick
through one after another, repeating when we get to the end. Let's
create a live loop that does exactly that:

```
use_synth :tb303
live_loop :squelch do
  n = (ring :e1, :e2, :e3).tick
  play n, release: 0.125, cutoff: 100, res: 0.8, wave: 0
  sleep 0.125
end
```

Take a look at each line. 

1. On the first line we set the default synth to be `tb303` with the
  `use_synth` fn.

2. On line two we create a live loop called `:squelch` which will just
   loop round and round.

3. Line three is where we create our riff - a ring of notes (E in
   octaves 1, 2, and 3) which we simply tick through with `.tick`. We
   define `n` to represent the current note in the riff. The equals sign
   just means to assign the value on the right to the name on the
   left. This will be different every time round the loop. The first
   time round, `n` will be set to `:e1`. The second time round it will
   be `:e2`, followed by `:e3`, and then back to `:e1`, cycling round
   forever.
   
4. Line four is where we actually trigger our `:tb303` synth. We're
   passing a few interesting opts here: `release:`, `cutoff:`, `res:`
   and `wave:` which we'll discuss below.
   
5. Line five is our `sleep` - we're asking the live loop to loop round
   every `0.125`s or 8 times a second at the default BPM of 60.
   
6. Line six is the `end` to the live loop. This just tells Sonic Pi
   where the end of the live loop is.

Whilst you're still figuring out what's going on, type in the code above
and hit the Run button. You should hear the `:tb303` kick into
action. Now, this is where the action is: let's start live coding.

Whilst the loop is still live, change the `cutoff:` opt to `110`. Now
hit the Run button again. You should hear the sound become a little
harsher and more squelchy. Dial in `120` and hit run. Now `130`. Listen
how higher cutoff values make it sound more piercing and
intense. Finally, drop it down to `80` when you feel like a rest. Then
repeat as many times as you want. Don't worry, I'll still be here...

Another opt worth playing with is `res:`. This controls the level of
resonance of the filter. A high resonance is characteristic of acid bass
sounds. We currently have our `res:` set to `0.8`. Try cranking it up to
`0.85`, then `0.9`, and finally `0.95`. You might find that a cutoff
such as `110` or higher will make the differences easier to
hear. Finally go crazy and dial in `0.999` for some insane sounds. At a
`res` this high, you're hearing the cutoff filter resonate so much it
starts to make sounds of its own!

Finally, for a big impact on the timbre try changing the `wave:` opt to
`1`. This is the choice of source oscillator. The default is `0` which
is a sawtooth wave. `1` is a pulse wave and `2` is a triangle wave.

Of course, try different riffs by changing the notes in the ring or even
picking notes from scales or chords. Have fun with your first acid bass
synth.

## Deconstructing the TB-303

The design of the original TB-303 is actually pretty simple. As you can
see from the following diagram there's only 4 core parts. 

![TB-303 Design](../../../etc/doc/images/tutorial/articles/A.05-acid-bass/tb303-design.png)

First is the oscillator wave - the raw ingredients of the sound. In this
case we have a square wave. Next there's the oscillator's amplitude
envelope which controls the amp of the square wave through time. These
are accessed in Sonic Pi by the `attack:`, `decay:`, `sustain:` and
`release:` opts along with their level counterparts. For more
information read Section 2.4 'Duration with Envelopes' in the built-in
tutorial. We then pass our enveloped square wave through a resonant low
pass filter. This chops off the higher frequencies as well as having
that nice resonance effect. Now this is where the fun starts. The cutoff
value of this filter is also controlled by its own envelope! This means
we have amazing control over the timbre of the sound by playing with
both of these envelopes. Let's take a look:

```
use_synth :tb303
with_fx :reverb, room: 1 do
  live_loop :space_scanner do
    play :e1, cutoff: 100, release: 7, attack: 1, cutoff_attack: 4, cutoff_release: 4
    sleep 8
  end
end
```
    
For each standard envelope opt, there's a `cutoff_` equivalent opt in
the `:tb303` synth. So, to change the cutoff attack time we can use the
`cutoff_attack:` opt. Copy the code above into an empty buffer and hit
Run. You'll hear a crazy sound warble in and out. Now start to play. Try
changing the `cutoff_attack:` time to `1` and then `0.5`. Now try `8`.

Notice that I've passed everything through a `:reverb` FX for extra
atmosphere - try other FX to see what works!

## Bringing it all together

Finally, here's a piece I composed using the ideas in this
tutorial. Copy it into an empty buffer, listen for a while and then
start live coding your own changes. See what crazy sounds you can make
with it! See you next time...

```
use_synth :tb303
use_debug false
 
with_fx :reverb, room: 0.8 do
  live_loop :space_scanner do
    with_fx :slicer, phase: 0.25, amp: 1.5 do
      co = (line 70, 130, steps: 8).tick
      play :e1, cutoff: co, release: 7, attack: 1, cutoff_attack: 4, cutoff_release: 4
      sleep 8
    end
  end
 
  live_loop :squelch do
    use_random_seed 3000
    16.times do
      n = (ring :e1, :e2, :e3).tick
      play n, release: 0.125, cutoff: rrand(70, 130), res: 0.9, wave: 1, amp: 0.8
      sleep 0.125
    end
  end
end
```
