A.11 Tick Tock

# Tracking the Beat

Last month in this series we took a deep technical dive into the
randomisation system underpinning Sonic Pi. We explored how we can use
it to deterministically add new levels of dynamic control over our
code. This month we're going to continue our technical dive and turn our
attention to Sonic Pi's unique tick system. By the end of this article
you'll be ticking your way through rhythms and riffs on your way to
being a live coding DJ.

## Beat Counting

When making music we often want to do a different thing depending on
which beat it is. Sonic Pi has a special beat counting system called
`tick` to give you precise control over when a beat actually occurs and
even supports multiple beats with their own tempos. 

Let's have a play - to advance the beat we just need to call
`tick`. Open up a fresh buffer, type in the following and hit Run:

```
puts tick #=> 0
```

This will return the current beat: `0`. Notice that even if you press
the Run button a few times it will always return `0`. This is because
each run starts a fresh beat counting from 0.  However, whilst the run
is still active, we can advance the beat as many times as we want:

```
puts tick #=> 0
puts tick #=> 1
puts tick #=> 2
```

Whenever you see the symbol `#=>` at the end of a line of
code it means that that line will log the text on the
right-hand-side. For example, `puts foo #=> 0` means the code `puts foo`
prints `0` to the log at that point in the program.

## Checking the Beat

We've seen that `tick` does two things. It increments (adds one)
and returns the current beat. Sometimes we just want to look at the
current beat without having to increment it which we can do via `look`:

```
puts tick #=> 0
puts tick #=> 1
puts look #=> 1
puts look #=> 1
``` 

In this code we tick the beat up twice and then call `look` twice. We'll
see the following values in the log: `0`, `1`, `1`, `1`. The first two
`tick`s returned `0`, then `1` as expected, then the two `look`s just
returned the last beat value twice which was `1`. 


## Rings

So now we can advance the beat with `tick` and check the beat with
`look`. What next? We need something to tick over. Sonic Pi uses rings
for representing riffs, melodies and rhythms and the tick system has
been specifically designed to work very closely with them. In fact,
rings have their own dot version of `tick` which does two things. Firstly,
it acts like a regular tick and increments the beat. Secondly it looks
up the ring value using the beat as the index. Let's take a look:

```
puts (ring :a, :b, :c).tick #=> :a
```

`.tick` is a special dot version of `tick` which will return the first
value of the ring `:a`. We can grab each of the values in the ring by
calling `.tick` multiple times:

```
puts (ring :a, :b, :c).tick #=> :a
puts (ring :a, :b, :c).tick #=> :b
puts (ring :a, :b, :c).tick #=> :c
puts (ring :a, :b, :c).tick #=> :a
puts look                   #=> 3
```

Take a look at the log and you'll see `:a`, `:b`, `:c` and then `:a`
again. Notice that `look` returns `3`. Calls to `.tick` act just like
they are regular calls to `tick` - they increment the local beat.


## A Live Loop Arpeggiator 

The real power comes when you mix `tick` with rings and
`live_loop`s. When combined we have all the tools we need to both build
and understand a simple arpegiator. We need just four things:

1. A ring containing the notes we want to loop over.
2. A means of incrementing and obtaining the beat. 
3. The ability to play a note based on the current beat.
4. A loop structure to keep the arpegiator repeating.

These concepts can all be found in the following code:

```
notes = (ring 57, 62, 55, 59, 64)

live_loop :arp do
  use_synth :dpulse
  play notes.tick, release: 0.2
  sleep 0.125
end
```

Let's look at each of these lines. First we define our ring of notes
which we'll continually play. We then create a `live_loop` called `:arp`
which loops round for us. Each time round the `live_loop` we set our
synth to `:dpulse` and then play the next note in our ring using
`.tick`. Remember that this will increment our beat counter and use the
latest beat value as an index into our notes ring. Finally, we wait for
an eighth of a beat before looping round again.

## Multiple Simultaneous Beats

A really important thing to know is that `tick`s are local to the
`live_loop`. This means that each `live_loop` has its own independent
beat counter. This is much more powerful than having a global metronome
and beat. Let's take a look at this in action:

```
notes = (ring 57, 62, 55, 59, 64)

with_fx :reverb do
  live_loop :arp do
    use_synth :dpulse
    play notes.tick + 12, release: 0.1
    sleep 0.125
  end
end

live_loop :arp2 do
  use_synth :dsaw
  play notes.tick - 12, release: 0.2
  sleep 0.75
end
```

## Clashing Beats

A big cause of confusion with Sonic Pi's tick system is when people want
to tick over multiple rings in the same `live_loop`:

```
use_bpm 300
use_synth :blade
live_loop :foo do
  play (ring :e1, :e2, :e3).tick
  play (scale :e3, :minor_pentatonic).tick
  sleep 1
end
```

Even though each `live_loop` has its own independent beat counter, we're
calling `.tick` twice within the same `live_loop`. This means that the
beat will be incremented twice every time round. This can produce some
interesting polyrhythms but is often not what you want. There are two
solutions to this problem. One option is to manually call `tick` at the
start of the `live_loop` and then use `.look` to look up the current
beat in each `live_loop`. The second solution is to pass a unique name
to each call to `.tick` such as `.tick(:foo)`. Sonic Pi will then create
and track a separate beat counter for each named tick you use. That way
you can work with as many beats as you need! See the section on named
ticks in 9.4 of the built-in tutorial for more information.

## Bringing it all together

Let's bring all this knowledge of `tick`s, `ring`s and `live_loop`s
together for a final fun example. As usual, don't treat this as a
finished piece. Start changing things and play around with it and see
what you can turn it into. See you next time...

```
use_bpm 240
notes = (scale :e3, :minor_pentatonic).shuffle

live_loop :foo do
  use_synth :blade
  with_fx :reverb, reps: 8, room: 1 do
    tick
    co = (line 70, 130, steps: 32).tick(:cutoff)
    play (octs :e3, 3).look, cutoff: co, amp: 2
    play notes.look, amp: 4
    sleep 1
  end
end

live_loop :bar do
  tick
  sample :bd_ada if (spread 1, 4).look
  use_synth :tb303
  co = (line 70, 130, steps: 16).look
  r = (line 0.1, 0.5, steps: 64).mirror.look
  play notes.look, release: r, cutoff: co
  sleep 0.5
end
```
