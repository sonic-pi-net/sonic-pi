A.9 Randomisation

# Surfing Random Streams

Back in episode 4 of this tutorial series we took a brief look at
randomisation whilst coding up some sizzling synth riffs. Given that
randomisation is such an important part of my live coding DJ sets I
thought it would be useful to cover the fundamentals in much greater
detail. So, get your lucky hat on and let's surf some random streams!

## There is no random

The first thing to learn which might really surprise you when playing
with Sonic Pi's randomisation functions is that they're not actually
really random. What does this actually mean? Well, let's try a couple of
tests. First, imagine a number in your head between 0 and 1. Keep it
there and don't tell me. Now let me guess... was it `0.321567`? No? Bah,
I'm clearly no good at this. Let me have another go, but let's ask Sonic
Pi to choose a number this time. Fire up Sonic Pi v2.7+ and ask it for a
random number but again don't tell me:

```
print rand
```

Now for the reveal... was it `0.75006103515625`? Yes! Ha, I can see
you're a little sceptical. Perhaps it was just a lucky guess. Let's try
again. Press the Run button again and see what we get... What?
`0.75006103515625` again? This clearly can't be random! You're right,
it's not.

What's going on here? The fancy computer science word here is
determinism. This just means that nothing is by chance and everything is
destined to be. Your version of Sonic Pi is destined to always return
`0.75006103515625` in the program above. This might sound pretty
useless, but let me assure you that it's one of the most powerful parts
of Sonic Pi. If you stick at it you'll learn how to rely on the
deterministic nature of Sonic Pi's randomisation as a fundamental
building block for your compositions and live coded DJ sets.

## A Random Melody

When Sonic Pi boots it actually loads into memory a sequence of 441,000
pre-generated random values. When you call a random function such as
`rand` or `rrand`, this random stream is used to generate your
result. Each call to a random function consumes a value from this
stream. Therefore the 10th call to a random function will use the 10th
value from the stream. Also, every time you press the Run button, the
stream is reset for that run. This is why I could predict the result to
`rand` and why the 'random' melody was the same every time. Everybody's
version of Sonic Pi uses the exact same random stream which is very
important when we start sharing our pieces with each other. 

Let's use this knowledge to generate a repeatable random melody:

```
8.times do
 play rrand_i(50, 95)
 sleep 0.125
end
```

Type this into a spare buffer and hit Run. You'll hear a melody
consisting of 'random' notes between 50 and 95. When it's finished, hit
Run again to hear exactly the same melody again.

## Handy Randomisation Functions

Sonic Pi comes with a number of useful functions for working with the
random stream. Here's a list of some of the most useful:

* `rand` - Simply returns the next value in the random stream
* `rrand` - Returns a random value within a range
* `rrand_i` - Returns a random whole number within a range
* `one_in` - Returns true or false with the given probability
* `dice` - Imitates rolling a dice and returns a value between 1 and 6
* `choose` - Chooses a random value from a list

Check out their documentation in the Help system for detailed
information and examples.

## Resetting the Stream

Whilst the ability to repeat a sequence of chosen notes is essential to
allow you to replay a riff on the dance floor, it might not be exactly
the riff you want. Wouldn't it be great if we could try a number of
different riffs and choose the one we liked best? This is where the real
magic starts.

We can manually set the stream with the fn `use_random_seed`. In
Computer Science, a random seed is the starting point from which a new
stream of random values can sprout out and blossom. Let's try it:

```
use_random_seed 0
3.times do
  play rrand_i(50, 95)
  sleep 0.125
end
```

Great, we get the first three notes of our random melody above: `84`,
`83` and `71`. However, we can now change the seed to something
else. How about this:

```
use_random_seed 1
3.times do
  play rrand_i(50, 95)
  sleep 0.125
end
```

Interesting, we get `83`, `71` and `61` . You might notice that the
first two numbers here are the same as the last two numbers before -
this isn't a coincidence.

Remember that the random stream is just a giant list of 'pre-rolled'
values. Using a random seed simply jumps us to a point in that
list. Another way of thinking about it is to imagine a huge deck of
pre-shuffled cards. Using a random seed is cutting the deck at a
particular point. The fabulous part of this is that it's precisely this
ability to jump around the random stream which gives us huge power when making
music.

Let's revisit our random melody of 8 notes with this new stream
resetting power, but let's also throw in a live loop so we can
experiment live whilst it's playing:

```
live_loop :random_riff do    
  use_random_seed 0
  8.times do
    play rrand_i(50, 95), release: 0.1
    sleep 0.125
  end
end
```
  
Now, whilst it's still playing, change the seed value from `0` to
something else. Try `100`, what about `999`. Try your own values,
experiment and play around - see which seed generates the riff you like
best.

## Bringing it all together

This month's tutorial has been quite a technical dive into the workings
of Sonic Pi's randomisation functionality. Hopefully it has given you
some insight into how it works and how you can start using randomisation
in a reliable way to create repeatable patterns within your music. It's
important to stress that you can use repeatable randomisation *anywhere*
you want. For example, you can randomise the amplitude of notes, the
timing of the rhythm, the amount of reverb, the current synth, the mix
of an FX, etc. etc. In the future we'll take a close look at some of
these applications, but for now let me leave you with a short example.

Type the following into a spare buffer, hit Run, and then start changing
the seeds around, hit Run again (whilst it's still playing) and explore
the different sounds, rhythms and melodies you can make. When you find a
nice one, remember the seed number so you can get back to it. Finally,
when you've found a few seeds you like, put on a live coded performance
for your friends by simply switching between your favourite seeds to
create a full piece.

```
live_loop :random_riff do
  use_random_seed 10300
  use_synth :prophet
  s = [0.125, 0.25, 0.5].choose
  8.times do
    r = [0.125, 0.25, 1, 2].choose
    n = (scale :e3, :minor).choose
    co = rrand(30, 100)
    play n, release: r, cutoff: co
    sleep s
  end
end

live_loop :drums do
  use_random_seed 2001
  16.times do
    r = rrand(0.5, 10)
    sample :drum_bass_hard, rate: r, amp: rand
    sleep 0.125
  end
end
```
