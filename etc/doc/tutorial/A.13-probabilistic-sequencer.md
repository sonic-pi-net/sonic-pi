A.13 Code a Probabilistic Sequencer

# Code a Probabilistic Sequencer

In a previous episode of this Sonic Pi series we explored the power of
randomisation to introduce variety, surprise and change into our live
coded tracks and performances. For example, we randomly picked notes
from a scale to create never-ending melodies. Today we're going to learn
a new technique which uses randomisation for rhythm - probabilistic
beats!

## Probability

Before we can start making new beats and synth rhythms we need to take a
quick dive into the basics of probability. This might sound daunting and
complicated, but really it's just as simple as rolling a dice -
honestly!  When you take a regular 6 sided board game dice and roll it
what's actually happening? Well, firstly you'll roll either a 1, 2, 3,
4, 5 or 6 with exactly the same chance of getting any of the numbers. In
fact, given that it's a 6 sided dice, on average (if you roll lots and
lots of times) you'll throw a 1 every 6 throws. This means you have a 1
in 6 chance of throwing a 1. We can emulate dice rolls in Sonic Pi with
the fn `dice`. Let's roll one 8 times:

```
8.times do
  puts dice
  sleep 1
end
```

Notice how the log prints values between 1 and 6 just as if we'd rolled
a real dice ourselves.

## Random Beats

Now imagine you had a drum and every time you were about to hit it you
rolled a dice. If you rolled a 1, you hit the drum and if you rolled any
other number you didn't. You now have a probabilistic drum machine
working with a probability of 1/6! Let's hear what that sounds like:

```
live_loop :random_beat do
  sample :drum_snare_hard if dice == 1
  sleep 0.125
end
```


Let's quickly go over each line to make sure everything is very
clear. First we create a new `live_loop` called `:random_beat` which
will continually repeat the two lines between `do` and `end`. The first
of these lines is a call to `sample` which will play a pre-recorded
sound (the `:drum_snare_hard` sound in this case). However, this line
has a special conditional `if` ending. This means that the line will
only be executed if the statement on the right hand side of the `if` is
`true`. The statement in this case is `dice == 1`. This calls our `dice`
function which, as we have seen, returns a value between 1 and 6. We
then use the equality operator `==` to check to see if this value is
`1`. If it is `1`, then the statement resolves to `true` and our snare
drum sounds, if it isn't `1` then the statement resolves to `false` and
the snare is skipped. The second line simply waits for `0.125` seconds
before rolling the dice again.

## Changing Probabilities

Those of you that have played role play games will be familiar with lots
of strangely shaped dice with different ranges. For example there is the
tetrahedron shaped dice which has 4 sides and even a 20 sided dice in
the shape of a icosahedron. The number of sides on the dice changes the
chance, or probability of rolling a 1. The fewer sides, the more likely
you are to roll a 1 and the more sides the less likely. For example,
with a 4 sided dice, there's a one in 4 chance of rolling a 1 and with a
20 sided dice there's a one in 20 chance. Luckily, Sonic Pi has the
handy `one_in` fn for describing exactly this. Let's play:

```
live_loop :different_probabilities do
  sample :drum_snare_hard if one_in(6)
  sleep 0.125
end
```

Start the live loop above and you'll hear the familiar random
rhythm. However, don't stop the code running. Instead, change the `6` to
a different value such as `2` or `20` and hit the `Run` button
again. Notice that lower numbers mean the snare drum sounds more
frequently and higher numbers mean the snare triggers fewer
times. You're making music with probabilities!

## Combining Probabilities

Things get really exciting when you combine multiple samples being
triggered with different probabilities. For example:

```
live_loop :multi_beat do
  sample :elec_hi_snare if one_in(6)
  sample :drum_cymbal_closed if one_in(2)
  sample :drum_cymbal_pedal if one_in(3)
  sample :bd_haus if one_in(4)
  sleep 0.125
end
```

Again, run the code above and then start changing the probabilities to
modify the rhythm. Also, try changing the samples to create an entirely
new feel. For example try changing `:drum_cymbal_closed` to
`:bass_hit_c` for extra bass!


## Repeatable Rhythms

Next, we can use our old friend `use_random_seed` to reset the random
stream after 8 iterations to create a regular beat. Type the following
code to hear a much more regular and repeating rhythm. Once you hear the
beat, try changing the seed value from `1000` to another number. Notice
how different numbers generate different beats.

```
live_loop :multi_beat do
  use_random_seed 1000
  8.times do
    sample :elec_hi_snare if one_in(6)
    sample :drum_cymbal_closed if one_in(2)
    sample :drum_cymbal_pedal if one_in(3)
    sample :bd_haus if one_in(4)
    sleep 0.125
  end
end
```

One thing I tend to do with this kind of structure is to remember which
seeds sound good and make a note of them. That way I can easily
re-create my rhythms in future practice sessions or performances.

## Bringing it all together

Finally, we can throw in some random bass to give it some nice melodic
content. Notice that we can also use our newly discovered probabilistic
sequencing method on synths just as well as samples. Don't leave it at
that though - tweak the numbers and make your own track with the power
of probabilities!

```
live_loop :multi_beat do
  use_random_seed 2000
  8.times do
    c = rrand(70, 130)
    n = (scale :e1, :minor_pentatonic).take(3).choose
    synth :tb303, note: n, release: 0.1, cutoff: c if rand < 0.9
    sample :elec_hi_snare if one_in(6)
    sample :drum_cymbal_closed if one_in(2)
    sample :drum_cymbal_pedal if one_in(3)
    sample :bd_haus, amp: 1.5 if one_in(4)
    sleep 0.125
  end
end
```
