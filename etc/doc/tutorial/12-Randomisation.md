# Randomisation

A great way to add some interest into your music is using some random numbers. Sonic Pi has some great functionality for adding randomness to your music, but before we start we need to learn a shocking truth: in Sonic Pi *random is not truly random*. What on earth does this mean? Well, let's see. 

## Repeatability

A really useful random function is `rrand` which will give you a random value between two numbers - a *min* and a *max*. (`rrand` is short for ranged random). Let's try playing a random note:

```
play rrand(50, 100)
```

Ooh, it played a random note. It played note `77.4407`. A nice random note between 50 and 100. Woah, wait, did I just predict the exact random note you got too? Something fishy is going on here. Try running the code again. What? It chose `77.4407` again? That can't be random!

The answer is that it is not truly random, it's pseudo-random. Sonic Pi will give you random-like numbers in a repeatable manner. This is very useful for ensuring that the music you create on your machine sounds identical on everybody else's machine - even if you use some randomness in your composition. 

Of course, in a given piece of music, if it 'randomly' chose `77.4407` every time, then it wouldn't be very interesting. However, it doesn't. Try the following:

```
loop do
  play rrand(50, 100)
  sleep 0.5
end 
```

Yes! It finally sounds random. Within a given *run* subsequent calls to random functions will return random values. However, the next run will produce exactly the same random values and sound exactly the same. It's as if all Sonic Pi code went back in time to exactly the same point every time the run button was pressed. It's the Groundhog Day of music!

## Haunted Bells

A lovely illustration of randomisation in action is the haunted bells example which loops the `:perc_bell` sample with a random rate and sleep time between bell sounds:

```
loop do
  sample :perc_bell, rate: (rrand 0.125, 1.5)
  sleep rrand(0, 2)
end
```  

## Random cutoff

Another fun example using randomisation is to modify the cutoff of a synth randomly. A great synth to try this out on is the `:tb303` emulator:

```
use_synth :tb303

loop do
  play 50, release: 0.1, cutoff: rrand(60, 120)
  sleep 0.125
end
```

## Random seeds

So, what if you don't like the randomisation Sonic Pi provides? Well it's totally possible to choose a different starting point via `use_random_seed`. The default seed happens to be 0, so choose a different seed for a different random experience!

Consider the following:

```
5.times do
  play rrand(50, 100)
  sleep 0.5
end
```

Every time you run this code, you'll hear the same sequence of 5 notes. To get a different sequence simply change the seed:

```
use_random_seed 40
5.times do
  play rrand(50, 100)
  sleep 0.5
end
```

This will produce a different sequence of 5 notes. By changing the seed and listening to the results you can find something that you like - and when you share it with others, they will hear exactly what you heard too.

Let's have a look at some other useful random functions.


## choose

A very common thing to do is to choose an item randomly from a list of known items. For example, I may want to play one note from the following: 60, 65 or 72. I can achieve this with `choose` which will let me choose an item from a list. First, I need to put my numbers in a list which is done by wrapping them with square brackets and separating them with commas: `[60, 65, 72]`, next I just need to pass them to `choose`:

```
choose([60, 65, 72])
```

Let's hear what that sounds like:

```
loop do
  play choose([60, 65, 72])
  sleep 1
end
```

## rrand

We've already seen `rrand`, but let's run over it again. It returns a random number between two values exclusively. That means it will never return either the top or bottom number - always something in between the two. The number will always be a float - meaning it's not a whole number but a fraction of a number. Examples of floats are returned by `rrand(20, 110)`

* 20.343235
* 12.324324
* 100.93423

## rrand_i

Occasionally you'll want a whole random number, not a float. This is where `rrand_i` comes into the rescue. It works similarly to `rrand` except it may return the min and max values as potential random values (which means it's inclusive rather than exclusive of the range). Examples of numbers returned by `rrand_i(20, 110)` are:

* 20.0
* 46.0
* 99.0

## rand

This will return a random float between 0 and the max value you specify. By default it will return a value between 0 and one. It's therefore useful for choosing random `amp:` values:


```
loop do
  play 60, amp: rand
  sleep 0.25
end
```

## rand_i

Similar to the relationship between `rrand_i` and `rrand`, `rand_i` will return a whole number between 0 and the max value you specify.

## dice

Sometimes you want to emulate a dice throw - this is a special case of `rrand_i` where the lower value is always 1. A call to `dice` requires you to specify the number of sides on the dice. A standard dice has 6 sides, so `dice(6)` will act very similarly - returning values of either 1, 2, 3, 4, 5, or 6. However, just like fantasy role-play games, you might find value in a 4 sided dice, or a 12 sided dice, or a 20 sided dice - perhaps even a 120 sided dice!

## one_in

Finally you may wish to emulate throwing a the top score of a dice such as a 6 in a standard dice. `one_in` therefore returns true with a probability of one in the number of sides on the dice. Therefore `one_in(6)` will return true with a probability of 1 in 6 or false otherwise. True and false values are very useful for `if` statements which we will cover in a subsequent section of this tutorial.

Now, go and jumble up your code with some randomness!
