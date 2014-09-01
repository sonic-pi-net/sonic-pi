# Switching Synths

So far we've had quite a lot of fun making beeps and playing samples. However, you're probably starting to get bored of the basic beep noise. Is that all Sonic Pi has to offer? Isn't there more to live coding life than beeps? Yes there is, and in this section we'll explore the exciting range of sounds that Sonic Pi has to offer.

## Synths

Sonic Pi has a range of instruments it calls synths which is *short for synthesisers*. Whereas samples represent pre-recorded sounds, synths are capable of generating new sounds depending on how you control them (which we'll explore later in this tutorial). Sonic Pi's synths are very powerful and expressive and you'll have a lot of fun playing with them. First, let's learn how to select the current synth to use.

## Buzzy saws and prophets

A fun sound is the *saw wave* - let's give it a try:

```
use_synth :saw
play 38
sleep 0.25
play 50
sleep 0.25
play 62
```

Let's try another sound - the *prophet*:

```
use_synth :prophet
play 38
sleep 0.25
play 50
sleep 0.25
play 62
```

How about combining two sounds. First one after another

```
use_synth :saw
play 38
sleep 0.25
play 50
sleep 0.25
use_synth :prophet
play 57

```

Now at the same time:

```
use_synth :tb303
play 38
use_synth :dsaw
play 50
sleep 0.25
use_synth :prophet
play 57
```


Notice that the `use_synth` command only affects the following calls to `play`. Think of it like a *big switch* - new calls to `play` will play whatever synth it's currently pointing to. You can move the switch to a new synth with `use_synth`.


## Discovering Synths

To see which synths Sonic Pi has for you to play with take a look at the Synths option in the far left vertical menu (above Samples). There are over 20 to choose from. Here are a few of my favourite:

* `:prophet`
* `:dsaw`
* `:fm`
* `:tb303`
* `:pulse`

Now play around with *switching synths during your music*. Have fun combining synths to make new sounds as well as using different synths for different sections of your music.


