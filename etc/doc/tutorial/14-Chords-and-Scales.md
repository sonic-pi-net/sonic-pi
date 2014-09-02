# Lists: Chords and Scales

In this section we'll take a look at a datastructure which is very useful - the list. We met it very briefly before in the section on randomisation when we randomly chose from a list of notes to play:

```
play choose([50, 55, 62])
```

In this section we'll explore using lists to also represent both chords and scales. First let's recap how we might play a chord. Remember that if we don't use `sleep`, sounds all happen at the same time: 

```
play 52
play 55
play 59
```

Let's look at other ways to represent this code.

## Chords

One option is to place all the notes in a list: `[52, 55, 59]`. Our friendly `play` function is smart enough to know how to play a list of notes. Try it:

```
play [52, 55, 59]
```

Ooh, that's already nicer to read. Playing a list of notes doesn't stop you from using any of the parameters as normal:

```
play [52, 55, 59], amp: 0.3
```

Of course, you can also use the traditional note names instead of the MIDI numbers:

```
play [:E3, :G3, :B3]
```

Now those of you lucky enough to have studied some music might recognise that chord as *E Minor* played in the 3rd octave. Sonic Pi has built-in support for chord names like this. Try it for yourself:

```
play chord(:E3, :minor)
```

Now, were really getting somewhere. That looks a lot more pretty. So what other chords does Sonic Pi support? Well, a *lot*. Try some of these:


* `chord(:E3, :m7)`
* `chord(:E3, :minor)`
* `chord(:E3, :dim7)`
* `chord(:E3, :dom7)`

## Arpeggios

We can easily turn chords into arpeggios with the function `play_pattern`:

```
play_pattern chord(:E3, :m7)
```

Ok, that's not so fun - it played it really slowly. `play_pattern` will play each note in the list separated with a call to `sleep 1` between each call to `play`. We can use another function `play_pattern_timed` to specify our own timings and speed things up:

```
play_pattern_timed chord(:E3, :m7), 0.25
```

We can even pass a list of times which it will treat as a circle of times:

```
play_pattern_timed chord(:E3, :m13), [0.25, 0.5]
```

This is the equivalent to:

```
play 52
sleep 0.25
play 55
sleep 0.5
play 59
sleep 0.25
play 62
sleep 0.5
play 66
sleep 0.25
play 69
sleep 0.5
play 73
```

Which would you prefer to write?

## Scales

Finally Sonic Pi also has support for a wide range of scales. How about playing a C3 major scale?

```
play_pattern_timed chord(:c3, :major), 0.125, release: 0.1
```

We can even ask for more octaves:

```
play_pattern_timed scale(:c3, :major, num_octaves: 3), 0.125, release: 0.1
```

How about all the notes in a pentatonic scale?

```
play_pattern_timed scale(:c3, :major_pentatonic, num_octaves: 3), 0.125, release: 0.1
```

## Random notes

Chords and scales are great ways of constraining the a random choice to something meaningful. Have a play with this example which picks notes from random from the chord E3 minor:

```
use_synth :tb303
loop do
  play choose(chord(:E3, :minor)), release: 0.3, cutoff: rrand(60, 120)
  sleep 0.25
end
```

Try switching in different chord names and cutoff ranges.

## Discovering Chords and Scales

To find out which scales and chords are supported by Sonic Pi simply click the Lang button on the far left of this tutorial and then choose either chord or scale in the API list. In the information in the main panel, scroll down until you see a long list of chords or scales (depending on which you're looking at).

Have fun and remember, there are no mistakes, only opportunities.


