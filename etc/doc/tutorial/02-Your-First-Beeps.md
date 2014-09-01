# Your First Beeps

OK, enough of the intros - let's get into some sound. Take a look at the following code:

```
play 50
```

This is where it all starts. Go ahead, copy and paste it into the code window at the top of the app (the big white space under the Run button). Now, press Run...

## Beep!

Intense. Press it again. And again. *And again...*

Woah, crazy, I'm sure you could keep doing that all day. But wait, before you lose yourself in an infinite stream of beeps, try changing the number:

```
play 55
```

Can you hear the difference? Try a lower number:

```
play 47

```

So, lower numbers make lower pitched beeps and higher numbers make higher pitched beeps. Just like on a piano, the keys at the lower part of the piano (the left hand side) play lower notes and the keys on the higher part of the piano (the right hand side) play higher notes. In fact, the numbers actually relate to notes on the piano. `play 47` actually means play the 47th note on the piano. Which means that `play 48` is one note up (the next note to the right). It just so happens that the 4th octave C is number 60. Go ahead and play it: `play 60`.

*Don't worry* if this means nothing to you - it didn't to me when I first started. All that matters right now is that you know that *low numbers make lower beeps* and *high numbers make higher beeps*. 

## Chords

Playing a note is quite fun, but playing many at the same time can be even better. Try it:

```
play 52
play 55
play 59
```

Jazzy! So, when you write multiple `play`s, they all play at the same time. Try it for yourself - which number sound good together? Which sound terrible? Experiment, explore and find out for yourself.

## Melody

So, playing notes and chords is fun - but how about a melody? What if you wanted to play one note after another and not at the same time? Well, that's easy, you just need to `sleep` between the notes:

```
play 52
sleep 1
play 55
sleep 1
play 59
```

How lovely, a little arpeggio. So what does the `1` mean in `sleep 1`? Well it means the *duration of the sleep*. It actually means sleep for one beat, but for now we can think about it as sleeping for 1 second. So, what if we wanted to make our arpeggio a little faster? Well, we need to use shorter sleep values. What about a half i.e. `0.5`:

```
play 52
sleep 0.5
play 55
sleep 0.5
play 59
```

Notice how it plays faster. Now, try for yourself, change the times - use different times and notes.

One thing to try is in-between notes such as `play 52.3` and `play 52.63`. There's absolutely no need to stick to standard whole notes. Play around and have fun.


## Traditional Note Names

For those of you that already know some musical notation (don't worry if you don't - you don't need it to have fun) you might want to write a melody using note names such as C and F# rather than numbers. Sonic Pi has you covered. You can do the following:

```
play :C
sleep 0.5
play :D
sleep 0.5
play :E
```

Remember to put the colon `:` in front of of your note name so that it goes pink. Also, you can specify the octave by adding a number after the note name:

```
play :C3
sleep 0.5
play :D3
sleep 0.5
play :E4
```

If you want to make a note sharp, add an `s` after the note name such as `play :Fs3` and if you want to make a note flat, add a `b` such as `play :Eb3`.

Now go *crazy* and have fun making your own tunes.





