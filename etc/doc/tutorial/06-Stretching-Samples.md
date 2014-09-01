# Stretching Samples

Now we can play a variety of synths and samples to create some music it's time to learn how to modify both the synths and samples to make the music even more unique and interesting. First, let's explore the ability to *stretch* and *squash* samples.

## Sample Representation

Samples are pre-recorded sounds stored as numbers which represent how to move the speaker cone to reproduce the sound. The speaker cone can move in and out, and so the numbers just need to represent how far in and out the cone needs to be for each moment in time. To be able to faithfully reproduce a recorded sound the sample typically needs to store many thousands of numbers per second! Sonic Pi takes this list of numbers and feeds them at the right speed to move your computer's speaker in and out in just the right way to reproduce the sound. However, it's also fun to change the speed the numbers are fed to the speaker to change the sound.

## Changing Rate

Let's play with one of the ambient sounds: `:ambi_choir`. So to play it with the default rate, you can pass a `rate:` argument to `sample`:

```
sample :ambi_choir, rate: 1
```

This plays it at normal rate (1), so nothing special yet. However, we're free to change that number to something else. How about `0.5`:

```
sample :ambi_choir, rate: 0.5
```

Woah! What's going on here? Well, two things. Firstly, the sample takes twice as long to play, secondly the sound is an octave lower. Let's explore these things in a little more detail.

## Have a Play

sample that's fun to stretch and compress is the Amen Break. At normal rate, we might imagine throwing it into a *drum 'n' bass* track:

```
sample :loop_amen
```

However by changing rate we can switch up genres. Try half speed for *old school hip-hop*:

```
sample :loop_amen, rate: 0.5
```

If we speed it up, we enter *jungle* territory: 

```
sample :loop_amen, rate: 1.5
```

Now for our final party trick - let's see what happens if we use a negative rate:

```
sample :loop_amen, rate: -1
```

Woah! It plays it *backwards*! Now try playing with lots of different samples at different rates. Try very fast rates. Try crazy slow rates. See what interesting sounds you can produce.

## A Simple Explanation of Sample Rate

A useful way to think of samples is as springs. Playback rate is like squashing and stretching the spring. If you play the sample at rate 2, you're *squashing the spring* to half its normal length. The sample therefore takes half the amount of time to play as it's shorter. If you play the sample at half rate, you're *stretching the spring* to double its length. The sample therefore takes twice the amount of time to play as it's longer. The more you squash (higher rate), the shorter it gets, the more you stretch (lower rate), the longer it gets.

Compressing a spring increases its density (the number of coils per cm) - this is similar to the sample sounding *higher pitched*. Stretching the spring decreases its density and is similar to the sound having a *lower pitch*.


## The Maths Behind Sample Rate

(This section is provided for those that are interested in the details. Please feel free to skip it...)

As we saw above, a sample is represented by a big long list of numbers representing where the speaker should be through time. We can take this list of numbers and use it to draw a graph which would look similar to this:

![sample graph](:/images/tutorial/sample.png)

You might have seen pictures like this before. It's called the *waveform* of a sample. It's just a graph of numbers. Typically a waveform like this will have 44100 points of data per second (this is due to the Nyquist–Shannon sampling theorem). So, if the sample lasts for 2 seconds, the waveform will be represented by 88200 numbers which we would feed to the speaker at a rate of 44100 points per second. Of course, we could feed it at double rate which would be 88200 points per second. This would therefore take only 1 second to play back. We could also play it back at half rate which would be 22050 points per second taking 4 seconds to play back.

The duration of the sample is affected by the play back rate: 

* Doubling the playback rate halves the play back time,
* Halving the playback rate doubles the play back time,
* Using a playback rate of one fourth quadruples the playback time,
* Using a playback rate of 1/10 makes playback last 10 times longer.

We can represent this with the formula:

```
new_sample_duration = (1 / rate) * sample_duration 
```

Changing the playback rate also affects the pitch of the sample. The frequency or pitch of a wave form is determined by how fast it moves up and down. Our brains somehow turn fast movement of speakers into high notes and slow movement of speakers into low notes. This is why you can sometimes even see a big bass speaker move as it pumps out super low bass - it's actually moving a lot slower in and out than a speaker producing higher notes.

If you take a waveform and squash it it will move up and down more times per second. This will make it sound higher pitched(. It turns out that doubling the amount of up and down movements (oscillations) doubles the frequency. So, *playing your sample at double rate will double the frequency you hear it*. Also, *halving the rate will half the frequency*. Other rates will affect the frequency accordingly.
