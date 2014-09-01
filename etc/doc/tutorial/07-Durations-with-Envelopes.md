# Duration with Envelopes

As we introduced in an earlier section, we can use the `sleep` command to control when to trigger our sounds. However, we haven't yet been able to control the duration of our sounds (except in the case of samples where we can stretch and compress them).

In order to give us a simple, yet powerful, means of *controlling the duration* of our sounds, Sonic Pi provides the notion of an *ADSR amplitude envelope* (we'll cover what ADSR means later in this section). An amplitude envelope offers two useful aspects of control:

* control over the duration of a sound
* control over the amplitude of a sound

## Duration

The duration is the length the sound lasts for. A longer duration means that you hear the sound for longer. Sonic Pi's sounds all have a controllable amplitude envelope and the total duration of that envelope is the duration of the sound. Therefore, by controlling the envelope you control the duration.
 
## Amplitude

The ADSR envelope not only controls duration it also gives you *fine control over the amplitude of the sound*. All sounds start and end silent with some non-silent part in-between. Envelopes allow you to slide and hold the amplitude of non-silent parts of the sound. It's like giving someone instructions on how to turn up and down the volume for some music. For example you might ask someone to "start at silence, slowly move up to full volume, hold it for a bit, then quickly back to silence." Sonic Pi allows you to program this with envelopes.

As we have seen in a previous section, an amplitude of 0 is silence and an amplitude of 1 is normal volume.

## Release Time

By default all synths have a release time of 1. This means that they have a duration of 1 second before completing. We can change this duration by passing the `release:` argument to our calls to `play`. For example, to make a synth play for 2 seconds we specify a `release:` of `2`:

```
play 60, release: 2
```

We can make the synth sound for a very short amount of time by using a very small release time:

```
play 60, release: 0.2
```

So what is release time? It's the time it takes for the sound to go from full amplitude (typically a value of 1) to zero amplitude. This is called the *release phase* and it's a linear transition (i.e. a straight line). The following diagram illustrates this transition:

![release envelope](:/images/tutorial/env-release.png)

The vertical line at the far left of the diagram shows that the sound starts at 0 amplitude, but goes up to full amplitude immediately (this is the attack phase which we'll cover next). Once at full amplitude it then moves in a straight line down to zero taking the amount of time specified by `release:`.  *Longer release times produce longer synth fade outs.*

You can therefore change the duration of your sound by changing the release time. Have a play adding release times to your music.

## Attack Time

By default, the attack phase is 0 for all synths which means they move from 0 amplitude to 1 immediately. This gives the synth an initial percussive sound. However, you may wish to fade your sound in. This can be achieved with the `attack:` argument. Try fading in some sounds:

```
play 60, attack: 2
sleep 3
play 65, attack: 0.5
```

You may use multiple arguments at the same time. For example for a short attack and a long release try:

```
play 60, attack: 0.7, release: 4
```

This short attack and long release envelope is illustrated in the following diagram:

![attack release envelope](:/images/tutorial/env-attack-release.png)

Of course, you may switch things around. Try a long attack and a short release:

```
play 60, attack: 4, release: 0.7
```

![long attack short release envelope](:/images/tutorial/env-long-attack-short-release.png)

Finally, you can also have both short attack and release times for shorter sounds.

```
play 60, attack: 0.5, release: 0.5
```

![short attack short release envelope](:/images/tutorial/env-short-attack-short-release.png)

## Sustain Time

In addition to specifying attack and release times, you may also specify a sustain time. This is the time which to maintain the sound at full amplitude and takes place between the attack and release stages. 

```
play 60, attack: 0.3, sustain: 1, release: 1
```

![ASR envelope](:/images/tutorial/env-attack-sustain-release.png)

The sustain time is useful for important sounds you which to hold full presence in the mix before entering an optional release phase. Of course, it's totally valid to set both the attack and release arguments to 0 and just use the sustain to have absolutely no fade in or fade out to the sound. However, be warned, a release of 0 can produce clicks in the audio and it's often better to use a very small value such as 0.2.


## Decay Time

Finally, for the moments where you need an extra level of control, you can also specify a decay time. This is a phase of the envelope the fits between the attack and sustain phases and specifies a time where the amplitude will drop from the `attack_level` to the `sustain_level`. By default, the decay argument is 0 and both the attack and sustain levels are 1 so you'll need to specify them for the decay time to have any effect:

```
play 60, attack: 0.1, attack_level: 1, decay: 0.2, sustain_level: 0.4, sustain: 1, release: 0.5
```

![ADSR envelope](:/images/tutorial/env-attack-decay-sustain-release.png)

## ADSR Envelopes

So to summarise, Sonic Pi's ADSR envelopes have the following phases:

1. *attack* - time from 0 amplitude to the `attack_level`,
2. *decay* - take to move amplitude from `attack_level` to `sustain_level`,
3. *sustain* - time to keep the amplitude at `sustain_level`,
4. *release* - time to move amplitude from `sustain_level` to 0

It's important to note that the duration of a sound is the summation the times of each of these phases. Therefore the following sound will have a duration of 0.5 + 1 + 2 + 0.5 = 4 seconds:

```
play 60, attack: 0.5, decay: 1, sustain_level: 0.4, sustain: 2, release: 0.5
```

Now go and have a play adding envelopes to your sounds

