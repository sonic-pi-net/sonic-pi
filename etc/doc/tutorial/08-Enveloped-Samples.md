# Enveloped Samples

It is also possible to modify the *duration* and *amplitude* of a sample using an ADSR envelope. However, this works slightly differently to the ADSR envelope available on synths. Sample envelopes only allow you to reduce the amplitude and duration of a sample - and never to increase it. The sample will stop when either the sample has finished playing or the envelope has completed - whichever is first. So, if you use a very long `release:`, it won't extend the duration of the sample.

## Amen Envelopes

Let's return to our trusty friend the Amen Break:

```
sample :loop_amen
```

With no parameters, we hear the full sample at full amplitude. If we want to fade this in over 1 second we can use the `attack:` param:

```
sample :loop_amen, attack: 1
```

For a shorter fade in, choose a shorter attack value:

```
sample :loop_amen, attack: 0.3
```

## Auto Sustain

Where the ADSR envelope's behaviour differs from the standard synth envelope is in the *sustain* value. In the standard synth envelope, the sustain defaulted to 0 unless you set it manually. With samples, the sustain value defaults to an *automagical* value - the time left to play the rest of the sample. This is why we hear the full sample when we pass no defaults. If the attack, decay,  sustain and release values were all 0 we'd never hear a peep. Sonic Pi therefore calculates how long the sample is, deducts any attack, decay and release times and uses the result as your sustain time. If the attack, decay and release values add up to more than the duration of the sample, the sustain is simply set to 0.

## Fade Outs

To explore this, let's consider our Amen break in more detail. If we ask Sonic Pi how long the sample is:

```
print sample_duration :loop_amen
```

It will print out `1.753310657596372` which is the length of the sample in seconds. Let's just round that to `1.75` for convenience. Now, if we set the release to `0.75` something surprising will happen:

```
sample :loop_amen, release: 0.75
```

It will play the first second of the sample at full amplitude before then fading out over a period of 0.75 seconds. This is the *auto sustain* in action. By default, the release always works from the end of the sample. If our sample was 10.75 seconds long, it would play the first 10 seconds at full amplitude before fading out over 0.75s.

Remember by default, `release:` fades out at the end of a sample.

## Fade In and Out

We can use both `attack:` and `release:` together with the auto sustain behaviour to fade both in and out over the duration of the sample:

```
sample :loop_amen, attack: 0.75, release: 0.75
```

As the full duration of the sample is 1.75s and our attack and release phases add up to 1.5s, the sustain is automatically set to 0.25s. This allows us to easily fade the sample in and out.

## Explicit sustain

We can easily get back to our normal synth ADSR behaviour by manually setting `sustain:` to a value such as 0:

```
sample :loop_amen, sustain: 0, release: 0.75
```

Now, our sample only plays for 0.75 seconds in total. With the default for `attack:` and `decay:` at 0, the sample jumps straight to full amplitude, sustains there for 0s then releases back down to 0 amplitude over the release period - 0.75s.

## Percussive cymbals

We can use this behaviour to good effect to turn longer sounding samples into shorter more percussive versions. Consider the sample `:drum_cymbal_open`:

```
sample :drum_cymbal_open
```

You can hear the cymbal sound ringing out over a period of time. However, we can use our envelope to make it more percussive:

```
sample :drum_cymbal_open, attack: 0.01, sustain: 0, release: 0.1
```

You can then emulate hitting the cymbal and then dampening it by increasing the sustain period:

```
sample :drum_cymbal_open, attack: 0.01, sustain: 0.3, release: 0.1
```

Now go and have fun putting envelopes over the samples. Try changing the rate too for really interesting results.







