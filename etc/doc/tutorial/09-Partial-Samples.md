# Partial Samples

This section will conclude our exploration of Sonic Pi's sample player. Let's do a quick recap. So far we've looked how we can trigger samples:

```
sample :loop_amen
```

We then looked at how we can change the rate of samples such as playing it at half speed:

```
sample :loop_amen, rate: 0.5
```

Next, we looked at how we could fade a sample in (let's do it half speed):

```
sample :loop_amen, rate: 0.5, attack: 1
```

We also looked at how we could just use the start of a sample percussively by giving `sustain:` an explicit value and setting both the attack and release to be short values:


```
sample :loop_amen, rate: 2, attack: 0.01, sustain: 0, release: 0.35
```

However, wouldn't it be nice if we didn't have to always start at the beginning of the sample? Wouldn't it also be nice if we didn't have to always finish at the end of the sample?

## Choosing a starting point

It is possible to choose an arbitrary starting point in the sample as a value between 0 and 1 where 0 is the start of the sample, 1 is the end and 0.5 is half way through the sample. Let's try playing only the last half of the amen break:

```
sample :loop_amen, start: 0.5
```

How about the last quarter of the sample:

```
sample :loop_amen, start: 0.75
```

## Choosing a finish point

Similarly, it is possible to choose an arbitrary finish point in the sample as a value between 0 and 1. Let's finish the amen break half way through:

```
sample :loop_amen, finish: 0.5
```

## Specifying start and finish

Of course, we can combine these two to play arbitrary segments of the audio file. How about only a small section in the middle:

```
sample :loop_amen, start: 0.4, finish: 0.6
```

What happens if we choose a start position after the finish position?


```
sample :loop_amen, start: 0.6, finish: 0.4
```

Cool! It plays it backwards!

## Combining with rate

We can combine this new ability to play arbitrary segments of audio with our friend `rate:`. For example, we can play a very small section of the middle of the amen break very slowly:

```
sample :loop_amen, start: 0.5, finish: 0.7, rate: 0.2
```

## Combining with envelopes

Finally, we can combine all of this with our ADSR envelopes to produce interesting results:

```
sample :loop_amen, start: 0.5, finish: 0.8, rate: -0.2, attack: 0.3, release: 1
```

Now go and have a play with mashing up samples with all of this fun stuff...







