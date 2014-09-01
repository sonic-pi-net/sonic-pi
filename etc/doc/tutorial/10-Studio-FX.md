# Studio FX

One of the most rewarding and fun aspects of Sonic Pi is the ability to easily add studio effects to your sounds. However, before we can use them we need to learn an important piece of code syntax - do/end blocks/

## do/end Blocks

With parameters we were able to change something that happened on a single line. However, sometimes we want to do something meaningful to a number of lines of code. For example we may wish to add reverb to some of our calls to `play` or `sample`. Consider the following code:

```
play 50
sleep 0.5
sample :elec_plip
sleep 0.5
play 62
```

We therefore need to tell Sonic Pi where we wish to *start* using reverb and where to *end* using reverb. We use `do` for start and `end` for end. For example:

```
do
  play 50
  sleep 0.5
  sample :elec_plip
  sleep 0.5
  play 62
end
```

However, this isn't yet complete and won't work (try it and you'll get an error) as we haven't told Sonic Pi what we want to do with this *do/end block*. We tell Sonic Pi this by writing some special code before the `do`. For example, let's look at adding some reverb.

## Reverb

If we want to use reverb we write `with_fx :reverb` like this:

```
with_fx :reverb do
  play 50
  sleep 0.5
  sample :elec_plip
  sleep 0.5
  play 62
end

```

Now play this code and you'll hear it played with reverb. It sounds good doesn't it! Everything sounds pretty nice with reverb. 

Now let's look what happens if we have code outside the do/end block:

```
with_fx :reverb do
  play 50
  sleep 0.5
  sample :elec_plip
  sleep 0.5
  play 62
end

sleep 1
play 55
```

Notice how the final `play 55` isn't played with reverb. This is because it is *outside* the do/end block, so it isn't captured by the reverb FX.

Similarly, if you make sounds before the do/end block, they also won't be captured:


```
play 55
sleep 1

with_fx :reverb do
  play 50
  sleep 0.5
  sample :elec_plip
  sleep 0.5
  play 62
end

sleep 1
play 55
```

## Echo

There are many FX to choose from. How about some echo?

```
with_fx :echo do
  play 50
  sleep 0.5
  sample :elec_plip
  sleep 0.5
  play 62
end
```

One of the powerful aspects of Sonic Pi's FX blocks is that they may be passed parameters similar to parameters we've already seen with `play` and `sample`. For example a fun echo parameter to play with is `phase:` which represents the duration of a given echo in seconds. Let's make the echo slower:

```
with_fx :echo, phase: 0.5 do
  play 50
  sleep 0.5
  sample :elec_plip
  sleep 0.5
  play 62
end
```

Let's also make the echo faster:

```
with_fx :echo, phase: 0.125 do
  play 50
  sleep 0.5
  sample :elec_plip
  sleep 0.5
  play 62
end
```

Finally we can use a number of parameters. Let's make the echo take longer to fade away by setting the `decay:` time to 8 seconds:

```
with_fx :echo, phase: 0.5, decay: 8 do
  play 50
  sleep 0.5
  sample :elec_plip
  sleep 0.5
  play 62
end
```

## Nesting FX

One of the most powerful aspects of the FX blocks is that you can nest them. This allows you to very easily chain FX together. For example, what if you wanted to play some code with echo and then with reverb? Easy, just put one inside the other:

```
with_fx :reverb do
  with_fx :echo, phase: 0.5, decay: 8 do
    play 50
    sleep 0.5
    sample :elec_blup
    sleep 0.5
    play 62
  end
end
```

Think about the audio flowing from the inside of the inner block out. The sound of all the code within the inner do/end block such as `play 50` is first sent to the echo FX and the sound of the echo FX is in turn sent out to the reverb FX.

We may use very deep nestings for crazy results. However, be warned, the FX can use a lot of resources and when you nest them you're effectively running multiple FX simultaneously. So be sparing with your use of FX especially on low powered platforms such as the Raspberry Pi.

## Discovering FX

Sonic Pi ships with a large number of FX for you to play with. To find out which ones are available click on FX in the far right of this help system and you'll see a list of available options. Here's a list of some of my favourite:

* wobble,
* reverb,
* echo,
* distortion,
* slicer

Now go crazy and add FX everywhere for some amazing new sounds!

