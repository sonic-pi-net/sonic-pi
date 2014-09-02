# Controlling Running Synths

So far we've only concerned ourselves with triggering new sounds and FX. However, Sonic Pi gives us the ability to manipulate and control currently running sounds. We do this by using a variable to capture a reference to a synth:

```
s = play 60, release: 5
```

Here, we now have a run-local variable, `s` which represents the synth playing note 60. Note that this is run-local - you can't access it from other runs like you can with functions.

Once we have `s` we can start controlling it via the `control` function:

```
s = play 60, release: 5
sleep 0.5
control s, note: 65
sleep 0.5
control s, note: 67
sleep 3
control s, note: 72
```

The thing to notice is that we're not triggering 4 different synths here - we're just triggering one synth and then changing the pitch 3 times afterwards.

We can pass any of the standard parameters to `control` so you can control things like `amp:`, `cutoff:`, and `pan`. 

## Non-controllable Parameters

Some of the parameters can't be controlled once the synth has started. This is the case for all the ADSR envelope parameters. You can find out which parameters are controllable by looking at their documentation in the help system. If the documentation says *Can not be changed once set* you know it's not possible to control the parameters after the synth has started. 

## Controlling FX

It is also possible to control FX, although this is achieved in a slightly different way:

```
with_fx :reverb do |r|
  play 50
  sleep 0.5
  control r, mix: 0.7
  play 55
  sleep 1
  control r, mix: 0.9
  sleep 1
  play 62
end
```

Instead of using a variable, we use the goalpost parameters of the do/end block. Inside the `|` bars, we need to specify a unique name for our running FX which we are free to use within the confines of the containing do/end block. This behaviour is identical to using parameterised functions.

Now go and control some synths and FX!



