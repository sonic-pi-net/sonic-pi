# Thread Synchronisation

Once you have become sufficiently advanced live coding with a number of functions and threads simultaneously, you've probably noticed that it's pretty easy to make a mistake in one of the threads which kills it. That's no big deal, because you can easily restart the thread by hitting run. However, when you restart the thread it is now *out of time* with the original threads.

## Inherited Time

As we discussed earlier, new threads created with `in_thread` inherit all of the settings from the parent thread. This includes the current time. This means that threads are always in time with each other when started simultaneously.

However, when you start a thread on its own it starts with its own time which is unlikely to be in synchronisation with any of the other currently running threads.

## Cue and Sync

Sonic Pi provides a solution to this problem with the functions `cue` and `sync`.

`cue` allows us to send heartbeat messages out to all other threads. By default the other threads aren't interested and ignore these heartbeat messages. However, you can easily register interest with the `sync` function.

The important thing to be aware of is that `sync` is similar to `sleep` in that it stops the current thread from doing anything for a period time. However, with `sleep` you specify how long you want to wait for and with `sync` you don't know how long you will wait for - as you'll wait for the next `cue` from another thread which may be soon or a long time away. 

Let's explore this in a little more detail.

```
in_thread do
  loop do
    cue :tick
    sleep 1
  end
end  

in_thread do
  loop do
    sync :tick
    sample :drum_heavy_kick
  end
end
```

Here we have two threads - one acting like a metronome not playing any sounds but sending out `:tick` heartbeat messages every second. The second thread is synchronising on `tick` messages and when it receives one it inherits the time of the `cue` thread and continues running. 

We can see that the synchronisation is really happening by inserting a naughty `sleep` between threads. Typically this would make the second thread out of phase with the first. However, as we're using `cue` and `sync` we automatically sync the threads bypassing any accidental timing offsets:

```
in_thread do
  loop do
    cue :tick
    sleep 1
  end
end  

sleep 0.3

in_thread do
  loop do
    sync :tick
    sample :drum_heavy_kick
  end
end
```

## Cue Names  

You are free to use whatever name you'd like for your `cue` messages - not just `:tick`. You just need to ensure that any other threads are `sync`ing on the correct name - otherwise they'll be waiting for ever (or at least until you press the stop button).

Let's play with a few `cue` names:

```
in_thread do
  loop do 
    cue [:foo, :bar, :baz].choose
    sleep 0.5
  end
end

in_thread do
  loop do 
    sync :foo 
    sample :elec_beep
  end
end

in_thread do
  loop do            
    sync :bar        
    sample :elec_flip
  end
end

in_thread do
  loop do            
    sync :baz        
    sample :elec_blup
  end
end
```

Here we have a main `cue` loop which is randomly sending one of the heartbeat names `:foo`, `:bar`, or `:baz`. We then also have three loop threads syncing on each of those names independently and then playing a different sample. The net effect is that we hear a sound every 0.6 seconds as each of the `sync` threads is randomly synced with the `cue` thread and plays its sample.

This of course also works if you order the threads in reverse as the `sync` threads will simply sit and wait for the next `cue`.

## Live Coding with Cue and Sync

Let's take a quick look at how you might live code with `cue` and `sync`. Here's one of the examples modified to use a `cue` metronome:

```
define :metro do
  cue :tick
  sleep 0.5
end

define :drums do
  sync :tick
  sample :drum_heavy_kick, rate: 0.75
  sleep 0.5
  sample :drum_heavy_kick
end

define :synths do
  sync :tick
  use_synth :mod_pulse
  use_synth_defaults amp: 1, mod_range: 15, cutoff: 80, pulse_width: 0.2, attack: 0.03, release: 0.6,   mod_phase: 0.25
  play 30
  sleep 0.25
  play 38
end

in_thread(name: :drums){loop{drums}}
in_thread(name: :synths){loop{synths}}
in_thread(name: :metro){loop{metro}}
```

If you play this, you'll hear everything in sync. Now, whilst it's still playing go ahead and break the code. Change `:drum_heavy_kick` to `drum_sdlfkjsd`. Boom! The drums stop, and an exception is thrown telling you that something has gone wrong. Now, whilst it is still playing undo your change and switch the code back to `:drum_heavy_kick` and press run again. Wooh! The drums have returned and are completely back in sync. The jamming can continue...

