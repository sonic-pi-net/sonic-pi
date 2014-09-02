# Threads

So you've made your killer bassline and a phat beat. How do you play them at the same time? One solution is to weave them together manually - play some bass, then a bit of drums, then more bass... However, the timing soon gets hard to think about, especially when you start weaving in more elements. 

What if Sonic Pi could weave things for you automatically? Well it can, and you do it with a special thing called a thread.

## Infinite Loops

 To keep this example simple, you'll have to imagine that this is a phat beat and killer bassline:


```
loop do
  sample :drum_heavy_kick
  sleep 1
end

loop do
  use_synth :fm
  play 40, release: 0.2
  sleep 0.5
end
```

As we've discussed previously, loops are like *black holes* for the program. Once you enter a loop you can never exit them until you hit stop. How do we play both loops at the same time? We have to tell Sonic Pi that we want to start something at the same time as the rest of the code. This is where threads come to the rescue.

## Threads to the Rescue

```
in_thread do
  loop do
    sample :drum_heavy_kick
    sleep 1
  end
end

loop do
  use_synth :fm
  play 40, release: 0.2
  sleep 0.5
end
```

By wrapping the first loop in an `in_thread` do/end block we tell Sonic Pi to run the contents of the do/end block at *exactly* the same time as the next statement after the do end block (which happens to be the second loop). Try it and you'll hear both the drums and the bassline weaved together!

Now what if we wanted to add a synth on top. Something like:

```
in_thread do
  loop do
    sample :drum_heavy_kick
    sleep 1
  end
end

loop do
  use_synth :fm
  play 40, release: 0.2
  sleep 0.5
end

loop do
  use_synth :zawa
  play 52, release: 2.5, phase: 2, amp: 0.5
  sleep 2
end
```

Now we have the same problem as before. The first loop is played at the same time as the second loop due to the `in_thread`. However, *the third loop is never reached*. We therefore need another thread:

```
in_thread do
  loop do
    sample :drum_heavy_kick
    sleep 1
  end
end

in_thread do
  loop do
    use_synth :fm
    play 40, release: 0.2
    sleep 0.5
  end
end

loop do
  use_synth :zawa
  play 52, release: 2.5, phase: 2, amp: 0.5
  sleep 2
end
```

## Runs as threads

What may surprise you is that when you press the run button, you're actually creating a new thread for the code to run. This is why pressing it multiple times will layer sounds over each other. As the runs themselves are threads, they will automatically weave the sounds together for you.

## Scope

As you learn how to master Sonic Pi, you'll learn that threads are the most important building block for your music. One of the important jobs they have is to isolate the notion of *current things* from other threads. What does this mean? Well, when you switch synths using `use_synth` you're actually just switching the synth in the *current thread* - no other thread will have their synth switched. Let's see this in action:

```
play 50
sleep 1

in_thread do
  use_synth :tb303
  play 50
end

sleep 1
play 50

```

Notice how the middle sound was different to the others? The `use_synth` statement only affected the thread it was in and not the outer main run thread.

## Inheritance 

When you create a new thread with `in_thread` the new thread will automatically inherit all of the current settings from the current thread. Let's see that:

```
use_synth :tb303
play 50
sleep 1

in_thread do
  play 55
end
```

Notice how the second note is played with the `:tb303` synth even though it was played from a separate thread? Any of the settings modified with the various `use_*` functions will behave in the same way.

When threads are created, they inherit all the settings from their parent but they don't share any changes back.

## Naming Threads

Finally, we can give our threads names:

```
in_thread(name: :bass) do
  loop do
    use_synth :prophet
    play chord(:e2, :m7).choose, release: 0.6
    sleep 0.5
  end
end

in_thread(name: :drums) do
  loop do
    sample :elec_snare
    sleep 1
  end
end
```

Look at the log pane when you run this code. See how the log reports the name of the thread with the message:

```
[Run 36, Time 4.0, Thread :bass]
 |- synth :prophet, {release: 0.6, note: 47}
```

## Only One Thread per Name Allowed
 
One last thing to know about named threads is that only one thread of that name may be running at the same time. Let's explore this. Consider this code:

```
in_thread do
  loop do
    sample :loop_amen
    sleep sample_duration :loop_amen
  end
end
```

Go ahead and paste that into a workspace and press the run button. Press it again a couple of times. Listen to the cacophony of multiple amen breaks looping out of time with each other. Ok, you can press stop now. 

This is the behaviour we've seen again and again - if you press the run button, sound layers on top of any existing sound. Therefore if you have a loop and press the run button three times, you'll have three layers of loops playing simultaneously.

However, with named threads it is different:

```
in_thread(name: :amen) do
  loop do
    sample :loop_amen
    sleep sample_duration :loop_amen
  end
end
```

Try pressing the run button multiple times with this code. You'll only ever hear one amen break loop. You'll also see this in the log:

```
==> Skipping thread creation: thread with name :amen already exists.
```

Sonic Pi is telling you that a thread with the name `:amen` is already playing, so it's not creating another. 

This behaviour may not seem immediately useful to you now - but it will be very handy when we start to live code...
