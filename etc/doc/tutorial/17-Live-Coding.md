# Live Coding

, now we've learned enough to really start having some fun. In this section we'll draw from all the previous sections and show you how you can start making your music compositions live and turning them into a performance. For that we'll need 3 main ingredients:

* An ability to write code that makes sounds - CHECK!
* An ability to write functions - CHECK!
* An ability to use (named) threads - CHECK!

Alrighty, let's get started. Let's live code our first sounds. We first need a function containing the code we want to play. Let's start simple. We also want to loop calls to that function in a thread:

```
define :my_loop do
  play 50
  sleep 1
end

in_thread(name: :looper) do
  loop do
    my_loop
  end
end
```

If that looks a little too complicated to you, go ahead and re-read the sections on functions and threads. It's not too complicated if you've already wrapped your head around these things. 

What we have here is a function definition which just plays note 50 and sleeps for a second. We then define a named thread called `:looper` which just loops round calling `my_loop` repeatedly. 

If you run this code, you'll hear note 50 repeating again and again...

## Changing it up

Now, this is where the fun starts. Whilst the code is *still running* change 50 to another number, say 55, then press the run button again. Woah! It changed! Live!

It didn't add a new layer because we're using named threads which only allow one thread for each name. Also, the sound changed because we *redefined* the function. We gave `:my_loop` a new definition. When the `:looper` thread looped round again it simply called the new definition.

Try changing it again, change the note, change the sleep time. How about adding a `use_synth` statement. For example, change it to:

```
define :my_loop do
  use_synth :tb303
  play 50, release: 0.3
  sleep 0.25
end
```

Now it's sounding pretty interesting, but we can spice it up further. Instead of playing the same note again and again, try playing a chords

```
define :my_loop do
  use_synth :tb303
  play choose(chord(:e3, :minor)), release: 0.3
  sleep 0.5
end
```

How about playing random notes from the chords:

```
define :my_loop do
  use_synth :tb303
  play choose(chord(:e3, :minor)), release: 0.3
  sleep 0.25
end
```

How about about using a random cutoff value:

```
define :my_loop do
  use_synth :tb303
  play choose(chord(:e3, :minor)), release: 0.2, cutoff: rrand(60, 130)
  sleep 0.25
end
```

Finally, add some drums:

```
define :my_loop do
  use_synth :tb303
  sample :drum_bass_hard, rate: rrand(0.5, 2)
  play choose(chord(:e3, :minor)), release: 0.2, cutoff: rrand(60, 130)
  sleep 0.25
end
```

Now things are getting exciting! Start creating your own functions, looping them in threads and perform some music for your friends...
