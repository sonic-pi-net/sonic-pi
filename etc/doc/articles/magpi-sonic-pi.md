## Live Coding

The laser beams sliced through the wafts of smoke as the subwoofer
pumped bass deep into the bodies of the crowd. The atmosphere was ripe
with a heady mix of synths and dancing. However something wasn't quite
right in this nightclub. Projected in bright colours above the DJ booth
was futuristic text, moving, dancing flashing. This wasn't fancy
visuals, it was merely a projection of a terminal containing Emacs. The
occupants of the DJ booth weren't spinning disks, they were writing,
editing and evaluating code. This was a Meta-eX (http://meta-ex.com)
gig. The code was their musical interface and they were playing it live.

[Image of Meta-eX]

This wasn't a scene from a cheesy sci-fi film. Coding music like this is
a growing trend and is often described as Live Coding
(http://toplap.org). One of the recent directions this approach to music
making has taken is the Algorave (http://algorave.com) - events where
artists code music for people to dance to. However, you don't need to be
in a nightclub to Live Code - with Sonic Pi version 2.0 you can do it
anywhere you can take your Raspberry Pi and a pair of headphones or some
speakers.  Once you reach the end of this articla, you'll be programming
your own beats and modifying them live. Where you go afterwards will
only be constrained by your imagination.

## Getting Sonic Pi v2.0

Firstly, you'll need the latest version of Sonic Pi: version 2+. 

At the time of writing, this hasn't been released yet. However, by the
time you read this, it should be available as part of the latest
Raspbian OS. If you have an older image on your SD cards, running the
following should get you up to date: 

    sudo apt-get update
    sudo apt-get upgrade

You should be able to open Sonic Pi by clicking on the main menu,
and looking within Education -> Sonic Pi

## What's New?

Some of you may have already had a play around with Sonic Pi. Hopefully
you all had fun making beeps of different pitches. You can take all the
music coding skills you've learned with version 1 and apply it to
version 2. For those of you that have yet to open Sonic Pi - now is
definitely the time to give it a try. You'll be amazed with what you can
do with it now. Here's a quick list of the major new features:

* Ships with over 20 synth sounds
* Ability to play any wav or aiff sample file
* Ships with over 70 samples
* Extremely accurate timing
* Support for over 10 studio effects: reverb, echo, distortion, etc.
* Support for Live Coding: changing the code whilst it runs.

Let's have a look at all of these...

## Playing a drum loop

Let's code up a simple drum loop. We can use the amen break to get us
started. In the main code editor window of Sonic Pi, type the following
and then hit the Run button:

    sample :loop_amen
    
Boom! Instant drums! Go on, press it a few times. Have fun. I'll still
be here when you've finished...

But that's not all. We can mess around with the sample. Try this:

    sample :loop_amen, rate: 0.5
    
Oooh, half speed. Go on, try changing the rate. Try lower and higher
numbers. What happens if you use a negative number?

What if we wanted to play the loop a few times over? One way to do this
is to call sample a few times with some sleeps between:

    sample :loop_amen
    sleep 1.753
    sample :loop_amen
    sleep 1.753
    sample :loop_amen

However, this could get a bit annoying if you wanted to repeat it 10
times. So we have a nice way of saying that with code:

    10.times do
      sample :loop_amen
      sleep 1.753
    end
    
Of course, we can change the 10 to whatever number we want. Go on, try
it! What if we want to loop forever? We simply say loop instead of
10.times. Also, I'm sure you're asking what the magic 1.753 represents
and how I got it. Well, it's the length of the sample in seconds and I
got it because I asked Sonic Pi:

    puts sample_duration :loop_amen 

And it told me 1.753310657596372 - I just shortended it to 1.753 to make
it easier for you to type in. Now, the cool thing is, we can combine
this code and add a variable for fun:

    sample_to_loop = :loop_amen
    sample_rate = 0.5
    
    loop do
      sample sample_to_loop, rate: sample_rate
      sleep sample_duration sample_to_loop, rate: sample_rate
    end
    
Now, you can change the :loop_amen to any of the other loop samples (use
the auto-complete to discover them). Change the rate too. Have fun!

For a complete list of available samples click the help button.

## Adding Effects

One of the most exciting features in version 2.0 of Sonic Pi is the
support for studio effects such as reverb and echo and distortion. These
are really easy to use. For example take the following sample trigger code:

    sample :guit_e_fifths
    
To add some reverb to this, we simply need to wrap it with a with_fx block:

    with_fx :reverb do
      sample :guit_e_fifths
    end
    
To add some distortion too, we can add more fx:

    with_fx :reverb do
      with_fx :disortion do
        sample :guit_e_fifths
      end
    end
    
Just like synths and samples, FX also support parameters to allow you to
tinker with their settings:

    with_fx :reverb, mix: 0.8 do
      with_fx :distortion, distort: 0.8 do
        sample :guit_e_fifths
      end
    end 

Of course, you can wrap FX blocks around any code. For example here's
how you'd combine the :ixi_techno FX and our drum loop:

    with_fx :ixi_techno do
      loop do
        sample :loop_amen
        sleep sample_duration :loop_amen
      end
    end

For a complete list of FX and their parameters click the help button.

## Live Coding a Synth Loop

Now we've mastered the basics of triggering samples, sleeping and
looping, let's do the same with some synths and then jump head first
into live coding territory:
    
    loop do
      use_synth :tb303
      play 30, attack: 0, release: 0.2
      sleep 0.5
    end
    
So, what do the numbers mean in this example? Well, you could stop it
playing, change a number, then start it and see if you can hear the
difference. However all that stopping and starting gets quite
annoying. Let's make it possible to live code so you can instantly hear
changes. We need to put our code into a named function which we loop:

    define :play_my_synth do
      use_synth :tb303
      play 42, attack: 0, release: 0.2
      sleep 0.5
    end
    
    loop do 
      play_my_synth
    end
    
Now when you run this it will sound exactly the same as the simpler loop
above. However, now we have given our code a name (in this case,
play_my_synth) we can change the definition of our code whilst things
run. Follow these steps:

1. Write the code above (both the define and loop blocks)
2. Press the run button
3. Comment out the loop block (by adding # at the beginning of each line)
4. Change the definition of your function
5. Press the run button again
6. Keep changing the definition and pressing run!
7. Press stop

For example. Start with the code above. Hit run. Comment out the loop
block then change the note to play to something different. Your code
should look like this:

    define :play_my_synth do
      use_synth :tb303
      play 45, attack: 0, release: 0.2, cutoff: 70
      sleep 0.5
    end
    
    # loop do
    #   play_my_synth
    # end
    
Then hit the run button again. You should hear the note go higher. Try
changing the attack and release and cutoff parameters. Listen to the
effect they have. Notice that attack and release change the length of
the note and that cutoff changes the 'brightness' of the sound. Try
changing the synth too - fun values are :prophet, :dsaw,
:supersaw. Press the help button for a full list.

There's loads of other things we can do now, but unfortunately I'm
running out of space in this article so I'll just throw a couple of
ideas at you. Firstly, we can try some randomisation. A really fun
function is rrand. It will return a random value between two values. We
can use this to make the cuffoff bounce around for a really cool
effect. Instead of passing a number like 70 to the cutff value, try
rrand(40, 120). Also, instead of only playing note 45, let's choose a
value from a list of numbers. Try changing 45 to chord(:a3,
:minor).choose. Your play line should look like this:

    play chord(:a2, :minor).choose, attack: 0, release: 0.3, cutoff: rrand(30, 100)
    
Now you can start experimenting with different chords and range values
for cutoff. You can do something similar with the pan value too:

    play chord(:a2, :minor).choose, attack: 0, release: 0.3, cutoff: rrand(30, 100), pan: rrand(-1, 1)

Now throw some FX in, mess around and just have fun!

