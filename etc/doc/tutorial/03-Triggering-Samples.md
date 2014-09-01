# Triggering Samples

Playing beeps is only the beginning. Something that's a lot of fun is triggering pre-recorded samples. Try it:

```
sample :ambi_lunar_land
```

Sonic Pi includes many samples for you to play with. You can use them just like you use the `play` command. To play multiple samples and notes just write them one after another:

```
play 36
play 48
sample :ambi_lunar_land
sample :ambi_drone
```

If you want to space them out in time, use the `sleep` command:

```
sample :ambi_lunar_land
sleep 1
play 48
sleep 0.5
play 36
sample :ambi_drone
sleep 1
play 36
```

Notice how Sonic Pi doesn't wait for a sound to finish before starting the next sound. The `sleep` command only describes the separation of the *triggering* of the sounds. This allows you to easily layer sounds together creating interesting overlap effects. Later in this tutorial we'll take a look at controlling the *duration* of sounds with envelopes.


## Discovering Samples

There are two ways to discover the range of samples provided in Sonic Pi. First, you can use this help system. Click on Samples in the far left vertical menu, choose your category and then you'll see a list of available sounds.

Alternatively you can use the *auto-completion system*. Simply type the start of a sample group such as: `sample :ambi_` and you'll see a drop-down of sample names appear for you to select. Try the following category prefixes: 

* `:ambi_` 
* `:bass_`
* `:elec_`
* `:perc_`
* `:guit_`
* `:drum_`
* `:misc_`

Now start mixing samples into your compositions!
