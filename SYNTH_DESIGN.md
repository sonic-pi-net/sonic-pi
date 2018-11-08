# Synth Design

The sounds you hear in Sonic Pi are all produced by the SuperCollider
synthesis engine. These synths and effects have to be defined beforehand
in a special kind of binary file called a `synthdef`.  The built in
synthdefs are loaded up on boot so they are ready to trigger.

Synthdefs are ultimately compiled from source code into binary files,
but there are two main ways you can do this. You can either use the
Clojure based library called [Overtone](https://overtone.github.io),
or directly use SuperCollider's own language instead).

[Using **Overtone** to design and compile your synths](#overtone)<br/>
[Using **SuperCollider** to design and compile your synths](#supercollider)

All of the synthdefs that currently ship with Sonic Pi were designed in
Overtone. You can find the existing synth designs in the folder

```
etc/synthdefs/designs/sonic_pi/synths/
```

**_Please note however_**, that for new synth designs _that are intended to
be distributed with Sonic Pi_, source code written with Overtone is now
considered deprecated and only designs written in SuperCollider language
will be accepted. As such, the instructions for using Overtone
still work, but if you are considering submitting a new synth design for
distribution with Sonic Pi, please [create your synth with SuperCollider
directly](#supercollider).


## Synth design constraints

If you want your synth to work with Sonic Pi's automatic stereo
sound infrastructure *you need to ensure your synth outputs a stereo
signal* to an audio bus with an index specified by a synth arg named
`out_bus`.

Additionally, your synth must self-terminate at some point - Sonic Pi
will *not* tidy up zombied synths.


<a name="overtone"/>

## Using **Overtone** to design and compile your synths

### Editing the synthdefs

You'll need the following to be able to compile the synths yourself

* the latest version of Overtone (`git clone git@github.com:overtone/overtone.git`)
* leiningen (`brew install leiningen`)
* A text editor setup to work with Clojure

After cloning the Overtone repo, cd into the folder and edit the `project.clj` file to add
the Sonic Pi synthdefs folder to the locations it looks in when requiring Clojure files.

```
  :native-path "native"
  :min-lein-version "2.0.0"
  ;; this is the line to add
  :source-paths  ["src"  "/Users/foo/sonic-pi/etc/synthdefs/designs"]
  ;; make sure the path points to your installation of Sonic Pi
```

#### Starting a REPL

`cd` into the Overtone folder and run

```
$ lein repl
```

Make a note of the port number that nREPL starts on

```
nREPL server started on port 49223 ...
```

#### Connecting the editor to the REPL and booting Overtone

If you're using `vim` and `vim-fireplace`:

  * open a new terminal window
  * `cd` back into the Sonic Pi folder
  * Open `etc/synthdefs/designs/sonic_pi/synths/core.clj`
  * type `:Connect` then hit enter
  * when prompted, enter the nREPL port number from earlier
  * Visual select the namespace definition and type `:` followed by `Eval` then enter

This should start booting Overtone which will take a little while. If there are no errors,
your vim is successfully hooked up to Overtone!

If you're using `emacs`

  * open a new terminal window
  * open `emacs` and go to the `project.clj` for Overtone
  * `M-x cider-jack-in` to connect to the running nREPL
  * Navigate back into the Sonic Pi folder and open `etc/synthdefs/designs/sonic_pi/synths/core.clj`
  * Evaluate the namespace as you normally would for Clojure in emacs

### The synth design file

Taking the example of a basic synth lets have a look at what the bits are doing

```
(ns sonic-pi.synths.studio
  (:use [overtone.live])
  (:require [sonic-pi.synths.core :as core]))

(do
  (without-namespace-in-synthdef
    (defsynth sonic-pi-recorder
         [out-buf 0 in_bus 0]
         (disk-out out-buf (in in_bus 2))))


  (uncomment
    (core/save-synthdef sonic-pi-recorder)))
```

This is the definition used to define the record functionality in Sonic
Pi.

The `without-namespace-in-synthdef` from `sonic-pi.synths.core` needs to
wrap the `defsynth` for the synth to follow the correct naming
convention.

The `core/save-synthdef` manages the workflow for saving the compiled
synthdef file into the correct folder and also the graphviz design which
is used for documentation.

By evaluating the whole form this should cause all the files to be saved
to the correct places.

<a name="supercollider"/>

## Using **SuperCollider** to design and compile your synths

Simply define your synth with SuperCollider's built in language, and use
the writeDefFile command to store the compiled synthdef into a directory
of your choice. You can then dynamically load your synthdefs with the `load_synthdefs` fn.


    (
    SynthDef(\\piTest,
             {|freq = 200, amp = 1, out_bus = 0 |
               Out.ar(out_bus,
                      SinOsc.ar([freq,freq],0,0.5)* Line.kr(1, 0, 5, amp, doneAction: 2))}
    ).writeDefFile(\"/Users/sam/Desktop/my-synths\") ;
    )


## Making the synth available in Sonic Pi

To make the synths available simply call `load_synthdefs` with the path
to the folder containing the synthdefs you've compiled and they'll be
immediately available to Sonic Pi via the `synth` fn.

However, if you want the synths to be visible to the GUI you'll need to
add the appropriate metadata to
`app/server/sonicpi/lib/sonicpi/synths/synthinfo.rb` and re-compile the
app.

Have fun!
