# Synth Design

The awesome sounds you hear in Sonic Pi are all produced by the SuperCollider
synthesis engine. These synths and effects have to be defined beforehand in a special
kind of file called a `synthdef`.
All the `synthdef`s that ship with Sonic Pi are designed in a Clojure based library called
[Overtone](https://overtone.github.io). You can find the existing synth designs in the folder

```
etc/synthdefs/designs/sonic_pi/synths/
```

## Editing the synthdefs

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

### Starting a REPL

`cd` into the Overtone folder and run

```
$ lein repl
```

Make a note of the port number that nREPL starts on

```
nREPL server started on port 49223 ...
```

### Connecting the editor to the REPL and booting Overtone

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

## The synth design file

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

This is the definition used to define the record functionality in Sonic Pi.

The `without-namespace-in-synthdef` from `sonic-pi.synths.core` needs to wrap the `defsynth` for
the synth to follow the correct naming convention.

The `core/save-synthdef` manages the workflow for saving the compiled synthdef file into the correct folder
and also the graphviz design which is used for documentation.

By evaluating the whole form this should cause all the files to be saved to the correct places.

## Making the synth available in Sonic Pi

If you design your own synths you'll need to declare them in Ruby to make them show up in Sonic Pi itself.

At the moment this is done in the `app/server/sonicpi/lib/sonicpi/synths/synthinfo.rb`.

You'll also need to restart Sonic Pi to pickup any new synths.

Good luck!
