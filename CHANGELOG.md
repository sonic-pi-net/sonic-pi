# History
* [v2.9 'Venster'](#v2.9), 31st Dec, 2015
* [v2.8 'Karlsruhe'](#v2.8), 20th Nov, 2015
* [v2.7 'Rerezzed'](#v2.7), 10th Sept, 2015
* [v2.6 'Algorave'](#v2.6), 30th July, 2015
* [v2.5 'Craft'](#v2.5), 13th April, 2015
* [v2.4 'Defrost'](#v2.4), 11th Feb, 2015
* [v2.3 'Bitcrush'](#v2.3), 28th Jan, 2015
* [v2.2 'Slicer'](#v2.2), 18th Dec, 2014
* [v2.1 'Core'](#v2.1), 21st Nov, 2014
* [v2.0 'Phoenix'](#v2.0), 2nd Sept, 2014

<a name="v2.9"></a>

## Version 2.9 - 'Venster'
*Thursday 31st December, 2015*

Hot on the heels of the previous release comes `v2.9` codenamed
`Venster` (Dutch for window). This release has a specific focus on
fixing all the known issues with Sonic Pi running on Windows. If you've
tried Sonic Pi on Windows before and had issues, make sure to try it
again with `v2.9`. If you're still having issues on Windows please do
let us know so we can fix it as soon as possible.

For all you Raspberry Pi and Mac OS X users - you're not left out. Sonic
Pi is just as stable as it was before (if not more stable) and `v2.9`
comes with a surprising amount of tweaks and features for its short 1
month development cycle. There's two new FX to play with: `tanh` and
`gverb` as well as a heavily revamped logging system which is much
clearer about printing which opts are actually being used for your
synths and samples. Finally, we now include of all the published MagPi
magazine articles in the tutorial. We also now have Hungarian and
Norwegian translations of the GUI.

Happy Live Coding!

### Breaking Changes

* Rename fn `invert_chord` to `chord_invert`
* Sampler no longer mixes `rate:` and `pitch_stretch:` opts. It's now
  possible to set the `rate:` independent of the `pitch:` modification
  caused by `pitch_stretch`.

### New Fns

* `block_duration` - returns the duration of the given block in seconds (not scaled with the BPM).
* `block_slept?` - returns true if the contents of the block slept or synced.
* `synth_names` - returns a list of all the available synths
* `reset_mixer!` - resets the main mixer back to its default values.
* `sample_free` - unload a sample from memory to free up memory usage.
* `sample_free_all` - unload all samples from memory.
* `use_octave` - similar to `use_transpose` but for whole octaves.
* `with_octave` - similar to `with_transpose` but for whole octaves.
* `use_merged_sample_defaults` - similar to `use_merged_synth_defaults`
  but for samples
* `with_merged_sample_defaults` - similar to
  `with_merged_synth_defaults` but for samples
* `use_cent_tuning` - uniformly tune all subsequent notes in cents
* `with_cent_tuning` - uniformly tune all notes in cents within the block

### Synths & FX

* New FX `tanh` - for more distortion goodness.
* New FX `gverb` - a different reverb FX to play with.

### GUI

* Synths and samples now also log defaults set by `use_synth_defaults`
  and friends.
* Opts are logged in the order they were defined with local opts first
  followed by inherited opts. 
* BPM scaling is now accounted for in logging vals.
* Log metadata such as run number and time is now printed in a more
  code-like way: as a hash of key value pairs.
* `C-k` will now kill multiple lines if lines end with `,`.
* When saving a buffer a `.txt` extension is automatically added to the
  file name if none specified.
* Add Hungarian and Norwegian translations of GUI.
* Add title to main window - enables Sonic Pi to be selected in
  screensharing app lists such as Google Hangouts and OBS.
* Add autocompletion for tuning systems.

### Documentation

* Add 8 complete MagPi magazine articles to the tutorial in appendix A.
* Add new example 'ambient experiment' by Darin Wilson.
* Add new example 'chord inversions' by Adrian Cheater.
* Change tutorial license to CC-BY-SA 4.0.
* Add instructions for compiling and building on Windows.
* Many, many minor tweaks and improvements.


### Improvements

* Add `sync:` opt to `live_loop` and `in_thread`. This now syncs the
  live loop once on creation. Similar to the `delay:` opt. If both
  `sync:` and `delay:` are specified then the delay happens before the
  sync.
* The `synth` fn can now play chords with the `notes:` opt such as:
  `synth :dsaw, notes: (chord :e3, :minor)`. This will return a single
  controllable synth node representing all the notes similar to
  `play_chord`.
* BPM scaling and other normalisation is now honoured when controlling nodes
* The `on:` opt is now honoured when playing chords.
* Samplers sound signal now bypasses filter envelope when not used.
* It is now possible to use externally defined FX synths by passing a
  string as the FX name: `with_fx "my_shiny_effect"`. This needs to be
  loaded manually via `load_synthdefs`.
* OS X now supports rates other than 44.1k provided they are similar for
  both audio in and out.
* Run code in `~/.sonic-pi/init.rb` on launch if it exists.  
* If environment variable `SONIC_PI_HOME` is set it will be used over
  `~` as the root of the user's `.sonic-pi` directory (used to auto-save
  files and store logs).
* Default sound card selection is now honoured on Raspberry Pi - so you
  may now use your IQaudIO hat (or similar) out of the box.


### Bugfixes

* Fix number of synth validation errors.
* Fix sporadically failing boot issues on Windows
* Add auto-reboot functionality for audio server. This now detects
  errors in the server (such as a killed or zombied process) and
  automatically reboots it to enable Sonic Pi to continue without a full
  restart. Reboots automatically reload all loaded samples and
  synthdefs.
* `sample_duration` now correctly takes account of TL defaults set by
  `use_sample_defaults`.
* Sampler opts `pitch_stretch`, `beat_stretch` and `rpitch` can now be
  used in TL defaults.

<a name="v2.8"></a>

## Version 2.8 - 'Karlsruhe'
*Friday 20th November, 2015*

This release, named after Karlsruhe, one of the home cities of live
coding, is mainly a maintenance release with a strong focus on both
performance, stability and documentation. This is therefore the fastest
and most stable release of Sonic Pi ever with a massive 10% performance
improvement on the original Raspberry Pi 1. It also ships with new
translations in Polish, Japanese and French. Many of these improvements
(such as the complete rewrite of the OSC stack) are not documented in
this release list but can instead be found in the commit logs over on
Github.

However, not to go unnoticed are a number of exciting new features. For
example we now have a new Band EQ FX, the ability to use MIDI note names
such as `:e5` as values for opts such as `cutoff:`, and new powerful
cutoff envelopes on the sampler.



### Breaking Changes

* Shortcuts for switching buffers have changed. They are now `M-{` and
  `M-}` for switching left and right respectively.
* `sync` no longer inherits BPM by default. Set the `bpm_sync:` opt to
  true to enable BPM inheritance.
* Random seed generation for new threads is now reset on calls to
  `use_random_seed`.

### New Fns

* `octs` - returns a ring of successive octaves.
* `assert` - for raising an exception if the argument is not true.
* `assert_equal` - for raising an exception if the two arguments are not
  equal.
* `bt` - converts the specified time w.r.t. the current BPM.
* `inspect` - similar to `print` but prints the inspected version of the argument.

### GUI

* New translations for Polish, Japanese and French.
* Improve efficiency of logging panel.
* `M-RET` is now a duplicate shortcut for running the code.
* Log title bar is now hidden in full-screen mode.
* Log - don't display └ or ├ if the line is blank, instead display │
* Add sample name autocompletion to more fns such as `sample_duration`.

### Documentation

* New tutorial section on ring chains (chainable functions for modifying rings)
* Tilburg 2 example slightly tweaked for Raspberry Pi 1 compatibility.
* Many minor tweaks and improvements in all areas.

### Synths & FX

* New FX - Band EQ for attenuating or boosting a specific frequency band.
* New synth - DPulse - a detuned pulse wave.
* Sampler now has a cutoff envelope which can be accessed via new opts
  which mirror the standard envelope opts but with a `cutoff_` prefix
  (such as `cutoff_attack`, `cutoff_decay_level` and friends).
* Sampler now correctly handles samples with different sample rates.
* Bitcrusher FX now has an internal low pass filter modifiable via a new `cutoff` opt.
* Panslicer now correctly honours min and max pan values.
* New default opt `on:` for both `sample` and `synth`. This acts like
  `if` but ensures all the opt vals are evaluated (useful if wanting to
  keep the consumption of random streams or ticks consistent even when
  not triggering a synth.
* MIDI opts such as `cutoff:` can now accept note names such as `:c4`.
* FX learned the global `slide:` opt to match synths.

### Improvements

* Massive performance improvements.
* Teach `play_pattern_timed` to handle rings.
* `current_transpose` now returns 0 if there is no current
  transposition.
* BPM scaling is now honoured when controlling synths and FX
* All `with_fx*` fns now return the result of their block.
* `spark` now handles rings correctly.
* `spark` now treats booleans as 1s and 0s so you can now spark rings of bools.
* `puts`, `print` and `mc_message` now handle multiple message arguments


### Bug Fixes

* Ensure `with_fx` doesn't swallow TL modifications such as
  transposition, current synth etc.
* Ensure `with_fx` doesn't affect random seed.
* Improve reliability of boot process on Mac and Windows.
* The FX `pre_amp:` opt is no longer scaled w.r.t. the current BPM.
* Fixed GUI side of update checking system.

<a name="v2.7"></a>

## Version 2.7 - 'Rerezzed'
*Thursday 10th September, 2015*

This release brings a substantial change to the random number
generator. This has the unfortunate side effect of breaking backwards
compatibility.  If you have been using `rand`, `choose`, `shuffle` and
friends to create predictable patterns for your riffs, your code will
produce different results in this release. Please let me apologise and
say it's for a good cause. So what is this good cause?  Well, you can
now jump backwards and forwards through the random stream giving you way
more creative control than before! The random stream is now also unified
with the random stream on the synthesis server allowing you to sync
behaviour between synths and code. Exciting times.

The sampler has also been super charged. We can now easily change the
rate via MIDI note intervals with `rpitch:` and stretch the sample whilst
preserving pitch via `pitch_stretch:` (although with variable results
`;-)`).

Finally you can now control the global mixer with `set_mixer_control!`
for those full filter sweeps over the entire sound...

Have fun and happy live coding!


### Breaking Changes

* Complete rewrite of random number system. This means if you've been
  combining calls to `use_random_seed` with randomisation to create
  predictable melodies/rhythms/timbral manipulations you will
  unfortunately get different results in `v2.7`. This change is to
  synchronise both the Ruby rand stream with the one in SuperCollider as
  well as enabling the reversal of calls to rand via `rand_back`.
* `sync` now causes the BPM to be inherited from the thread calling the
  matching `cue`. This may be disabled with the new `bpm_sync:` opt.
* `rrand` and `rand` now return 0 if called with 0.
* `invert_chord` now handles negative inversions in a more musically
  appropriate manner..


### New Fns

* `ratio_to_pitch` which provides the inverse of `pitch_to_ratio`
* `midi_notes` - returns a ring of numbers (mapping the source
  ring/array through the fn `note`).
* `rand_back` - reverse the random stream and 'undo' calls to `rand`
* `rand_skip` - skip forward through the random stream.
* `rand_reset`- reset the random stream to the last seed.


### GUI

* It is now possible to toggle the commenting of whole selections or
  individual lines with the shortcut `M-/`.
* Added Icelandic translation.

  
### Synths & FX

* All synths learned the `decay_level` opt to allow the sustain phase to
  shift between two distinct values. The default value for `decay_level`
  is to mirror `sustain_level:` thus preserving current behaviour.
* `play` and `synth` have now learned the `pitch:` opt to match
  `sample`. This just increments or decrements the final note.
* `sample` now correctly validates opts.
* `sample` learned the `pitch_stretch:` opt which combines `rate:`
  modification (similar to `beat_stretch:` with `pitch:`. Attempts to
  maintain the pitch whilst simultaneously changing the rate. The
  underlying implementation is very basic and can easily destroy the
  sound.
* `sample` learned the `rpitch:` opt for simple rate pitch
  modulation. This modifies the rate to match the specified number of
  MIDI notes relative from the current pitch. For example, a `rpitch:`
  of 12 would double the rate.
* The unit of the FX `:echo`'s `decay:` opt is now beats and the value
  is scaled with the BPM.


### Examples

* Most examples have been tweaked to sound good with the new random
  generator. 
* Tilburg has been replaced with Tilburg 2. Play it and get your
  Algorave on!


### Improvements

* Auto-align code on Run.
* `live_loop` learned the `seed:` opt which will set the new thread with
  the specified seed prior to initial run.
* Add check to ensure BPM is a positive value.
* `density` has now been taught to handle values between 0 and 1 which
  will now stretch time for the specified block.
* Errors now no longer print out crazy print version of context object
  i.e. #<SonicPiSpiderUser1:0x007fc82e1f79a0>
* Both `in_thread` and `live_loop` have now learned the `delay:` opt
  which will delay the initial execution by the specified number of
  beats.
* Buffer and thread name are now printed on error.  
* `sample_duration` now understands all the opts that you can pass to `sample`  
* It is now possible to do basic arithmetic on symbols representing
  rests: `:r + 3` returns `:r` (a rest plus any MIDI note shift is still
  a rest).


### Bug Fixes
  
* Fixed crash when synth args were specified as Rationals.  
* `note_info` now correctly handles octaves.
* Fix windows paste shortcut `C-v`.
* Teach `invert_chord` how to properly handle out of range index ranges.

<a name="v2.6"></a>

## Version 2.6 - 'Algorave'
*Thursday 30th July, 2015*
[(view commits)](https://github.com/samaaron/sonic-pi/commits/v2.6.0)


The laser beams sliced through the wafts of smoke as the subwoofer
pumped bass deep into the bodies of the crowd. The atmosphere was ripe
with a heady mix of synths and dancing. However something wasn't quite
right in this nightclub. Projected in bright colours above the DJ booth
was futuristic text, moving, dancing, flashing. This wasn't fancy
visuals, it was merely a projection of Sonic Pi running on a Raspberry
Pi. The occupant of the DJ booth wasn't spinning disks or playing MP3s,
she was writing, editing and evaluating code. She was *live coding* and
this was an [Algorave](http://twitter.com/algorave).

This release is codenamed [Algorave](http://twitter.com/algorave) to
celebrate that Sonic Pi is now ready to be performed within nightclubs
as well as still being a fantastic tool for learning how to code
creatively. There are many improvements as detailed below. However,
let's just take a brief look at some of the most fun. Firstly we have
the new error reporting system to make it easier to find and debug your
errors. Syntax errors are now blue and runtime errors pink. We also have
a new look and feel including a new dark mode for performing in dark
places. We also have some fantastic new synths, FX and have even
improved the standard synths. For example, `sample` now lets you stretch
to match the beat with the `beat_stretch:` opt and change pitch with
`pitch:`. The `slicer` FX now sports a fantastic deterministic
`probability:` opt for creating and manipulating rhythmic structures
with ease. Finally there's the new thread local `tick`/`look` system
which will revolutionise the way you work with `ring`s within
`live_loop`s. Of course there's so much more too!

Enjoy this release and happy [Algoraving!](http://algorave.com)

### Breaking Changes

* The `res:` opt for all synths and FX now has range 0->1 rather than
  1->0. This means that a higher res value results in more
  resonance. This will hopefully be more intuitive to beginners and less
  surprising for people with existing synth knowledge.
* The fn `stop` has been renamed to `kill` for killing specific
  synths. In its place a new fn `stop` has been added to stop a given
  thread or `live_loop`.
* `invert_wave` opts are now inverted. The default is now 0 which
  has the same behaviour as the old 1.  This means that it's more
  intuitive to use the opt as to invert the current wave, you now
  specify: `invert_wave: true`, rather than `invert_wave: false`. This
  shouldn't affect any code which doesn't explicitly set the `invert_wave:`
  opt. Pieces which have explicit inversion need to swap all 0s for 1s
  and vice versa.
* The `res:` opt for `rrand` and `rdist` has been renamed to `step:` to
  avoid confusion with the resonance opt for cutoff filters.
* Rename `pitch_ratio` to `pitch_to_ratio` to keep in line with other
  similar fns such as `midi_to_hz`.


### New Fns

* New thread-local (i.e. live_loop local) counter system via fns `tick`
  and `look`.
* New fn `vector` which creates a new kind of Array - an immutable
  vector (`SPVector`) which is now the default base class for all rings.
* New fns `use_sample_defaults` and `with_sample_defaults` which act
  similarly as their `*_synth_defaults` counterparts but for samples not
  synths.
* New fns `use_tuning` and `with_tuning` for exploring other tuning
  systems such as `:just`, `:pythagorean`, and `:meantone`.
* New fn `invert_chord` for chord inversions.  
* New fn `current_beat_duration` for returning the duration of the
  current beat in seconds.
* New fn `note_range` for returning a range of notes between two notes
  with a `pitches:` opt for constraining the choice of filler notes.
* New fns `scale_names` and `chord_names` for returning a ring of all
  chords and scales.
* New example `rerezzed` - strongly influenced by Daft Punk's track
  `derezzed`.
* New example `reich phase` - a nice way of combining `live_loop`s and
  `tick` to create sophisticated polyrhythms.
* New fns `use_cue_logging` and `with_cue_logging` for enabling and
  disabling the logging of `cue` messages.



### GUI  
* New visual look and feel including a new Dark Mode for live coding in
  night clubs.
* New preferences for hiding/showing aspects of the GUI such as the
  buttons, log, tabs etc.
* New preference for full screen mode.
* Improve error message reporting. Syntax errors are now made distinct
  from runtime errors with colour-coded messages. Also, the line number
  of the error is much more visible, and the line of the error is
  highlighted with an arrow in the left-hand margin.
* Workspaces are now named buffers. This is a smaller word which works
  better on lower res screens and is also a lovely term used by a number
  of wonderful programming editors such as Emacs.
* Print friendly messages to the log on boot.  
* Add pref option to check for updates now.


### Synths & FX

* New FX - `krush` for krushing the sound.
* New FX - `panslicer` similar to `slicer` and `wobble` but modulates
  the stereo panning of the audio.
* New synth `subpulse` for a full range pulse with extra bass.
* New synth `blade` - a moody Blade Runner-esque synth violin
* New synth `piano` - a basic piano emulation. Only capable of whole notes.
* FXs `slicer` and `wobble` now have a wonderful new `probability:` opt
  which will only slice on (or off depending on wave inversion) with the
  specified probability. The behaviour is deterministic, so repeated
  calls with the same `seed:` and `probability:` opts will result in the
  same behaviour. Great for adding interesting rhythmic variation to
  sound.
* FXs `slicer` and `wobble` now have smoothing opts for even more
  control over the resulting wave form.
* Teach `sample` the opt `beat_stretch:` for modifying the rate of the
  sample to make sure the duration of the sample is n beats long (with
  respect to the current bpm). Note: stretching the beat *does* change
  the pitch.
* Teach `sample` the opt `pitch` to enable pitch shifting on any sample.  
* FX `flanger`'s feedback mixing is now more fair and is less likely to
  hike up the amplitude.


### Improvements

* Teach `note_info` to also handle a number as its param.
* Teach `factor?` to handle division by 0.
* Teach `load_sample` to throw exception when passed an empty path.
* Now throws an exception when you attempt to create an empty ring.
* Rings are now immutable (in the Clojure sense) which means they can be
  safely passed to multiple threads/live_loops without any issues.
* Teach `use_sample_bpm` the opt `num_beats:` to indicate that a given
  sample consists of a specific number of beats.
* Teach `comment` and `uncomment` to require blocks.  
* Teach synth chord groups to allow their notes to be controlled
  individually to allow transitions between chords.
* Throw nicer exception when unable to normalise synth args  
* Teach `chord` the new opt `invert:` as a shortcut to the new
  `invert_chord` fn.
* Teach `sample_duration` about the opts `start:` and `finish:` and
  envelope opts such as `attack:` and `release:`. This allows you to
  replace any call to `sample` with `sample_duration` to get the exact
  duration of that call.
* Teach `chord` the opt `num_octaves` to enable the easy creation of
  arpeggios.
* It is now possible to set the block type in the Minecraft API.


### Bug Fixes

* Fix bug in `with_sample_pack_as` to now correctly accept a block.
* `mc_surface_teleport` no longer throws an error.
* `Array#shuffle` now works correctly with the random seeds for
  deterministic behaviour.
* Fix broken behaviour with multiple nested calls to `*_sample_bpm`.



<a name="v2.5"></a>

## Version 2.5 - 'Craft'
*Monday 13th April, 2015*
[(view commits)](https://github.com/samaaron/sonic-pi/commits/v2.5.0)

This release comes with support for
[Minecraft: Pi Edition](http://pi.minecraft.net) installed on the
Raspberry Pi. You can now create music with Minecraft visuals or even
code up a synth score in Minecraft blocks and read and play the score
from Sonic Pi! Another exciting aspect of this release is much improved
editor functionality for navigating around and manipulating code via
keyboard shortcuts. This means that live coding just got a lot more
fun. The keyboard shortcuts are based on the standard shortcuts provided
by [GNU Emacs](https://www.gnu.org/software/emacs/) - the oldest and
most powerful text editor in use by wizard programmers today.

### Breaking Changes

* `invert_wave` now defaults to 1 everywhere. I found I always inverted
   the wave every time I used a synth/fx where wave inversion was
   key. This seemed like such a better default I've broken compatibility
   for it. Apologies if this has affected you.
* The `flanger` FX now defaults the optional arg `stereo_invert_wave` to
  1.
* Renamed FX `ring` to `ring_mod` to reduce the potential for confusion
  with the `ring` datastructure.
* `Tab` now indents current line or selection rather than inserting a
  useless tab character.

### New

* Support for programming [Minecraft Pi Edition](http://pi.minecraft.net).
* `sync` now accepts multiple cue ids and will sync on the first matching id.
* New fn `pitch_ratio` for converting a midi note to a frequency
  ratio. Useful for tuning samples.
* New fn `line` for creating a line from start to finish with a specific
  number of slices.
* New fn `spark` for displaying lists of numbers in a fancy text-graph
  (`▁▃▅▇`) in the log.
* On stop, amplitude of output slides down over 1s to silence for a
  smoother transition.
* `sample_duration` now scales result based on current BPM.
* `range` now accepts optional args: `inclusive:` and `step:`.


### GUI
* German translation of GUI and tutorial. Simply open Sonic Pi with a
  machine with a German localisation setting.
* Display GUI fully maximised when opening for first ever time.
* Workspace indexing now starts at 0 to match standard programming indexes.
* New shortcuts - `M-<` and `M->` for switching workspaces.
* Many new Emacs-based code navigation and editing shortcuts. See the
  shortcut cheatsheet in the built-in tutorial for more information.
* Increase height of doc and error panes.
* Improve error pane colour scheme.
* Auto-align now trims whitespace from start and end of buffer.
* Add preference toggle to hide/show line numbers.
* Documentation now supports semantic formatting and highlighting.
* Docsystem tabs are now positioned at the bottom for better navigation.
* New preference to hide/show line numbers in editor.

### Synths & FX

* New FX - `:pitch_shift`  

### Bug Fixes

* Fix OSC lib to properly encode multibyte chars such as UTF8
* Fix sporadic issue on some platforms when trigging percussive sounds
  within a `reverb` FX caused a serious audio overload.
* Add missing fn metadata for `*_sample_bpm`
* Fix synth metadata for FX `:bpf`.
* Fix arg metadata for `use_sample_pack_as`  
* Rings now pretty print themselves as `(ring 1, 2, 3)` rather than `[1, 2, 3]`.
* `C-k` keyboard shortcut now copies text into the clipboard.
* Scales and chords now return actual `ring`s rather than `ring`-like
  things.
* Improve Ring Mod FX arguments
* Exceptions created within `with_fx` are now raised correctly.


 <a name="v2.4"></a>

## Version 2.4 - 'Defrost'
*Wednesday 11th February, 2015*
[(view commits)](https://github.com/samaaron/sonic-pi/commits/v2.4.0)

A quick release following `v2.3` to address an issue with the GUI
freezing on specific CPUs. However, although this release has had a
small development cycle, it ships with three fantastic features. Firstly
we now have the `spread` fn which provides an amazing way to create
interesting rhythms with very little code. Secondly we can now use
`cutoff:` on any sample massively increasing their timbral range and
finally we have three exciting new synths for you to play with. Have
fun!

### Breaking Changes

* Unfortunately 5 pre-release synths accidentally slipped into
  v2.3. Three of them have been polished up and are in this release (one
  with major changes including a name change). However, the other two
  have been removed.

### New

* New fn `spread` for creating rings of Euclidean distributions. Great
  for quickly creating interesting rhythms.
* GUI now automatically appends a `:` to the FX opt autocomplete list  
* Synths and FX now raise an exception if any of their non-modulatable
  opts are modulated. This is disabled when the pref 'check synth
  args' is unchecked.
* GUI now renders pretty UTF-8 `└─` `├─` characters when printing in the log
  on RP.
* Improve docstrings for sample player.

### Synths & FX
* New Synth `:dark_ambience`, an ambient bass trying to escape the
  darkness.
* New Synth `:hollow`, a hollow breathy sound.
* New Synth `:growl`, a deep rumbling growl.
* Sampler synths now sport built-in `rlpf` and `normaliser` FX. These
  are disabled by default (i.e. won't affect sound of the sample) and
  can by enabled via the new `cutoff:`, `res:` and `norm:` opts.

### Bug Fixes

* Fix insanely obscure bug which caused the GUI to freeze on certain
  platforms (32 bit Windows and RP2 with 2G/2G kernel).
* Remove DC Bias offset from Prophet synth (see
  http://en.wikipedia.org/wiki/DC_bias)


<a name="v2.3"></a>

## Version 2.3 - 'Bitcrush'
*Wednesday 28th January, 2015*
[(view commits)](https://github.com/samaaron/sonic-pi/commits/v2.3.0)

### Breaking Changes

* Playing chords with the fn `chord` now divides the amplitude of each
  resulting synth by the number of notes in the chord. This ensures the
  resulting amplitude isn't excessive and is normalised.
* Chords now evaluate their args once and those args are used for all
  synth triggers. This means random values are only generated once and
  are similar across all notes in the chord. Previous behaviour can be
  obtained by calling play multiple times with no interleaved sleeps.
* Ensure each new thread's random number generator is unique yet seeded
  in a deterministic manner. This stops random vals across `at` from
  being identical.
* `range` is now exclusive: `(range 1, 5) #=> (ring 1, 2, 3, 4)`

### New

* New fn `density` for compressing and repeating time Dr Who style. For
  example, wrapping some code with a call to density of 2 will double
  the bpm for that block as well as repeating it twice. This ensures the
  block takes the same amount of time to execute while doing double the
  work.
* New fns `with_bpm_mul` and `use_bpm_mul` which will multiply the
  current bpm by a specified amount. Useful for slowing down and
  speeding up the execution of a specific thread or live_loop.
* New fn `rdist` - generate a random number with a centred distribution
* New examples: `square skit`, `shufflit` and `tilburg`

### Improvements

* Teach control to ignore nil nodes i.e. `control nil, amp: 3` will do
  nothing.
* Teach Float#times to yield a float to the block. For example,
  `3.4.times {|v| puts v}` will yield `0.0`, `1.0` and `2.0`.
* Synth, Sample and FX args now handle bools and nil correctly. `true`
  resolves to `1.0` and `false`, `nil` resolve to `0.0`. This allows you
  to write code such as: `play :e3, invert_wave: true`
* Teach `at` to handle varying block arities differently. See docs for
  more detail. Original behaviour is preserved and only extended.
* App now checks for updates (at most once every 2 weeks). This may be
  disabled in the prefs.
* Teach `:reverb` FX to extend its kill delay time with larger room
  sizes to reduce the chance of clipping.

### Synths & FX

* New FX `bitcrusher` - for crunching and destroying those hi-fi sounds.
* New FX `flanger` - a classic swhooshing effect typically used with
  vocals and guitars.
* New FX `ring` - ring modulation for that friendly Dalek sound
* New FX `bpf` - a band pass filter
* New FX `rbpf` - a resonant band pass filter
* New FX `nbpf` - a normalised band pass filter
* New FX `nrbpf` - a normalised resonant band pass filter

### New Samples

* `perc_snap` - a finger snap
* `perc_snap2` - another finger snap
* `bd_ada` - a bass drum
* `guit_em9` - a lovely guitar arpeggio over Em9

### Bug Fixes

* Namespace `live_loop` fn and thread names to stop them clashing with
  standard user defined threads and fns.
* GUI no longer crashes when you start a line with a symbol.
* `with_fx` now returns the result of the block
* Kill zombie scsynth servers on startup for better crash recovery.
* Handle paths with UTF8 characters gracefully
* Force sample rate for output and input to 44k on OS X. This stops
  scsynth from crashing when output and input sample rates are
  different.


<a name="v2.2"></a>

## Version 2.2 - 'Slicer'
 *Thursday 18th December, 2014*
[(view commits)](https://github.com/samaaron/sonic-pi/commits/v2.2.0)

This release brings a number of nice enhancements. However the main
feature is the accurate timing for triggering FX. This means you can now
reliably use FX for accurate rhythmic purposes such as wobbling, slicing
and echoes.

### Breaking Changes

* `use_sample_pack_as` now uses a double underscore `__` as a separator
  between the user-specified alias and the sample name.

### API Changes

* Teach synth args to take prefixed maps: `play 50, {amp: 0.5}, {release: 2}, amp: 2`
* Don't round Floats when user specifically prints them to log with puts
* `with_fx` FX synths are now triggered using virtual time rather than
  real time. This means that FX can now be used for rhythmical purposes.
* Work on new `RingArray` datastructure. This is essentially an array
  that wraps its indexes so you can use indexes larger than the array size.
* New fn `ring` - `(ring 1, 2, 3)` creates a new ring array.
* New fn `knit` - `(knit :a1, 2, :c1, 1)` returns `(ring :a1, :a1, :c1)`
* New fn `bools` - `(bools 1, 0, 1)` returns `(ring true, false, true)`
* New fn `range` - `(range 70, 100, 10)` returns `(ring 70, 80, 90, 100)`
* New fn `sample_loaded?` - to detect whether a specific sample has been loaded

### Synths & FX

* Fixed regression in `:tb303` synth - sound is reverted to v2.0 behaviour
* New Synth - `:square` - Pure square wave

### GUI

* Help system now autodocks on close
* Preferences are now remembered across sessions
* On Raspberry Pi, previous volume and audio output options are forced
  on boot.

### New Samples

* `bd_tek` - Bass drum

### Bug fixes

* `one_in` now returns false if num is < 1
* Ensure `live_loop`'s no-sleep detector works within nested `with_fx` blocks
* `chord` now returns a ring.

## Version 2.1.1
*Tuesday 25th November, 2014*
[(view commits)](https://github.com/samaaron/sonic-pi/commits/v2.1.1)

* Windows version no longer needs special firewall exceptions to run
* Added license information to info window
* Minor grammar and spelling tweaks to tutorial


<a name="v2.1"></a>

## Version 2.1 - 'Core'
*Friday 21st November, 2014*
[(view commits)](https://github.com/samaaron/sonic-pi/commits/v2.1.0)

The focus of release is very much on technical improvements, efficiency
and general polish.

The most obvious and exciting change is the introduction of the
`live_loop` which will change the way you work with Sonic Pi. For more
information on `live_loop` take a look at the new section in the
tutorial. Another very exciting development is that v2.1 marks the
official support for Windows thanks to the excellent work by Jeremy
Weatherford. Finally, this release is also the first release where Sonic
Pi has a Core Team of developers. Please give a warm welcome to Xavier
Riley, Jeremy Weatherford and Joseph Wilk.


### API Changes
* New fn `live_loop` - A loop for live coding
* New fn `inc` - increment
* New fn `dec` - decrement
* New fn `quantise` - quantise a value to resolution
* New fn `factor?` - Factor test
* New fn `at` - Run a block at the given times
* New fn `degree` - for resolving a note in a scale by degree such as `:i`, `:iv`
* New fn `chord_degree` - Construct chords based on scale degrees
* New TL fn `use_sample_bpm` - for changing the BPM based on a sample's duration
* New fn `rest?` - Determine if note or args is a rest
* New fn `vt` - Get virtual time
* New fn `set_control_delta!` - Set control delta globally
* `wait` now handles both `sleep` and `sync` functionality
* Allow first arg to `play` to be a proc or lambda. In which case simple call it and use the result as the note
* Teach `play` to accept a single map argument (in which case it will extract `:note` key out if it exists.
* Fns `play` and `synth` now treat 'notes' `nil`, `:r` and `:rest` as rests and don't trigger any synths.


### GUI Modifications

* Updated and improved tutorial
* Much improved autocompletion
* Add HPF, LPF, mono forcer and stereo swapping preferences to new studio section for use when performing with Sonic Pi through an external PA.
* Shortcuts overhauled - now supports basic Emacs-style Ctrl-* navigation.
* Shortcuts Alt-[ and Alt-] now cycle through workspaces
* Shortcuts now work when toolbar is hidden
* Font sizes for individual workspaces are now stored between sessions
* Ctl-Mouse-wheel zooms font on Windows
* Links are now clickable (opening external browser)
* Entries  in docsystem's synth arg table are now clickable and will take focus down to arg documentation
* Stop users accidentally clearing entire workspace if they type quickly after hitting run
* Hitting F1 or `C-i` over a function name now opens up the doc system at the relevant place

### Bugs/Improvements
* Reworked examples.
* Much improved efficiency in many areas - especially for Raspberry Pi.
* Avoid occasional clicking sound when stopping runs
* Note Cb is now correctly resolved to be a semitone lower than C
* Non RP systems now start with more audio busses (1024)
* Array#sample and Array#shuffle are now correctly seeded with thread local random generator
* Log files are now placed into ~/.sonic-pi/log
* Chords and scales now wrap around when accessed from indexes outside of their range.
* `rand_i` and `rrand_i` now correctly return integers rather than floats
* rrand arguments now correctly handle a range of 0 (i.e. min == max)
* Line offset in error messages is now correct
* When saving files on Windows, CRLF line endings are used
* Stop users defining functions with same name as core API fns


### Synths, Samples & FX
* New samples (bass drums, snares and loops)
* Allow `mod_range:` opt to have negative values (for oscillating with lower notes)
* Change slide mechanism to default to linear sliding with support for changing the curve type. All modifiable args now have corresponding  `_slide_shape` and `_slide_curve` args.
* Improve TB303 synth - now supports separate cutoff ADSR envelopes. New opts:
  - `cutoff_attack:`,
  - `cutoff_sustain:`,
  - `cutoff_decay:`,
  - `cutoff_release:`,
  - `cutoff_min_slide:`,
  - `cutoff_attack_level:`,
  - `cutoff_sustain_level:`,
  - `cutoff_env_curve:`


## Version 2.0.1
*Tuesday 9th September, 2014*
[(view commits)](https://github.com/samaaron/sonic-pi/commits/v2.0.1)

* Fix recording functionality
* Improve documentation content and layout
* Close off OSC server from external clients
* Add History, Contributors and Community pages to info window
* Improve startup speed on OS X
* Re-work and add to shortcuts for key actions:
  - on RP they are all `alt-*` prefixed
  - on OS X they are all `cmd-*` prefixed
* Improve highlighting of log messages (`cue`/`sync` messages are more clearly highlighted)
* Log now communicates when a run has completed executing
* Fix bug encountered when stopping threads in super fast loops (stopped comms with server)


<a name="v2.0"></a>

## Version 2.0 - 'Phoenix'
*Tuesday 2nd September, 2014*
[(view commits)](https://github.com/samaaron/sonic-pi/commits/v2.0.0)

* Complete rewrite since v1.0
* Support for Live Coding - redefining behaviour whilst music is playing
* New timing system - timing now guaranteed to be accurate
* Many new synths
* New chainable studio FX system
* Support for sample playback
* Inclusion of over 70 CC0 licensed samples from http://freesound.org
* Support for controlling and modifying synth, fx and sample playback
  arguments after they have been triggered
* Completely re-designed GUI
* Help system with full documentation, tutorial and many examples
* Record functionality (for recording performances/pieces)
* Support for controlling system audio settings on RP
