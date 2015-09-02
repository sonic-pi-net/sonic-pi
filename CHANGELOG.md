# History
* [v2.7-dev](#v2.7), In development
* [v2.6 'Algorave'](#v2.6), 30th July, 2015
* [v2.5 'Craft'](#v2.5), 13th April, 2015
* [v2.4 'Defrost'](#v2.4), 11th Feb, 2015
* [v2.3 'Bitcrush'](#v2.3), 28th Jan, 2015
* [v2.2 'Slicer'](#v2.2), 18th Dec, 2014
* [v2.1 'Core'](#v2.1), 21st Nov, 2014
* [v2.0 'Phoenix'](#v2.0), 2nd Sept, 2014

<a name="v2.7"></a>
## Version 2.7
*In development*

### Breaking Changes

* `sync` now causes the BPM to be inherited from the thread calling the
  matching `cue`. This may be disabled with the new `bpm_sync:` opt.
* Complete rewrite of random number system. This means if you've been
  combining calls to `use_random_seed` with randomisation to create
  predictable melodies/rhythms/timbral manipulations you will
  unfortunately get different results in `v2.7`. This change is to
  synchronise both the Ruby rand stream with the one in SuperCollider as
  well as enabling the reversal of calls to rand (undo rand!)
* `rrand` and `rand` now only return 0 if called with 0.

### New Fns

* `ratio_to_pitch` which provides the inverse of `pitch_to_ratio`
* `midi_notes` - returns a ring of numbers (mapping the source
  ring/array through the fn `note`).

### GUI

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
  * FX `:echo`'s `decay_level:` opt is now scaled with the BPM.

### Examples

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
* It is now possible to do basic arethmetic on symbols representing
  rests: `:r + 3` returns `:r` (a rest plus any MIDI note shift is still
  a rest).
  
  ### Bug Fixes
  
* Fixed crash when synth args were specified as Rationals.  
* `note_info` now correctly handles octaves.
* Fix windows paste shortcut `C-v`.


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
