# History
* [v4.3.0 'Pathfinder'](#v4.3.0), 30th Sept, 2022
* [v4.2.0 'Batch'](#v4.2.0), 14th Sept, 2022
* [v4.1.0 'Phase'](#v4.1.0), 30th August 2022
* [v4.0.3 'Dovetail'](#v4.0.3), 18th July 2022
* [v4.0.2 'Relink'](#v4.0.2), 15th July 2022
* [v4.0.1 'Verknüpfen'](#v4.0.1), 12th July 2022
* [v4.0.0 'Link'](#v4.0.0), 6th July, 2022
* [v3.3.1 'Beamer'](#v3.3.1), 1st Feb, 2021
* [v3.3 'Beam'](#v3.3), 28th Jan, 2021
* [v3.2.2 'Tau3'](#v3.2.2), 5th April, 2020
* [v3.2.1 'Take Tau'](#v3.2.1), 3rd April, 2020
* [v3.2.0 'Tau'](#v3.2.0), 28th Feb, 2020
* [v3.1.0 'Sauna'](#v3.1.0), 23rd Jan, 2018
* [v3.0.1 'IOIO'](#v3.0.1), 27th July, 2017
* [v3.0 'IO'](#v3.0), 18th July, 2017
* [v2.11.1 'Hack'](#v2.11.1), 16th Dec, 2016
* [v2.11 'Time Warp'](#v2.11), 3rd Nov, 2016
* [v2.10 'Cowbell'](#v2.10), 15th April, 2016
* [v2.9 'Venster'](#v2.9), 31st Dec, 2015
* [v2.8 'Karlsruhe'](#v2.8), 20th Nov, 2015
* [v2.7 'Rerezzed'](#v2.7), 10th Sept, 2015
* [v2.6 'Algorave'](#v2.6), 30th July, 2015
* [v2.5 'Craft'](#v2.5), 13th April, 2015
* [v2.4 'Defrost'](#v2.4), 11th Feb, 2015
* [v2.3 'Bitcrush'](#v2.3), 28th Jan, 2015
* [v2.2 'Slicer'](#v2.2), 18th Dec, 2014
* [v2.1.1 'Firewall'](#v2.1.1), 25th Nov, 2014
* [v2.1 'Core'](#v2.1), 21st Nov, 2014
* [v2.0.1](#v2.0.1), 9th Sept, 2014
* [v2.0 'Phoenix'](#v2.0), 2nd Sept, 2014

<a name="v4.3.0"></a>
## Version 4.3.0 'Pathfinder'
<!-- [(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v4.3.0): -->

This is another maintenance and bugfix release - primarily addressing a critical issue with configuring external sound cards for audio input. This was accidentally introduced in v4.2. An issue with the auto-indentation preference in the GUI has also been addressed.

The GUI is now restricted to having one instance open at any one time. Having multiple instances of Sonic Pi running simultaneously can cause conflicts. This is now no longer possible, which should reduce any issues users may have observed when accidentally opening the app multiple times in quick succession. Now, only the first instance will open and all the subsequent others will exit before starting if they detect another instance already running.

### GUI
* Restrict running instances to only allow one instance of Sonic Pi to be running at any one time on a given machine.

### Translations
* Improvements to the Dutch, Estonian, Japanese and Polish translations.

### Bugfixes
* The behaviour of external audio card configuration should now be as it was in v4.1.
* The auto-indent preference in the GUI now works as expected. Unchecking will stop the GUI from automatically reformatting the code.

<a name="v4.2.0"></a>

## Version 4.2.0 'Batch'
14th September 2022
<!-- [(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v4.2.0): -->

The main purpose of this release is to address a booting issue discovered on a school Windows cluster with common security restrictions. This issue was reported by Adam Cooper from Moor Park High School & Sixth Form in the UK. Adam was wonderfully helpful in finding the specific issue and testing potential solutions. Thanks Adam!

If you happen to have had any trouble running v4 of Sonic Pi on your school clusters, please give this release a go! (Note, this issue didn't affect v3 of Sonic Pi).

In addition, there have been some translation improvements, Global Time Warp now has a greater range and a couple of `time_warp` related bugs were found and dealt with. Have fun!

### GUI
* Change Global Time Warp range from +-250 to -250 -> 999. This lets users access the full phase down to a BPM of 48.

### Improvements
* Booting process on Windows has been modified to enable it to run when executing in an environment which restricts access to CMD (which prevents terminal usage and running .bat files). This is typical in schools.
* Sonic Pi stores all its history, configuration and logs in a folder called `.sonic-pi` which is typically placed in the user's home directory. If the user happens to not have permission to write to their home directory, Sonic Pi now raises a descriptive error on boot and explains that the fix is to set an environment variable called `SONIC_PI_HOME` to point to a directory the user does have permission to write to.

### Translations
* Improvements to the Dutch, Hungarian and Indonesian translations.

### Bugfixes
* Teach timing safety system that `time_warp` doesn't sleep (even if it contains calls to `sleep` internally). This now means that wrapping all code within a `live_loop` with `time_warp` no longer causes the thread to spin out of control. It instead reports that the live loop did not sleep as expected.
* Throw error when attempting to call `sync` within a `time_warp`. This is because time warps are effectively timeless and `sync` asks the running thread to wait for an unknown period (until a matching event appears). Attempting to wait within a timeless context is clearly nonsensical!

<a name="v4.1.0"></a>

## Version 4.1.0 'Phase'
30th August 2022
<!-- [(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v4.1.0): -->

The release of v4 of Sonic Pi introduced the ability to change the tempo of live running code to an external beat. This is possible either by setting the tempo manually to a specific BPM (Beats Per Minute) or by using the Tap Tempo button to tap along to an external rhythm. However, even when you do this, it's quite likely that things still don't sound quite right as there's a good chance that the two rhythms are out of phase despite being at the same tempo. This release introduces a new feature - Global Time Warp which lets you shift the phase of Sonic Pi's timing back or forward up to 250 milliseconds to let you align it with an external rhythm.

Taking a quick step back, what does it mean for two rhythms to be out of phase and why is it a problem? Imagine you had two metronomes ticking away. If one has a higher tempo than the other, then one will be ticking faster than the other. This can be fixed by setting the tempos to be the same. However, unless they were both started at exactly the same time, they both wont be ticking at the same time. Instead, one will tick, then the other, then the first, then the second, etc. The second will always be a little later than the first and as they are at the same tempo, they will never catch up with each other, they will always tick at different times. This is called being out of phase. When two rhythms are out of phase it can sound very messy. Therefore, being able to get rhythms to start at exactly the same time is really important to being able to jam together and for it to sound good.

The new Global Time Warp control lets you shift the timing of all triggered synths, MIDI and OSC messages forwards or backwards by a specified time in milliseconds. You can either slide a slider or type in a specific time to adjust the phase of Sonic Pi's timing to match the phase of an external rhythm.

This means you can now easily jam to an external track - first by tapping out the tempo and then by shifting the phase using the new Global Time Warp controls until it sounds right. This is essentially what DJs do when they mix tracks together in a live set - only now you can layer coded rhythms, basslines and riffs on top of external music with code. This is not only a great way to perform live, but it's also a wonderful way to practice - by jamming along to music you love.

In addition to this, the accuracy of the Tap Tempo functionality has been improved in addition to UI tweaks to all the metronome controls.

Have fun jamming in phase to external music and take your live coding collaborations to the next level!


### GUI
* New Global Time Warp controls. This lets you adjust the timing of all scheduled events (synths, samples, MIDI and OSC) forwards or backwards up to 250 milliseconds. This is important for matching the phase of an external rhythm.
* Improved Tap Tempo logic aimed towards increased BPM detection accuracy and speed.
* BPM scrubber and new Global Time Warp scrubber now respond to mouse wheel changes.

### Improvements
* Clearly specify in the tutorial that user-defined function names should start with a lower case letter. Therefore use `define :foo` rather than `define :Foo`.

### Translations
* Minor improvements to the Indonesian, Korean Portuguese (Brazil) and Russian translations.

### Bugfixes
* The `duration:` opt for `midi_clock_beat` has been fixed.
* Fix `scsynth_opts` and `scsynth_opts_override` options in the `~/.sonic-pi/config/audio-settings.toml` config file.
* Audio output via external soundcards on macOS should now work without requiring audio inputs to be enabled in the GUI.
* `current_debug` now correctly returns `true` if logging/debug is enabled (the default). Previously it reported the inverse.
* Minor improvements to documentation layout.


<a name="v4.0.3"></a>

## Version 4.0.3 'Dovetail'
18th July 2022
<!-- [(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v4.0.3): -->

This is a minor bugfix and maintenance release.

### Improvements
* The docs for `beat` have been updated to reflect the beat value can no longer be assumed to start at `0`. This changed since Ableton Link integration in v4 which now defines the initial beat value based on its internal state.

### Translations
* Minor improvements to the Vietnamese translation.

### Bugfixes
* A regression in the function `midi_sysex` introduced in v4 has been resolved. `midi_sysex` can now be called as expected.
<a name="v4.0.2"></a>

## Version 4.0.2 'Relink'
15th July 2022
<!-- [(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v4.0.2): -->

Hot on the heals of v4.0.1 here's another bugfix release addressing an issue with the translations.

During the last year there has been significant upheaval of the process used to build Sonic Pi. Somehow, during this upheaval, the translations never got added into the final release builds, despite being part of the dev builds. This has been true since v4.0 BETA 1 back in August 2021 - yet it somehow went unnoticed until now. Apologies! This release fixes this and re-introduces the translations which are so important for lowering the barrier to entry across the world. 

As a bonus, a couple of nice improvements also managed to sneak in for good measure. Have fun!

### GUI
* All the translations are now available again - including the vast amount of new translation effort since v3.3.1 was released.
* Loading a file no longer erases the undo history. This means that you can now "undo" a file load in the current buffer and return it to the state prior to the load.
* The BPM-scrubbing behaviour (clicking your mouse and dragging to change the BPM) has been tweaked to make it easier to use.
* Improvements for the Portuguese translation.


<a name="v4.0.1"></a>

## Version 4.0.1 'Verknüpfen'
12th July 2022
<!-- [(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v4.0.1): -->

This is a bugfix release addressing a startup issue for a number of users with non-ASCII characters within audio harware information on macOS.

We also sadly say goodbye to Core Team member Hanno Zulla who did amazing work with Linux packaging - especially getting Sonic Pi into Debian. Hanno also spearheaded the translation work setting up the crowdsourced translation system which has now enabled Sonic Pi to be translated into over 50 languages. Thank-you so much Hanno - we wish you success and happiness in all you do going forwards.

This release also includes Italian and Polish translation updates.


<a name="v4.0.0"></a>

## Version 4.0.0 'Link'
6th July, 2022
<!-- [(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v4.0.0): -->

Get ready to Jam with v4 of Sonic Pi. 

In this release, the internal timing algorithms have been completely refactored to enable them to link to a new global metronome. The GUI now features a new metronome panel which will let you dynamically _change the BPM_ of your code _whilst it is running_. You can also use the new "Tap Tempo" button to click out a tempo and Sonic Pi will automatically shift to play in time with your taps. Additionally, you can link the metronome with other versions of Sonic Pi running on other computers connected to the same network (wifi or ethernet). This means it is now possible to gather some laptops together to form an ensemble or turn a whole classroom of computers into an orchestra of live coders - and for everyone to be in time with each other. You can even dynamically change the BPM for _everyone on the network_ whilst code is _running on everyone's computers independently_ and everything _maintains synchronised_ and _in time_. It's a real jam. 

Finally, you're not limited to syncing tempo with other copies of Sonic Pi. The new global metronome is built on top of a wonderful piece of technology called [Link](https://www.ableton.com/en/link/) which was developed and open sourced by [Ableton](https://www.ableton.com) who also make [Live](https://www.ableton.com/en/live/) and [Push](https://www.ableton.com/en/push/), both widely used by professional musicians and also automatically sync using Link. Additionally there are also over [200 other apps or hardware products](https://www.ableton.com/en/link/products/) that also sync with Link. Sonic Pi can therefore now automatically jam with any of these with just a click of a button. It's so exciting to imagine the new kinds of bands and collaborations that are going to be possible with this new timing system. 
 
There's also a large number of other improvements and fixes within this release that together combine to form the most powerful and exciting release of Sonic Pi yet. See below for a full breakdown of visible changes.

This release would not have been possible without the huge number of fabulous contributors that have been involved. Key contributions have been a huge refactoring of our GUI by Chris Maughan, the wrapping of Link as an Erlang NIF by Luis Lloret and the tireless and constant attention to detail by Ethan Crawford. Thanks so much to you all. Thanks also to José Valim for financially supporting development during a tricky period. José is the creator of the programming language Elixir which is now integrated into the internals of Sonic Pi. Elixir opens up a rich world of exciting opportunities within Sonic Pi which we have already started exploring with earnest. Expect exciting things for the future and consider becoming a [Patreon supporter](https://patreon.com/samaaron) to automatically get access to all upcoming BETAs..... 

Finally, we would love for you all to extend a warm welcome to the latest member of the Sonic Pi Core Team: Lily Foster. She brings a huge depth of Linux knowledge to the team and we're terrifically excited to have her on board.

Now, go and get your Live Coded Jam On!


### Breaking Changes
* The default BPM is now set to the new global Link metronome. This is set to 60 by default, so on the surface the behaviour will appear identical. However, if you change the Link metronome BPM using the GUI or the new fn `set_link_bpm!` it will change the BPM for all threads dynamically. Previous behaviour (a static BPM of 60 unlinked to the global metronome) can be obtained by starting your code with `use_bpm 60`.
* Previously it was possible that the `onset:` option for `sample` silently ignored the last onset of a given sample. This has now been fixed. Some samples may therefore have an additional onset index which won't affect any code using earlier indexes but will affect code which uses indexes larger than the number of onsets (and therefore relying on the index wrapping behaviour).
* The Minecraft Pi Edition API has been removed (all `mc_` fns). Minecraft Pi Edition appears to no longer ship on Raspberry Pi OS and the Pi Edition API is not the same as the standard Minecraft API.
* The main mixer settings have been modified to reduce load on the limiter in common scenaries with the GUI volume slider set to a high position. This caused distortion and other unwanted audio issues. This change should be an improvement for most users - however if you wish to return to the old behaviour, you can run the following at the start of your session: `set_mixer_control! pre_amp: 1, amp: 1`.
* The value returned by `beat` no longer starts at `0`. Instead it is now initialised with Ableton Link's current beat value. Other than the initial value not being `0`, `beat`'s behaviour remains unchanged.

### New 
* Support for [Ableton Link](https://www.ableton.com/link/). This enables you to synchronise the tempo of Sonic Pi running on multiple computers connected on the same network. It will also enable automatic BPM synchronisation with music production tools such as Ableton Live, VJ tools such as Resolume, DJ hardware such as the MPC and many compatible iPad music apps. For a full list see: https://www.ableton.com/link/products/,,
* New `:link` option to fn `use_bpm`. This enables Link mode for the current thread which automatically syncs the BPM to the Link metronome (which also syncs it with all other Link-capable apps running on any computer connected to the local (wired or wifi) network.
* New fn `link` which sets the BPM to a new `:link` mode and also waits until the start of the next bar (as determined by Link) before continuing. This lets you automatically sync tempo and beat phase in one command. 
* New fn `link_sync` which sets the BPM to a new `:link` mode, waits for the Link session to be playing and also waits until the start of the next bar (as determined by Link) before continuing. This lets you automatically "arm" Sonic Pi to sync tempo and beat phase and wait for an external "play" command from another Link device - such as Ableton Live.
* New fn `set_link_bpm!` to change the BPM/tempo of the Link metronome (and simultaneously change the tempo of all connected Link-capable apps on the network).
* New fn `current_random_source` which returns the current random number source kind (see `use_random_source`).
* New fn `load_synthdef` which lets you load a single synthdef file.
* `load_synthdefs` now loads both directories and single files (by dispatching to `load_synthdef` where necessary).



### Synths & FX
* New synth `:winwood_lead` - a lead synth inspired by the Winwood songs from the early 80s.
* New synth `:bass_foundation` - a soft bass synth inspired by the sounds of the 80s.
* New synth `:bass_highend` - an addition to the :bass_foundation synth inspired by the sounds of the 80s.


### Examples
* New Algomancer example - Blockgame coded by DJ_Dave.
* New Sorcerer example - Lorezzed.


### GUI
* Preference pane is now an overlay which hovers over the main window. This means that opening and closing it does not inadvertently modify a carefully chosen layout e.g. for a performance.
* New preference option to show and hide the pane titles such as Scope, Log, Cues, Context, Help, etc.
* New preference option to hide the menubar when in fullscreen mode (Windows and Linux only as this behaviour is standard on macOS).
* New preference option for enabling audio inputs (now disabled by default). Modifying this setting requires a reboot to take effect.
* New preference option for selecting the default language translation. Modifying this setting requires a reboot to take effect.
* Preferences now show audio hardware information such as connected input/out devices, sample rate, block size etc. The amount of information displayed varies by platform.
* New GUI controls for interacting with the new Link Metronome. You can connect/disconnect to the network (to share tempo with others), change the tempo (in BPM) and tap out a new tempo.
* Increase width of panel dividers and highlight on mouse hover.
* Highlight scrollbars and preference checkbox descriptions on mouse hover.
* Scrollbars now have rounded edges.
* Teach autocompletion about random source choices: `:white`, `:light_pink`, `:pink`, etc.
* Improve syntax indentation.
* Improvements for Arabic, Catalan, Chinese (Simplified), Dutch, Estonian, French, German, Italian, Japanese, Korean, Persian, Polish, Portuguese (Brazil), Russian, Sinhala, Spanish, Swedish, Ukrainian.
* Introduced new translations for Basque, Gaelic.


### Improvements
* Many minor documentation fixes and improvements.
* When running on Raspberry Pi, Sonic Pi connects to PulseAudio by default.
* The scheduling accuracy of outgoing OSC and MIDI messages is improved on Windows.
* Optimise `midi_clock_beat`.
* `note_range` can now handle both increasing and decreasing note ranges.
* No longer connect to the input sound device on macOS by default - this also means audio input/output rate mismatches no longer cause boot issues on macOS by default.
* SuperCollider audio server boot issues are now immediately detected and reported as a specific issue to the user alongside the full scsynth log file. 


### Bugfixes
* Improve robustness of outgoing OSC messages in the case where an outgoing hostname is malformed and can't be resolved. 
* Synths `:dull_bell` and `:pretty_bell` now properly free themselves when they finish playing, which now means the resources they consumed are also properly freed. 
* Indexing into an empty ring no longer causes a divide by zero error.
* No longer attempt to increase audio server priority on Windows which causes booting errors in some cases.
* Fixed encoding issues when saving/loading files containing non-ascii characters on Windows.
* `range` no longer loops infinitely with a step size of 0. Instead it now throws an error.
* In some circumstances having the lissajous visualiser visible caused the GUI to crash on startup. This has now been addressed.
* Stop thread hanging when passing `0` as the first argument to `spread`. Now just returns a ring of false values.


<a name="v3.3.1"></a>

## Version 3.3.1 - 'Beamer'
1st Feb, 2021
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v3.3.1):

This is a compatibility release addressing two issues with macOS Big Sur:

1. Correct language translations now used based on user's current locale
2. The language server is no longer killed by macOS Gatekeeper when a
   sample's onset times are queried.
   
This release also includes minor translation updates.

   

<a name="v3.3"></a>

## Version 3.3.0 - 'Beam'
28th Jan, 2021
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v3.3.0):

Ten months of development, over 700 individual commits, loving
contributions from many many people have gone into this new version of
Sonic Pi. This release is also the result of a 3 month long community
beta process which took place over on
[Patreon](https://patreon.com/samaaron) where supporters got access
to early releases and really helped polish and shape development. Huge
thanks to everyone that has supported continued development. We're
really excited and proud to share this new release with you all.

In terms of significant changes, there have been many internal
improvements, including a full rewrite of our MIDI subsystem. This
shouldn't result in much change as a user (although some MIDI cue names
might have changed slightly) but it really sets us up for some exciting
developments in the future. We're essentially moving much more of the
core input/output functionality of Sonic Pi into a special system called
the BEAM and a language called [Erlang](https://www.erlang.org). This is
a system that has a strong emphasis on low latency, low jitter and high
concurrency which is exactly what the IO of a music system requires!
We're so excited to see what we'll be able to achieve with this new
approach that the BEAM features as the codename of this release.

Another area that has seen a lot of love and attention is our strong
focus on accessibility. We now have a much improved menu bar which
essentially duplicates much of the functionality found in the
preferences but in a format that's extremely easy to work with via a
screen reader. We've also added a new context pane that displays the
current line and character position as a direct result of a
collaboration with Leiden University's accessibility group. There have
already been numerous studies that are reporting success for blind and
low-vision users of Sonic Pi - so we're confident we're on the right
track with respect to accessibility. We're also not stopping here and are
always looking for ways to lower the many barriers to entry for creative
experiences with code for everyone.

This is also the first release where we officially welcome Chris Maughan
as the latest member of the Sonic Pi core team. Chris has made
significant contributions over the last year including improvements to
the scope and a complete reworking of our build process. I hope you join
us in welcoming Chris to the team.

Finally, we have official support for macOS Big Sur, some lovely new
synths and FX, improved translations, support for advanced configuration
(audio and colour theming), new Turkish scales and many, many bug fixes
and improvements.

We really hope you love this version of Sonic Pi as much as we do.

Happy Live Coding!


### Breaking Changes
* MIDI port description names may be slightly different from previous versions. Precise matches on MIDI ports via `sync` and `get` will have to be updated accordingly.
* Incoming MIDI cues now no longer include clock tick events. Optionally re-enabling these and new ways of working with incoming timing systems will feature in a future release.


### New 
* Completely new MIDI subsystem.
* `use/with_random_source` - change the current random stream used when selecting random values. We now have `:white`, `:light_pink`, `:pink`, `:dark_pink` and `:perlin`.  Default is `:white` which is the same stream as previous releases to preserve compatibility.
* Many Turkish scales (makams) have been added.
* New user config directory for fine tuning aspects of the app such as advanced audio settings. See `~/.sonic-pi/config/README.md` for more information.


### Synths & FX
* New synth `:rodeo` - emulating an electric piano.
* New synth `:kalimba` - an African thumb piano.
* Add sliding to `:autotuner` FX opts `note:` and `formant_ratio:`.
* `mix`, `pre_mix` and `pre_amp` opts are now affected by `slide:`.


### GUI
* New context pane which displays the current line number and position making this information accessible to screen readers. This may be shown/hidden with a preference setting.
* New keyboard shortcuts for switching directly to a buffer. S-M-1 (hold shift and Meta and the number 1) will jump to buffer 1. Meta is Cmd on macOS and alt on Windows/Linux.
* Autocompletion system can now be enabled or disabled. This is intended to help those using screen readers for which the autocompletion system is currently incompatible. 
* Sample listings in the help system now have a handy play button that allows you to preview them by clicking rather than having to write any code.
* Improvements for Catalan, Chinese, Danish, Dutch, Finnish, French, German, Hebrew, Hungarian, Icelandic, Italian, Korean, Norwegian, Polish, Portuguese, Portuguese (Brazil), Russian, Slovak, Spanish, Swedish, Thai, Turkish and Vietnamese translations.
* Sinhalese translation added.
* GUI now has a much more complete menu bar which duplicates the functionality of the preferences pane whilst making it easily accessible to screen readers.
* MIDI port names are now autocompleted.
* The GUI now correctly saves and restores the visibility preferences for the buttons, tabs and log pane.



### Improvements
* Many minor improvements to the documentation.
* Unsent external MIDI messages are now flushed and no longer sent after hitting the Stop button.
* The common mistake of calling both `play` and `sample` on the same line such as `play sample :loop_amen` is now an error with an appropriately helpful description to explain that `play` should be removed.

### Bugfixes
* Fix `pulse_width_curve:` and `pulse_width_slide:` opts which were incorrectly swapped on `pulse`, `subpulse` and `dpulse` synths.
* Improve boot on Windows systems that have default locale code pages currently unsupported by Ruby. We now force UTF-8.
* Fix issue causing the incorrect error line to be highlighted in the GUI.
* Fix issue when syncing with cues with `-` symbols in their paths.
* Fix bug with `use_merged_synth_defaults` after a call to `use_synth_defaults`. This bug was triggered in the "Sonic Dreams" example which now plays through correctly again.
* MIDI device hotswapping (detecting whether devices were connected or disconnected) now works on macOS.
* Fix `.take_last` to no longer return `nil` for min and max of the range.
* Fix off-by-one error in `line` fn. Now should always return rings of the correct length.



<a name="v3.2.2"></a>

## Version 3.2.2 - 'Tau3'
*5th April, 2020*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v3.2.2):

This is a minor bugfix and maintenance release.

### GUI
* Significant improvements to the Danish and Swedish translations.

### Bugfixes
* Fixed a build issue on macOS that stopped recording from working.
* Fixed issues rendering images in the documentation for non-English translations.


<a name="v3.2.1"></a>

## Version 3.2.1 - 'Take Tau'
*3rd April, 2020*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v3.2.1):

This release is another maintenance release with a primary focus on
increasing stability and reliability. There are therefore no major new
features, but instead there are significant improvements to the
internals and the packaging of the Windows installer.

Whilst there are no major new features, we have still been working hard
engaging with users with accessibility requirements. We have added a
number of modifications to the GUI to improve keyboard navigation which
should improve things for users that do not use a mouse (for example,
users working with a screen reader). As mentioned in the previous
release, we will continue to work to improve accessibility support
wherever possible in each release.

One core internal focus has been improving memory utilisation of the
app. A number of memory leaks have been hunted down and fixed. A memory
leak is where the application requests increasingly more memory from the
computer and never gives it back. Over time this means that the app
would waste a lot of the system memory. This might have not been too
noticeable using Sonic Pi on modern computers that typically have
incredibly large amounts of memory. However, it was noticeable and
problematic on low-powered systems with limited memory capacity. Sonic
Pi now uses a lot less memory and after a warm-up period, the memory
usage stays much more constant than before. We will continue to look for
and fix memory leaks going forward.

Finally, this release is the first to ship with a fully signed Windows
installer. This is a major milestone as we now ship with signed versions
for both macOS and Windows which should help more users and
organisations (especially schools) trust the installation and running of
Sonic Pi.

Now, go and get live coding! 

### Breaking Changes

* The `ping_pong` FX now merges the dry sound with the result of the
  echo to match the behaviour of `echo`.
* In the last release, the `piano` synth had its default amplitude
  accidentally altered when being updated to work with arbitrary
  frequencies. This has been restored.
* Incoming MIDI Active Sensing messages are now ignored

### GUI

* New shortcuts and menu items for explicitly placing focus on specific
  areas of the app. This is primarily intended to improve accessibility
  for screen readers or those not using a mouse or trackpad. The new
  shortcuts are:
  
  - `Control-shift-e` - move focus to editor
  - `Control-shift-l` - move focus to logs
  - `Control-shift-c` - move focus to cues
  - `Control-shift-p` - move focus to preferences
  - `Control-shift-h` - move focus to help listing (e.g. the list of available help section)
  - `Control-shift-d` - move focus to help details (e.g. the contents of the chosen help section)
  - `Control-shift-w` - move focus to syntax/runtime error warnings
* Once focus is in the help listing, the up and down arrow keys now
  navigate up and down in the list and the left and right arrow keys
  switch tabs to work through the different help categories.
* When an error occurs, focus is now placed on the error pop-up
  window. Focus can be easily returned back to the editor by pressing
  either the Escape key or `Control-g`.
* Minor improvements for Catalan, Greek, Chinese, Russian,
  French and Romanian translations.


### Bugfixes

* Upgraded the Ruby process library which improves/fixes process
  shutdown behaviour on some Linux systems.
* Fixed a number of memory leaks which resulted in the memory
  requirements of the app increasing over time until it was closed and
  opened again. This was particularly noticeable and potentially
  problematic on machines with small memory footprints.
* General memory usage has been significantly reduced due to the
  introduction of further caching in hot areas of the code that created
  many unnecessary object allocations.
* The Windows installer now works on more Windows 10 systems thanks to
  additional libraries now being included in the installer.


<a name="v3.2.0"></a>

## Version 3.2.0 - 'Tau'
*28th Feb, 2020*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v3.2.0):

After a long 2 years without updates, we are threaded with excitement to
bring you Sonic Pi's first fully independent release. Sam Aaron's work
on this release has been 100% supported by kind and generous donations
from people on [Patreon](https://patreon.com/samaaron) in addition to
funds raised by delivering talks, workshops and performances. Sam's work
has also been hugely assisted by many wonderful code and translation
contributions both by members of the Core Team and by our growing
international community of live coders. This release has truly been a 
community effort.

So, what's new? This time there are no radical new features, instead we
have a much more polished, accessible and stable version than ever before. 
(This release has been heavily battle-tested in many performances worldwide).

This release is codenamed Tau -
[(an improved Pi)](https://tauday.com/tau-manifesto).

One of the core focusses of this release is accessibility. Tau
introduces support for screen readers. Previously the code editor was
invisible to the main screen readers on both macOS and Windows and this
is now fixed. There is also an accessible menu bar and a new high
contrast colour theme which is WCAG 2 level AAA compliant. Accessibility
is very important to us - we acknowledge that this is only the
beginning and we value and encourage feedback from users who have
specific accessibility requirements that we are not directly meeting. We
plan to improve our accessibility support where possible in every release
going forward.

Another aspect of accessibility is support for Windows devices with high
resolution displays. Previously on certain displays font and icon sizes
could be far too small, unreadable and out of proportion with the app in
general. This has all been addressed with significant work on ensuring
Sonic Pi looks great on a variety of monitors on both Windows and
macOS. We believe that this release looks better and is more usable than
any previous release.

Tau features two beautiful new audio visualiser scopes - a spectrum
scope showing the frequency content of the audio and a beautiful mirror
stereo scope. These scopes have been contributed by Chris Maughan who
has also been pivotal in improving and simplying the Windows build
process. We are hugely grateful for his contributions - thanks Chris!

We have two fabulous new FX - a ping pong delay contributed by Ethan
Crawford and an autotuner contributed by Xavier Riley. We hope you have
as much fun playing with these as we have.

Finally, this release has seen a significant improvement in
translation coverage - both of the GUI and the tutorial. We truly
believe that this helps us achieve our mission of lowering the barrier
to entry for a creative experience with code by helping more non-English
speakers start their live coding journeys.

Now, go and make some noise with code!

### Breaking Changes

* `spread` now produces identical patterns as in the 
  [Toussaint paper](http://cgm.cs.mcgill.ca/~godfried/publications/banff.pdf). 
  Previously, some of the patterns had been shifted. Use the `rotate:` opt to
  match prior behaviour if required.
* OSC cues now include the IP address and port number of incoming messages
* MIDI cues now include the source information in the first part of the
  path with `:` separators to match the new OSC cue format.
* The `osc` fn now forces all outgoing args to either be numbers or
  strings (binary blobs and timestamps are not supported at this
  point). If the value is neither a number or string, it is 'inspected'
  and the resulting description string is sent instead.
* The default incoming OSC cues port is now 4560 as the previous value
  was registered on the Iana Service Name and Transport Protocol Port
  Number Registry. However, 4560 is currently free.
* Constrain `:flanger` FX's `feedback:` opt to range 0->1. Values greater than 1 had the effect of increasing the amplitude which should be achieved via the `amp:` opt.
* Add arg checker for `sample`'s `hpf:` opt which now ensures the value is not higher than 119 as values higher than this can cause bad noise artefacts.
* Improve `sample`'s `start:`/`finish:`/`onset:`/`slice:` opt semantics. The `start:` and `finish:` opts for `sample` now work within the specified slice or onset. This allows you to easily play, say the first half of a given slice or onset by specifying a `start:` of `0` and a `finish:` of `0.5`. Where both the `slice:` and `onset:` opts are specified, then the slice operates within the onset. Where the `slice:`, `onset:` and `start:`/`finish:` opts are all used, the start/finish operate within the slice which in turn operates within the onset. See the updated sample examples for more information.



### New Fns

* `eval_file` evaluates the contents of the specified file inline in the
  current thread as if it was a function.
* `midi_sysex` for sending MIDI sysex messages.
* Add new chain fn `.notes` which takes a ring of values and returns a
  new ring with each element treated as a note. This will leave numbers
  unchanged but will turn note names like `:C1` to their corresponding
  MIDI note.


### Synths & FX

* New FX `ping_pong` - delayed echo alternating between left and right channels.
* New FX `autotuner` - Autotune/phase vocoder effect.
* The `gverb` FX now checks to ensure that the `room:` opt is greater than or equal to 1.
* The `piano` synth now supports fractional notes such as `70.3`


### GUI

* New WCAG 2 level AAA compliant high contrast colour theme. This is
  especially suited for those with low vision.
* New application menu and shortcuts.
* New default icons which match the pro icon feel.
* 2 new visualiser scopes - mirror stereo and spectrum.
* Move to simple numbered labels for buffers.
* Improve scaling of icons, text etc. on high DPI displays on Windows.
* The version number is no longer placed in the initial comment of new
  empty buffers. This felt like a friendly thing to do, but can be
  confusing if a given buffer hasn't been used and the version was
  updated. This results in the buffer reporting the old version number
  that was used to create the buffer not the current version used to
  display it.
* Many, many translation improvements. Thanks to all the wonderful
  volunteers contributing to the translation effort: https://hosted.weblate.org/projects/sonic-pi/
* Error pane height is now higher by default on all platforms and its
  height is correctly scaled on high DPI monitors.
* Preference checkboxes are now similarly styled on all platforms.

### Improvements

* Allow scale degrees over an octave and augmented/diminished degrees.
* Calling `.tick` and `.look` on a normal array such as `[1, 2, 3]` will
  now automatically convert it to a ring first.
* Add docstring for the `set` fn.
* Add new article A.20 - Creative Coding in the Classroom as appeared in
  issue #9 of the Hello World magazine.
* OSC messages are now sent from the external cue listening port. This
  allows OSC servers that reply directly back to incoming messages (such
  as SuperCollider) to be able to communicate back to messages sent from
  Sonic Pi.
* Random fns now understand notes. For example `rrand(:e1, :e2)` will
  return a random frequency between E octave 1 and 2.
* Windows can now detect sample onsets from samples stored in `.flac`
  and `.ogg` format in addition to arbitrary sample rates such as 48khz.
  
  
### Bugfixes

* Further improve boot stability on Mac in the cases where audio input/output
  sample rates do not match (typically due to the use of bluetooth
  headsets). Audio inputs on macOS are now disabled by default unless we
  can definitely determine the audio rates are the same. 
* Revert synthdefs to original bytecode version. This fixes a regression
  in at least the `:tb303` synth and possibly others. All synths should
  sound and behave as they did in `v3.0.1`.
* Fix issue with scaling default opts. Previously it was possible that
  unspecified opts correctly fell back to the default value - however
  that default value wasn't scaled. Default values are now always scaled
  whether or not explicitly specified.
* `with_sched_ahead_time` now correctly sets the schedule ahead time
  before running the block and returns the result of the
  block itself
* When "Enforce Timing Guarantees" is selected, Sonic Pi wil no longer
  throw out of time warnings in 'real time' threads (specified using
  `use_real_time`).
* `set` and `cue` no longer print duplicate messages which was possible
  in some cases.
* Fix `time_warp` examples which were incorrect and misleading.
* Improve pid handling on Linux. This should reduce the number of zombie
  processes accidentally generated.
* The `offset:` opt for `with_swing` now matches documented behaviour.
* Teach `sample_duration` to scale correctly.
* Fix GUI bug which could result in a buffer stuck in the highlighted
  state after hitting Run.
* Fix the `on:` opt when playing chords to work similarly across *all*
  notes in a given chord.
* Stop `defonce` from being executed multiple times
* Calling `.take` on an empty ring no longer results in infinite
  recursion but instead returns an empty ring.
* Scope now has a minimum height.
* Windows now supports sending and receiving OSC messages over the
  network to and from external machines.
  


<a name="v3.1.0"></a>

## Version 3.1.0 - 'Sauna'
*23rd Jan, 2018*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v3.1.0):

The major feature of this release is that it brings v3 functionality to
Windows. Windows now supports all the good stuff listed alongside
`v3.0.x` including `live_audio`, `midi`, `osc` and much more. See
`v3.0.1` and `v3.0.0` release notes below for more infomation.

Of course, no new release comes without new features, and there are a
number of additions and improvements on all levels. Firstly, we are
honoured to include a number of wonderful new samples kindly recorded
and released into the public domain by our friends Mehackit. If you
aren't already aware, Mehackit organise and run creative coding
workshops all around Finland and Europe and were a core part behind the
CodeBus Africa project which used Sonic Pi to engage almost 2000 African
students with creative code. Our favourite of these samples is the new
`:ambi_sauna` which has given us the name for this release.

We have added support for Ogg Vorbis encoded audio files `.ogg` and
`.oga`. These are lossy encoders similar to `.MP3` but free from license
restrictions. See: https://en.wikipedia.org/wiki/Vorbis

Another notable addition is the increase in translation coverage across
a number of languages. If you still feel the translation for your
language could be improved, please do consider joining in the
crowd-sourced effort here: https://hosted.weblate.org/projects/sonic-pi/

We have also included some styling tweaks to the GUI - to make
it look even cleaner and also work better when projecting code with
visuals underneath.

Finally, this release also gives me the opportunity to welcome Ethan
Crawford to the Core Team. He has been a prolific contributor to the
project over the years and his keen eye for detail has helped polish
Sonic Pi from the rough stone it once was to the shining gem it is
today.

Have fun and enjoy the new features - especially you lovely Windows
users!


### Breaking Changes

* The synth `chip_noise` now has standard default envelope opts (`sustain:` and `release:` are now 0 and 1 respectively (as opposed to 1 and 0))


### New Fns

* `set_audio_latency!` Apply positive or negative timing offset for audio events to allow for any timing differences between OSC/MIDI events caused by external latencies (such as wireless speakers).
* `midi_pc` for sending MIDI Program Change messages

### Samples

* Add support for Ogg Vorbis encoded audio files `.ogg` and `.oga`. 
* Add new samples (kindly recorded and released into the public domain by Mehackit). 
  - new `glitch_` sample group containing sounds with a glitchy texture.
  - new `mehackit_` sample group with sounds of old toys
  - new samples: `:perc_bell2`, `:perc_door`, `:perc_impact1`, `:perc_impact2`, `:perc_swoosh`, `:ambi_sauna`, `:bd_mehackit`, `:sn_generic`, `:loop_3d_printer`, `:loop_drone_g_97`, `:loop_electric`, `:loop_mehackit1`, `:loop_mehackit2`, `:loop_perc1`, `:loop_perc2`, `:loop_weirdo`, 


### GUI

* Add new in_thread forum to the community listings (and update other entries).
* Many, many translation additions and improvements for a multitude of
  languages. The tutorial is now available in German, Spanish, French,
  Italian, Japanese, Dutch, Polish, Portuguese, Romanian, and Russian.
* GUI now lets the user know when the buffer capacity has been exceeded
  on macOS and Linux (Windows behaviour is currently unchanged). This
  capacity limitation will be addressed more thoroughly in a future release.


### Bugfixes

* Stop the internal sample pattern matcher from duplicating matches in some cases.
* Stop pro icon preference from resetting to 'off' when switching from dark to light mode


<a name="v3.0.1"></a>

## Version 3.0.1 - 'IOIO'
*28th July, 2017*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v3.0.1):

This is a minor release addressing a few bugs and includes a small
number of modifications.

### Breaking Changes
* Raspberry Pi only - based on a request from Raspberry Pi we have
  removed the ability to switch between HDMI and headphone output as
  this is now duplicate functionality to what is now available in
  PIXEL. Now, to change audio output, you need to right-click on the
  audio logo in the menu at the top right of the screen.
  
### GUI
* The IO menu has been slightly tweaked to improve amount of space used.

### Improvements
* The Time State system no longer maintains a history of *all* events -
  instead it retains at least 20 events for each path and beyond that
  culls events 30s in the past.

### Bugfixes
* Fix minor issues with docs (grammar and rendering).
* Incoming OSC now correctly handles OSC paths which contain capital letters.

<a name="v3.0"></a>

## Version 3.0 - 'IO'

*18th July, 2017*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v3.0):


This release is our most ambitious to date. The goal is to open up the
code within Sonic Pi to the outside world. We want you to be able to both
manipulate the real world to change the code and change the real world
with the code. Input Output. IO.

We have therefore focussed on getting events and audio in and out of
Sonic Pi in new ways whilst keeping to our philosophy of simple code,
live manipulation, and strict, powerful timing. This release introduces
a number of brand-new components:

* *Time State* - a powerful new time-based deterministic shared memory system
* *MIDI* - support for input and output of MIDI 
* *OSC* - support for sending and receiving OSC messages on the network.
* *Live Audio* - for getting multiple streams of audio into Sonic Pi
* *Multi-channel audio out* - for outputting multiple streams of audio.
* *Audio Buffers* - for internal recording of audio enabling the creation of loopers.

It's important to mention that MIDI and OSC output functionality is
possible due to a new well-timed scheduler. This was built in a
remarkable programming language called [Erlang](http://erlang.org) which
is now part of the Sonic Pi distribution. We have had the great honour
of one of Erlang's co-creators, Joe Armstrong working directly with us
on the implementation of this scheduler and we look forward immensely to
continuing to work with him on new functionality.

Another very important part of this release is the addition of Robin
Newman to the core team. We have always been consistently impressed with
his fearless and experimental attitude - pushing Sonic Pi into new areas
with ease. We're excited that he's decided to join our core team and
hope you're as excited by his future work with us as we are.

Finally, we're really very excited about what new kinds of instruments
people will be able to create with this new technology both in the
classroom and on stage at musical festivals. Sonic Pi has now become a
fully programmable music studio. Have fun live coding!


### Breaking Changes

* Ring's `.pick` now returns 1 element by default. Previously calling `.pick` on a ring would pick n elements randomly from the ring (including duplicate picks) where n would be the size of the ring. With this change, `.pick` only returns a single element. This makes it similar to choose.


### New Fns

* `midi_*` - many new MIDI-specific fns such as `midi_note_on`, `midi_pitch_bend`, `midi_cc`, `midi_clock_tick`. See new tutorial section for more information. These fns *send* MIDI messages to connected MIDI devices. Incoming MIDI is received via the new event log.
* `with_swing` - add swing to successive calls to do/end block.
* `get` - get a named value from the Time State at the current time. This will return the last value entered. Previous values can be read when within a `time_warp`. Has full support for OSC 
* `set` - set a named value in the Time State at the current time. Future values can be set when within a `time_warp`.
* `use_real_time` - convenience fn for setting the schedule ahead time to 0 for the current thread. Very useful for removing latency from live loops that are working with external cues (such as MIDI or OSC).
* `use_midi_defaults` - set defaults to be used for all subsequent MIDI calls. Similar to `use_synth_defaults`. Also available: `with_midi_defaults`.

* `use_osc` - set the default hostname and port number for subsequent outgoing OSC messages. See also `with_osc`.
* `osc` - send Open Sound Control messages in time with the music to default hostname and port
* `osc_send` - similar to `osc` but requires you to specify the hostname and port
* `use_sched_ahead_time` - set the schedule ahead time specifically for the current thread. Also available - `with_sched_ahead_time`.
* `current_time` - return the current logical time.
* `assert_error` - An assertion to ensure the specified block of code raises the specified error.


### Synths & FX

* New synth `live_audio` - directly stream audio from your soundcard as a synth.

* New FX `record` - enables you to internally record any audio into named buffers. Perfect for building looper systems.

* New FX `:sound_out` - stream out audio to a specific output on your sound card. This enables multi-channel audio out.
* New FX `:sound_out_stereo` - similar to `sound_out` but streams out to a pair of consecutive audio card (left and right) output channels.
* New FX `eq` - Parametric EQ with three centre freqs - low, mid & high - all with Q values and gain (-1 -> 1). Also has low and high shelves with centre freqs and slope adjustment.
* New FX `tremolo` - simple tremolo effect which modulates the volume within the `do/end` block.


### GUI

* New 'pro' icon set for performances.
* New GUI translations for the following languages: (BS) Bosnian, (CA) Catalan, (CS) Czech, (DA) Danish, (EL) Greek, (ET) Estonian, (HI) Hindi, (ID) Indonesian, (KO) Korean, (PT) Portuguese, (TR) Turkish, (ZH) Chinese
* Added new pane for displaying new cue events (including incoming OSC and MIDI)
* Added new IO preferences tab for configuring MIDI and network settings.
* Automatically autocomplete `sync`, `cue` and `get` or `set`
* Increase width of autocompletion popup.


### Documentation & Examples

* New articles on additive and subtractive synthesis techniques.
* New example piece 'Cloud Beat' by Pit Noack


### Improvements

* Teach `time_warp` about input ranges. It now works similar to `at` in that it can now take two lists of args - times and values - which represent a list of time destinations to be visited in turn.
* Ensure any unprinted messages are displayed if an exception occurs.
* Teach `range` to work as expected with both floats and ints.
* Teach rings a new chain method - `.scale` which will return a new ring with all elements multiplied by the scale value.
* The fn `control` now returns the node you're controlling.
* Add many new chords


### Bugfixes

* Fix randomisation aspects of `:slicer`, `:wobble` and `:panslicer` FX (i.e. via the `probability:` opt).
* Fix file path drag and drop on Windows to not accidentally prefix path with /.
* Teach `chord_invert` and `sample` to work with floating point args.

<a name="v2.11.1"></a>

## Version 2.11.1 - 'Hack'

*16th Dec, 2016*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.11.1)

This is primarily a maintainance release containing a number of bug
fixes and minor tweaks. The feature set remains unchanged from `v2.11`.

One of the core bugs fixed in this release is an issue with the return
key within some Japanese layouts. Another important change is the
unification of the font on all platforms to
[Hack](http://sourcefoundry.org/hack/). This is from the same family as
Menlo and Bitstream Vera (the previous fonts for Mac and Linux
respectively) and a dramatic improvement to the Courier font previously
used for Windows.


### GUI

* Move all platforms to the same font -
  [Hack](http://sourcefoundry.org/hack/).
* Improve initial log information and messages.
* The scope axes are no longer shown by default for a cleaner look and
  feel.


### Docs

* Add new MagPi article on sample stretching.


### Bugfixes

* Ensure `chord_invert`'s shift value is a whole number - otherwise it
  is possible to get into an infinite recursion.
* Ensure thread locals have correct default values on `clear`. When a
  run is created, the thread is given a set of default thread
  locals. Previously `clear` removed *all* thread locals. Now, we reset
  the thread locals to the defaults for a brand new thread.
* Fix line reported on syntax error.
* Fix unknown synth error message.
* `sync` can once again handle multiple cues correctly.
* Fix boot issues for users that don't have a standard /etc/hosts file
  that contains an entry for localhost.
* `sample_duration` now handles onsets, slices and sustain.
* Fix issue with return key not being recognised with Japanese
  keyboards.
* Fix sporadic flickering of current line on Raspberry Pi.
* Fix errors in buffer 0 being reported as being from buffer 3.
* Fix scrollbar background colour on Windows in dark mode.
* Improve error message reported when required ports are not available
  at boot.
* Fix issue with calling `control` on a chord group.  
* Fix `rand` and `rand_i` to honour their arguments.

<a name="v2.11"></a>

## Version 2.11 - 'Time Warp'

*3rd Nov, 2016*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.11.0)

This release is the biggest and most adventurous release yet. There are
as many (invisible) modifications and improvements to the internal
systems as there are new external features that you can see and play
with. The aim was to create a solid foundation for new and exciting
features both in this release and in preparation for future releases.

We also open our arms to welcome two new Core Team members - Luis Lloret
and Adrian Cheater. Both have made generous and substantial
contributions to this release. Thank-you. Sadly we also say farewell to
Jeremy Weatherford. Please extend your kind thoughts and gratitude to
Jeremy for all of his contributions - in particular for turning the
Windows release from a possibility into a reality. Luckily Luis has
kindly stepped in to maintain the Windows installer.
 
The main visible feature is the new scope visualisers. The overall audio
output can now be visually monitored by one of three wave form
visualisers. Firstly there is the individual left and right channels,
next is a single mono scope which is mixed down from the stereo channels
using RMS and finally there is a Lissajous scope which displays phase
differences between the left and right channels. Typically the mono
output will be most useful. Use the preferences pane to hide and show
each of these scopes. All of them may be viewed at the same time if
necessary. Thanks to Adrian Cheater for the core work behind this
feature.

We now have support for multi-channel input (up to 16 channels) via the
new `sound_in*` synths for systems that have audio in. This opens up the
possibility to use Sonic Pi as an FX unit for vocals, guitars and any
other audio source.

Another exciting new feature is the sample opt `onset:` - which lets you
play a specific percussive part of a sample. This uses an automatic
onset detection algorithm to determine all the points in the sample that
go from quiet to loud quickly such as a drum, synth or bass hit. For
example, this allows you to take a complex drum sample and trigger each
of the individual drums in your own order and to your own timing.

Finally, translations are now crowd-sourced and small or large
contributions for any language can be made here:
`https://hosted.weblate.org/projects/sonic-pi/`. If your language isn't
yet available or you would like to improve things, please join in the
effort. Thanks to Hanno Zulla for making this possible.


### Breaking Changes
 
* `sample` now supports the opt `path:` which enables the sample's path
  to be overridden.
* `use_sample_pack` is now deprecated and no longer available. Consider
  using the new filter system. See documentation for `sample` for more
  details.
* `current_sample_pack` is now deprecated and no longer available.
* `inspect` has been removed. (Standard printing now calls
  `Object#inspect` by default)
* `load_sample` now only loads the first matched sample. `load_samples`
  now loads all matched samples.
* Remove SuperCollider server automatic reboot system as it was badly
  conflicting with machines that went into a 'sleep state' (for example,
  when a laptop is closed). The fn `reboot` is still supported and may
  still be triggered manually if required.
* Calls to `play`, `synth` and `sample` now consume all their arguments
  before testing to see if the synth should be triggered. This ensures
  all declared rands are consumed. This change might therefore
  potentially modify your random stream consumption. Consider using
  `rand_back` or `rand_skip` to re-align the stream if necessary.
* New threads now start with a fresh set of tick counters and a new
  random stream.
* It is no longer possible to use lambdas as values for synth
  defaults. This is because synth defaults are shared across thread
  boundaries and there is now a new safety system that only allows
  immutable/serialisable values to be used. Unfortunately Ruby has no
  notion of a 'pure' function and each lambda captures over its
  environment and therefore may contain free variables pointing to
  mutable data. A replacement system for describing a simple set of pure
  functions is being designed.


### New Fns

* `reset` - resets the user's thread locals (such as ticks and rand
  stream index) to the snapshot of the state as recorded at the start of
  the current thread.
* `clear` - clears all the user's thread locals to a blank state.
* `time_warp`- allows whole blocks of code to be shifted forward or
  backwards in time up to the value of `current_sched_ahead_time`.
* `rand_look` - generate a random number without consuming a rand by
  looking ahead in the random stream.
* `rand_i_look` - generate a random integer without consuming a rand by
  looking ahead in the random stream.
* `run_file` - Runs the contents of file at path as if it was in the
  current buffer.
* `run_code` - Runs the contents of the specified string as if it was in
  the current buffer.
* `Numeric#clamp` - max and minimum bound (will clamp self to a value <=
  other and >= -1*other
* `set_recording_bit_depth!` - set the bit depth for WAV files generated
  by recording the audio. Default is 16 bits, and can now be set to one
  of 8, 16, 24 and 32. Larger bit depths will result in better quality
  audio files but also much larger file sizes for the same duration.
* `scsynth_info` - obtain information about the running audio synthesis
  server SuperCollider such as the number of available busses and
  buffers.


### Synths & FX

* New synth `:tech_saws` - an implementation of a slightly modified
  supersaw.
* New synth `:sound_in` - reads audio from the sound card.
* New synth `:sound_in_stereo` - reads audio from the sound card.
* All FX now have a `pre_mix:` opt. This allows the audio flow to
  completely bypass a given FX (unlike `mix:` which passes the audio
  through the FX but modifies the amplitude afterwards).
* Teach `control` to manipulate the last triggered synth by default. For
  example, `control amp: 3` will set the `amp:` opt of the last
  triggered synth to 3. However, `control foo, amp: 3` will still
  specifically control synth `foo`.


### Samples

* New opt `slice:` - lets you play a specific slice of a sample. The
  default number of slices is 16 which may be changed with the
  `num_slices:` opt. Sample is divided equally into the number of slices
  without regard for audio content and onset points.  The `slice:` opt
  also works with `pick` for triggering random sample slices: `sample
  :loop_amen, slice: pick`.
* New opt `onset:` - lets you play a specific percussive part of a
  sample. Uses automatic onset detection to determine the points in the
  sample that go from quiet to loud quickly. Unlike `slice:`, `onset:`
  does not necessarily divide a sample into equal onsets - some onsets
  will be smaller or bigger than others and the number of onsets is
  determined by the algorithm and isn't known in advance.


### GUI

* New scope visualisers.
* Allow files to be dragged from the OS into the text area. This inserts
  the file/folder name as a string.
* GUI now remembers the last directory you saved or opened a file
  to/from as the default location for next time.
* Swap align button for a scope button. Given that alignment now happens
  automatically, a specific button seems somewhat redundant. Instead we
  now have a button for toggling the visibility of the scope(s).
* Loading multiple samples simultaneously is now much faster.
* Preferences have been slightly re-organised.
* Preferences now has a Master volume slider which controls Sonic Pi's
  audio amplitude independently from the system volume.
* All buttons now display status message + shortcut where available.
* Enable app transparency slider for Windows.
* Dark and light theme colours have been slightly polished and unified
  to use the same logic.
* On multi-screen systems, fullscreen mode now defaults to the app's
  current screen.


### Documentation

* Translations are now crowd-sourced. See:
  `https://hosted.weblate.org/projects/sonic-pi/`
* Improve docstring for `live_loop`.
* Add 3 new MagPi articles on amplitude modulation, performance and practice techniques.
* Add missing `pulse_width:` opt to flanger FX doc.


### Improvements

* Improve log messages written to `~/.sonic-pi/log`
* Improve booting on Mac in the case that the audio card's rate can't be
  determined.
* Massively improve boot stability on Windows.
* Improve error message for `play_chord` when notes isn't list like.
* The number of samples that may be loaded at any one time has been
  increased from 1000 to 4000. However, memory limitations still apply
  (4000 1MB samples will require 4000MB of free system memory)
* `pick` now returns a lambda if no list is given as the first argument
  (which makes it useful for using with sample's `onset:` and `slice:`
  opts.
* Audio server is now paused when app is not in use - reducing CPU load
  and battery consumption.
* Error messages now report names matching the editor tabs such as `buffer 0`.


### Bugfixes

* Decrease duration of `:loop_tabla` so that it correctly loops. (Length
  reduced to 10.674 seconds).
* Enforce UTF-8 encoding of all incoming text.
* Fix `:reverb` FX's `mix:` opt to ensure it's in the range 0 to 1.
* `sample nil` now no longer plays a sample - it was incorrectly
  defaulting to the first built-in sample (`:ambi_choir`)
* `pick`'s `skip:` opt now works as expected: `pick(5).drop(1) == pick(5, skip: 1)`
* `sample` now prints a 'no sample found' message with both `sample nil` and `sample []` rather than incorrectly playing the first built-in wav file.
* Limit `:piano` synth to notes less than 231 as higher values crash the audio server.


<a name="v2.10"></a>

## Version 2.10 - 'Cowbell'

*Friday 15th April, 2016*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.10.0)

_"I gotta have more cowbell!"_ - The Bruce Dickinson

The main feature in this release is the completely new sample system for
working with large folders of samples. You can now easily index into and
filter over whole folders of samples with a simple yet powerful filter
system. This system has been in development for over a year now and has
been tuned and tweaked over many performances and practice sessions. See
the documentation and examples for `sample` as well as the new tutorial
section 3.7 for details.

We also have a number of exciting new sounds to play with including some
beautifully sounding chiptune synths, fun retro FX and new drum samples
including a full tabla set and a cowbell.

Finally, even more boot issues on both OS X and Windows have been
fixed, making this the most polished and stable release to date.

Now go and get your live code on!


### Breaking Changes

* `use_sample_pack_as` and `with_sample_pack_as` have been removed.
* A synth opt value of `nil` now no longer resolves to 0.0. Instead it
  masks any defaults and ensures the downstream default (for the actual
  synthdef itself) is honoured. This allows you to override any existing
  synth defaults specified via `use_synth_defaults` for a given synth
  trigger.
* Default schedule ahead time is now 0.5s on all platforms for
  consistency (except for Raspberry Pi 1 which remains at 1s).


### New Fns

* `current_random_seed` - Get the current seed value of the random generator.
* `set_cent_tuning!` - global tuning.
* `on` - similar to `if` but behaves the same as the `on:` opt for
  synths and samples.
* `halves` - create a ring of successive halves.
* `doubles` - create a ring of successive doubles.
* `pick` - similar to shuffle but allows duplicates. You may also
  specify how many items to pick.
* `fx_names` - create a ring of all available FX.


### Synths & FX

* New synth `:dtri` - detuned triangle waves.
* New synth `:pluck` - a lovely synthesised plucked string.
* New synth `:chiplead` - retro NES style lead synth.
* New synth `:chipbass` - retro NES style bass synth.
* New synth `:chipnoise` - retro NES style noise synth.
* New FX `:whammy` - low-tech transposition effect similar to the
  Digitech Whammy guitar pedal.
* New FX `:octaver` - low-tech octave effect similar to early guitar
  pedals.
* New FX `:vowel` - modifies incoming signal to match human vowel
  sounds.
* New FX `:mono` - mono effect for summing stereo channels.
* `:tanh` FX is now more crunchy by default.
* `:compressor` and `:krush` FX now correctly honour the `mix:` opt.


### Samples

* Samples in FLAC format (Free Lossless Audio Codec) are now supported!
* The `sample` fn gained source and filter pre-opts. You may now specify
  a number of parameters directly before the opts which describe both
  folders containing samples and filters over those folders to allow you
  to easily and quickly work with large sample sets. See Section 3.7 of
  the tutorial for more information.
* Samplers now have `hpf:` and `lpf:` opts. Any `cutoff:` opts are
  automatically switched to `lpf:` and any errors will be reported with
  an `lpf:` prefix.
* The sampler synth gained a compressor enabled via the `compress:`
  opt. This means you can now compress any triggered sample directly
  without the need to reach for an FX.
* Samplers gained the `pre_amp:` opt which applies the amp at the
  beginning of its internal signal chain. You can use this to overload
  the new compressor.
* Samplers now have both high pass and low pass filter envelopes which
  behave similarly to the amplitude envelope but control internal hpf
  and lpf FX. These are available via new opts which mirror the standard
  envelope opts but with a `hpf_` and `lpf_` prefix (such as
  `hpf_attack`, `lpf_decay_level` and friends).
* Passing a directory path to `load_samples` will now load all the
  samples within that directory.
* Passing a directory path to `free_samples` will now free all the
  loaded samples within that directory.
* Samples are now loaded asynchronously in a separate thread. This
  therefore doesn't block the current thread and cause any subsequent
  synth/sample triggers to be late.
* Sample trigger logging now includes the directory of the contained
  sample.
* Samples are now reloaded asynchronously after a server reboot (and
  therefore no longer block execution).
* Add new `:tabla_` sample group with a range of tabla drum sounds.
* Add new `:vinyl_` sample group with a range of vinyl scratches and
  hisses.
* Add new samples: `:drum_cowbell`, `:drum_roll`, `:misc_crow`,
  `:misc_cineboom`, `:perc_swash`, `:perc_till`, `:loop_safari`,
  `:loop_tabla`.


### GUI

* Add new preference to enforce strict synth/FX timing. When enabled
  synths and samples no longer trigger if it is too late - instead they
  are silent and print a warning message. This behaviour ensures samples
  or synths are never triggered out of time.
* New load button which will load the contents of a file into the
  current buffer.
* The vertical bars which help visualise nesting now render in a
  contiguous fashion over blank lines.
* `C-k` now nukes over trailing whitespace.
* `load_sample` now has sample autocompletion.
* GUI now correctly reports if the host is a Raspberry Pi 3.
* New editor preference - Log Auto Scroll. When enabled will always
  scroll log to the bottom after printing every new message.
* Whitespace at top and bottom of buffer is no longer trimmed.
* Hitting `RETURN` now auto-indents the buffer - ensuring that the
  cursor is moved to the correct indentation on the following line.
* Added Chinese Simplified GUI translation.
* Log visibility now correctly matches GUI preference.


### Documentation

* New tutorial section 3.7 on Sample Pack Filters.
* New appendix sections.
* Examples for `sample` fn have been completely rewritten and extended.


### Improvements

* `scale` and `chord` can now handle being passed no tonic such as:
  `(chord :minor)` which will return a ring of offsets from 0.
* `chord` learned new variants: `add2`, `add4`, `add9`, `add11`,
  `add13`, `madd2`, `madd4`, `madd9`, `madd11`, `madd13`
* The ring's `.take` method can now take more elements than the original
  ring by wrapping around: 
  `(ring 1, 2, 3).take(5) #=> (ring 1, 2, 3, 1, 2)`
* Rings may now be added or subtracted from each other e.g. 
  `(ring 1, 2,  3) + (ring 4) #=> (ring 1, 2, 3, 4)`
* Adding or subtracting a number from a ring will create a new ring with
  the number added or subtracted from each of the original ring's
  elements: `(ring 1, 2, 3) - 1 #=> (ring 0.0, 1.0, 2.0)`
* Calling `(ring 1, 2, 3).take(0)` will now return an empty ring.
* `density` now complains if it isn't passed a do/end block.
* Improve error messages for `use/with_synth` when accidentally passed opts.


### Bugfixes

* On OS X only raise an error on boot if it's clear the sound card's in
  and out rates are different.
* Improve robustness of boot process on Windows.
* Rest notes are no longer printed if synth logging is disabled.
* No longer apply synth defaults to FX.
* You may now control opts that have no associated info (previously it
  would raise a 'not modulatable' error).
* Fix index lookup of Vectors.
* Fix `C-i` doc lookup shortcut to work with `:tb303` synth docs.
* `C-i` now always displays docs where available (previously it was
  possible for docs not to be displayed).
* Sliding between chords now works correctly  
* Windows version will now boot on mutiple networked machines logged in
  with the same account.


<a name="v2.9"></a>

## Version 2.9 - 'Venster'

*Thursday 31st December, 2015*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.9.0)

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
* Add Spanish translation of tutorial.
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
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.8.0)

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
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.7.0)

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
* Errors now no longer print out unusual print version of context object
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
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.6.0)


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
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.5.0)

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
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.4.0)

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
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.3.0)


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
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.2.0)

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


<a name="v2.1.1"></a>

## Version 2.1.1 - 'Firewall'
*Tuesday 25th November, 2014*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.1.1)

* Windows version no longer needs special firewall exceptions to run
* Added license information to info window
* Minor grammar and spelling tweaks to tutorial


<a name="v2.1"></a>

## Version 2.1 - 'Core'
*Friday 21st November, 2014*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.1.0)

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


<a name="v2.0.1"></a>

## Version 2.0.1

*Tuesday 9th September, 2014*
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.0.1)

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
[(view commits)](https://github.com/sonic-pi-net/sonic-pi/commits/v2.0.0)

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
