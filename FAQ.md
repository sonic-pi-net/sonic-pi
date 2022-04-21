# FAQ - Frequently Asked Questions

## Doesn't work? Where to look for clues:

If you have trouble with Sonic Pi, have a look at the logs. You will
find the log files in your user home directory in `~/.sonic-pi/log/`.
Most likely, you will find a helpful error message there.

## Windows: Server boot error - Can't open UDP port 4558

This problem was reported on Windows machines in Sonic Pi v. 2.7. It was reported in following issues: [#699](https://github.com/samaaron/sonic-pi/issues/699), [#702](https://github.com/samaaron/sonic-pi/issues/702), [#720](https://github.com/samaaron/sonic-pi/issues/720). Occurred on Windows 7, Windows 8.1 & Windows 10.

*Workaround solution:*

* be sure, that you have closed Sonic Pi
* open Windows Task Manager with `Ctrl + Shift + Esc` 
* try to kill all processes listed below if they're still running

Ruby Interpreter (CUI)  
scsynth.exe

You can also check in Task Manager that there is no application entry sonic-pi.exe  
and quit it if there is.

## Linux: There is no sound with use_synth :piano

You probably didn't install the SC3 plugins
package for SuperCollider on your system. If there
is no binary package provided by your distribution, you can
[install it from source](https://github.com/samaaron/sonic-pi/blob/main/INSTALL-LINUX.md#supercollider-sc3-plugins).

## Linux: Trouble with JACK

If the GUI complains that _"The Sonic Pi server could not be started!"_
or the message _"Loading previous buffer contents. Please wait..."_
does not disappear from your buffers after startup, the most likely
reason is a problem with JACK.

The Linux version of Sonic Pi relies on the
[JACK audio connection kit](http://jackaudio.github.io/) for its audio
output and it needs to be installed with it. The `jackd` audio daemon
needs to be started in the background and if it isn't running already,
the Sonic Pi server will start it for you. If this fails, Sonic Pi never
finishes starting up and won't get ready for you.

As a workaround, try this:

* be sure that you have closed Sonic Pi
* start jackd manually using the `qjackctl` GUI
* once it started, start `sonic-pi` again

Sonic Pi contributor @rbnpi has described a [working configuration to
workaround problems starting JACK](https://github.com/samaaron/sonic-pi/issues/827#issuecomment-160928821).

On Linux distributions using PulseAudio, starting JACK may interfere
with the applications using sound that were running already, e.g.
Firefox will stop playing audio once JACK starts. There is a guide on
[how to configure JACK with PulseAudio](https://github.com/jackaudio/jackaudio.github.com/wiki/WalkThrough_User_PulseOnJack)
to avoid this.

----
