# Sonic Pi Config Files

The files in this directory are configuration files for modifying the
behaviour of Sonic Pi. Have fun and happy live coding!

On your system, these files are located in:
- Linux, macOS: `~/.sonic-pi/config` (note that `~` means your home directory)
- Windows: `%USERPROFILE%\.sonic-pi\config`, which is usually
  `C:\Users\<your-username>\.sonic-pi\config`

If the `SONIC_PI_HOME` environment variable is set, it takes the place of
your home directory on every platform, so the config files live in
`$SONIC_PI_HOME/.sonic-pi/config` instead. That is useful if your home
directory isn't writable, or you want to keep your setup on a USB stick.


## init.rb

This is a Sonic Pi code file which will be executed at boot time. This
lets you put in any code or function definitions which you'd like to be
able to use in every Sonic Pi session.


## v5-audio-settings.toml

This is a toml file which lets you configure the behaviour of the
SuperCollider scsynth audio server (which generates all audio). You can
use this file to change audio cards, set buffer sizes and sample rates,
etc.


## v5-colour-theme.properties

This is a properties file which lets you override colour themes for the
GUI. This is currently very experimental and is likely to significantly
change.


## v5-gui-settings.ini

Unlike the files above, this one isn't an example that Sonic Pi copies in
for you — Sonic Pi writes it itself, so it only appears once you've run
Sonic Pi at least once. It remembers how you left the GUI: window position
and size, which panes were showing, your preferences, and the text sizes
you picked with the A-/A+ controls.

There's normally no need to edit this by hand, and it's best not to while
Sonic Pi is running: your current settings are written out when you quit,
which would overwrite anything you'd changed. To reset the GUI back to its
defaults, quit Sonic Pi and delete this file — you'll be met by the
welcome screen again next time you start.


## Questions and Issues

If you have any issues, then please report suspected bugs to GitHub
issues: https://github.com/sonic-pi-net/sonic-pi/issues or ask any
questions you might have on the community forums:
https://in-thread.sonic-pi.net

Note that if you modify any of these files incorrectly, i.e. with the
wrong syntax or in a way that Sonic Pi isn't expecting it may cause
booting issues.

If you can't boot Sonic Pi and have modified any of the config files,
you might want to try removing the config files (Sonic Pi will
automatically replace them with new empty ones) and try booting again.



