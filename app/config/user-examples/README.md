# Sonic Pi Config Files

The files in this directory are configuration files for modifying the
behaviour of Sonic Pi. Have fun and happy live coding!


## init.rb

This is a Sonic Pi code file which will be executed at boot time. This
lets you put in any code or function definitions which you'd like to be
able to use in every Sonic Pi session.


## audio-settings.toml

This is a toml file which lets you configure the behaviour of the
SuperCollider scsynth audio server (which generates all audio). You can
use this file to change audio cards, set buffer sizes and sample rates,
etc.


## colour-theme.properties

This is a properties file which lets you override colour themes for the
GUI. This is currently very experimental and is likely to significantly
change.


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



