## Sonic Pi

This is the source for the Sonic Pi music programming environment. The dependencies for this are:

* supercollider
* ruby1.9.1
* libqscintilla2-8

You will need to compile the Qt app within `app/gui` and run one of the `start-group-*` scripts to start the app.

The current implementation assumes the execution context is a Raspberry Pi. Patches for other platforms will be happily considered.
