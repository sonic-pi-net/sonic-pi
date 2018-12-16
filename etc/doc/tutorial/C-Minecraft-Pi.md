C Minecraft Pi

# Minecraft Pi

Sonic Pi now supports a simple API for interacting with Minecraft Pi -
the special edition of Minecraft which is installed by default on the
Raspberry Pi's Raspbian Linux-based operating system.

## No need to import libraries

The Minecraft Pi integration has been designed to be insanely easy to
use. All you need to do is to launch Minecraft Pi and create a
world. You're then free to use the `mc_*` fns just like you might use
`play` and `synth`. There's no need to import anything or install any
libraries - it's all ready to go and works out of the box.

## Automatic Connection

The Minecraft Pi API takes care of managing your connection to the
Minecraft Pi application. This means you don't need to worry about a
thing. If you try and use the Minecraft Pi API when Minecraft Pi isn't
open, Sonic Pi will politely tell you. Similarly, if you close Minecraft
Pi whilst you're still running a `live_loop` that uses the API, the live
loop will stop and politely tell you that it can't connect. To
reconnect, just launch Minecraft Pi again and Sonic Pi will
automatically detect and re-create the connection for you.

## Designed to be Live Coded

The Minecraft Pi API has been designed to work seamlessly within
`live_loop`s. This means it's possible to synchronise modifications in
your Minecraft Pi worlds with modifications in your Sonic Pi
sounds. Instant Minecraft-based music videos! Note however that
Minecraft Pi is alpha software and is known to be slightly buggy. If you
encounter any problems simply restart Minecraft Pi and carry on as
before. Sonic Pi's automatic connection functionality will take care of
things for you.

## Requires a Raspberry Pi 2.0

It is highly recommended that you use a Raspberry Pi 2 if you wish to
run both Sonic Pi and Minecraft at the same time - especially if you
want to use Sonic Pi's sound capabilities.

## API Support

At this stage, Sonic Pi supports basic block and player manipulations
which are detailed in Section C.1. Support for event callbacks
triggered by player interactions in the world is planned for a future
release.
