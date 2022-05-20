## Use of Erlang in Sonic Pi.

*Copied from Sam Aarons post in in-thread.sonic-pi.net about the usage of
Erlang in Sonic Pi.*

Sonic Pi works by scheduling all events ahead of time.

Prior to v3, the only events that needed to be scheduled were audio events.
This was made possible because SuperCollider, the synth engine that Sonic Pi
uses, is able to receive timestamped messages ahead of time, and schedule them
internally to execute at exactly the right time. This is made possible via OSC
bundles. For a (probably way too detailed) treaty of this take a look at this
paper: https://www.cs.kent.ac.uk/people/staff/dao7/publ/farm14-aaron.pdf

With the release of v3, Sonic Pi gained the ability to talk OSC and MIDI. It’s
clearly important that these OSC and MIDI events do not happen with the code
executes in Sonic Pi as this would be before the audio is heard. This is because
both the audio and the MIDI/OSC events will be created ahead of time, whilst the
audio event will then be accurately scheduled to happen a little later and the
MIDI/OSC events will happen immediately.

It was therefore essential to schedule MIDI/OSC events in a similar fashion to
the audio events. Unfortunately SuperCollider’s scheduler only manages its own
internal events, so it was clear that we had to build our own scheduler which
worked in a similar fashion.

Unfortunately an implementation in Ruby would be subject to potential timing
issues as Ruby has a global garbage collector which can execute at arbitrary
times. I therefore wanted to use a language built for reliably low latency and
high concurrency (lots of messages coming in independently wanting to be
scheduled independently). Erlang is a perfect fit for this.

Luckily we had Joe Armstrong (one of the co-creators of Erlang) contribute a
first iteration of a scheduler which is what you see with osc.erl and
pi_server.erl. This scheduler is able to take any OSC message and delay it for a
specific amount of time before forwarding it on to a different host/port with
~1ms accuracy (more could be achieved at the cost of CPU).

The next step was to implement all external non-audio comms as OSC messages.
Clearly this is trivial for OSC messages, but for MIDI we implemented an
OSC -> MIDI bridge (thanks to the good work of Luis Lloret) which would convert
OSC messages directly to MIDI as quickly as possible (this is implemented in
efficient C++).

To bring it all together, the audio events sent to SuperCollider are as before,
they just go directly to Supercollider from the Sonic Pi server. All other
events are first encoded as OSC, sent to Erlang which schedules it to trigger
in time with SuperCollider then we have tools to convert OSC to MIDI.

I can totally imagine other similar tools converting OSC to other useful formats
which will also allow us to take advantage of the Erlang scheduler in the future.

*End of post copy*

### Bundle commands
```
["/send_after", Host, Port | Cmd]
["/send_after_tagged", Tag, Host, Port | Cmd]
```
Both commands send the OSC message <Cmd> to <Host,Port> at the time in the
bundle header

### Immediate Commands
```
["/flush", <Tag>]
```

### Tagged send_after's

A Tag can be associated with a send-after command.
If no tag is explicitly named the tag called "default" is assumed.<br/>
`["/flush", Tag]` cancels all send-after commands which have not yet been
issued.

#### Examples:

`["/flush", "default"]`<br/>
cancels all send-after requests that were scheduled with a
`["/send_after", Host, Port, ...]` bundle.

`["/flush", "drums"]`<br/>
cancels all send-after request that were scheduled with a
`["/send_after_tagged,"drums", Host, Port, ...]` bundle

## Notes about time alignment from clients

The erlang system time is trying to align with the OS system time but can have
minor offsets. This should not be a big problem on the same machine. A client
can check time offset to compensate for the offset though.
To calibrate time offset between the pi_server and a client, the client
can send an OSC message with a delay to the pi_server that points back to
itself and measure the difference between the expected delay and the
real delay. The OSC bundle sent from the client can embed any return
command of choice that the client prefer use for this purpose.
The test3/0 function in the osc.erl module will give you a some input on how to
proceed.

## Information for developers

*  [Erlang](http://www.erlang.org/)
*  [Time and Time Correction in Erlang](http://erlang.org/doc/apps/erts/time_correction.html)
*  [Learn You Some Erlang for great good!](https://learnyousomeerlang.com/)
