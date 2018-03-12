11 MIDI

# MIDI

Once you've mastered converting code to music, you might wonder - what's
next? Sometimes the constraints of working purely within Sonic Pi's
syntax and sound system can be exciting and put you into a new creative
position. However, sometimes it is essential to break out of the code
into the real world. We want two extra things:

1. To be able to convert actions in the real world into Sonic Pi events to code with
2. To be able to use Sonic Pi's strong timing model and semantics to control and manipulate objects in the real world

Luckily there's a protocol that's been around since the 80s that enables
exactly this kind of interaction - MIDI. There's an incredible number of
external devices including keyboards, controllers, sequencers, and pro
audio software that all support MIDI. We can use MIDI to receive data
and also use it to send data.

Sonic Pi provides full support for the MIDI protocol enabling you to
connect your live code to the real world. Let's explore it further...
