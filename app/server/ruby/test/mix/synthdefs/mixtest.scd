// Harness-only synthdefs for the mix measurement tests. Not shipped: these
// exist so a test can play an exact signal into the real mixer and capture
// the real output, with nothing else in the path.
//
// Build with ./build.sh (needs SuperCollider's sclang).
(
// Plays a stereo test signal into the mixer's input bus. doneAction 2 frees
// the node at the end of the buffer so the render length is the signal length.
SynthDef('mixtest-source', {|buf = 0, out_bus = 16, amp = 1|
  Out.ar(out_bus, PlayBuf.ar(2, buf, 1, 1, 0, 0, 2) * amp);
}).writeDefFile("compiled".resolveRelative);

// Records the mixer's output bus. Runs at the tail so it sees the mixer's
// ReplaceOut for the current block. run = 1, loop = 0: record once, stop at
// the end of the buffer rather than wrapping over the start.
SynthDef('mixtest-capture', {|buf = 0, in_bus = 0|
  RecordBuf.ar(In.ar(in_bus, 2), buf, 0, 1.0, 0.0, 1.0, 0.0, 1.0, 0);
}).writeDefFile("compiled".resolveRelative);
)
