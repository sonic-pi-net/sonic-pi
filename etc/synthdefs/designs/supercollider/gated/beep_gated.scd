//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

// sonic-pi-beep_gated - gated variant of the beep synth. See beep.scd for
// notes on the Overtone-replica helpers and how to verify equivalence.
//
// To compile, evaluate this file in SuperCollider (or run it with sclang).
// The synthdef is written to etc/synthdefs/compiled/gated/.
(
var compiledDir = (thisProcess.nowExecutingPath.dirname +/+ "../../../compiled/gated").standardizePath;

var envGenKr = { |env, gate = 1, doneAction = 0|
	EnvGen.kr(env, gate, 1, 0, 1, doneAction);
};

var varlag = { |val, time, curvature, warp|
	var trig = Impulse.kr(0) + (HPZ1.kr(val).abs > 0) + (HPZ1.kr(time).abs > 0);
	envGenKr.([val, 1, -99, -99, val, time, warp, curvature], trig);
};

// Overtone's gated-shaped-adsr: as shaped-adsr, but held open at the
// sustain level (release node 3) until the gate closes.
var gatedShapedAdsr = { |attack, decay, sustain, release, attack_level, decay_level, sustain_level, env_curve|
	[0, 4, 3, -99,
		attack_level, attack, env_curve, 0,
		decay_level, decay, env_curve, 0,
		sustain_level, sustain, env_curve, 0,
		0, release, env_curve, 0];
};

SynthDef('sonic-pi-beep_gated', {|
	note = 52, note_slide = 0, note_slide_shape = 1, note_slide_curve = 0,
	amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
	pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
	attack = 0, decay = 0, sustain = 0, release = 1,
	attack_level = 1, decay_level = -1, sustain_level = 1,
	env_curve = 1,
	phase_offset = 0,
	gate = 1,
	out_bus = 0|

	var freq, snd, env;

	decay_level = Select.kr(decay_level |==| -1, [decay_level, sustain_level]);
	note = varlag.(note, note_slide, note_slide_curve, note_slide_shape);
	amp = varlag.(amp, amp_slide, amp_slide_curve, amp_slide_shape);
	pan = varlag.(pan, pan_slide, pan_slide_curve, pan_slide_shape);
	freq = note.midicps;
	// phase_offset is expressed as a fraction of a cycle (0..1), SinOsc wants radians
	snd = SinOsc.ar(freq, phase_offset * 2pi);
	env = envGenKr.(gatedShapedAdsr.(attack, decay, sustain, release, attack_level, decay_level, sustain_level, env_curve), gate, 2);
	Out.ar(out_bus, Pan2.ar(env * snd, pan, amp));
}).writeDefFile(compiledDir);
)
