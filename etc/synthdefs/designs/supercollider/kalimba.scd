// Adapted from http://sccode.org/1-51n
(
SynthDef('sonic-pi-kalimba', {|
	note = 52, note_slide = 0, note_slide_shape = 1, note_slide_curve = 0,
	amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
	pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
	attack = 0, decay = 0, sustain = 4, release = 1,
	attack_level = 1, decay_level = -1, sustain_level = 1,
	clickiness = 0.1,
	out_bus = 0|

	var freq, snd, click, env;


	note = note.midicps;
	note = note.varlag(note_slide, note_slide_curve, note_slide_shape);
	decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
	amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
	pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

	freq = note;

	// Basic tone is a SinOsc
	snd = SinOsc.ar(freq) * EnvGen.ar(Env.perc(0.03, Rand(3.0, 4.0), 1, -7), doneAction: 2);
	snd = HPF.ar( LPF.ar(snd, 380), 120);
	// The "clicking" sounds are modeled with a bank of resonators excited by enveloped white noise
	click = DynKlank.ar(`[
		// the resonant frequencies are randomized a little to add variation
		// there are two high resonant freqs and one quiet "bass" freq to give it some depth
		[240*ExpRand(0.97, 1.02), 2020*ExpRand(0.97, 1.02), 3151*ExpRand(0.97, 1.02)],
		[-9, 0, -5].dbamp,
		[0.8, 0.07, 0.08]
	], BPF.ar(PinkNoise.ar, 6500, 0.1) * EnvGen.ar(Env.perc(0.001, 0.01))) * clickiness;
	snd = (snd*clickiness) + (click*(1-clickiness));

	 env = Env.new(
			 levels: [0, attack_level, decay_level, sustain_level, 0],
			 times: [attack, decay, sustain, release],
			 curve: \lin);

	 snd = Pan2.ar(Mix(snd) * EnvGen.kr(env, doneAction: 2), pan);

	 Out.ar(out_bus, snd * amp);
}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");
)

// .writeDefFile("/Users/xavierriley/Projects/sonic-pi/etc/synthdefs/compiled/");
// use .add instead of .writeDefFile to test with the following:
// Synth('sonic-pi-kalimba', [note: 60, amp: 5, pan: 0, clickiness: 0.7]);