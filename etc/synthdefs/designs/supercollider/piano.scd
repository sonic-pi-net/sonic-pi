(
SynthDef('sonic-pi-piano', {|
	note = 52, amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
	pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
	attack = 0, decay = 0, sustain = 0, release = 1,
	attack_level = 1, decay_level = -1, sustain_level = 1, env_curve = 1,
	vel = 0.2, hard = 0.5, velcurve = 0.8, stereo_width = 0,
	out_bus = 0 |

	var snd, env;
	var rounded = round(note, 1);

	decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
	amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
	pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

	snd = MdaPiano.ar(freq: rounded.midicps, tune: note - rounded + 0.5, gate: 1,
		vel: vel.clip(0, 1).linlin(0, 1, 0, 127),
		hard: hard.clip(0, 1).linlin(0, 1, -3, 3),
		decay: decay, release: release, velhard: 0.8,
		muffle: 0.8, velmuff: 0.8, velcurve: velcurve, stereo: stereo_width,
		random: 0, stretch: 0, sustain: 0.1);
	snd = Balance2.ar(snd[0], snd[1], pan, amp) * 60;
	env = Env.new([0, attack_level, decay_level, sustain_level, 0],
		[attack, decay, sustain, release],
		env_curve);
	Out.ar(out_bus, snd * EnvGen.kr(env, doneAction: 2))}
).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/")
)
