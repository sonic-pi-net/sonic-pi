/*
Adapted port of snappizz gabberkick synthesizer, taken from here:

  https://sccode.org/1-57r gabber kick
*/

(
SynthDef('sonic-pi-gabberkick_gated', {|
 	note = 34,
	amp = 0.5, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
	pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
	attack = 0.001, decay = 0.01, sustain = 0.3, release = 0.02,
	attack_level = 1, decay_level = 0.7, sustain_level = 0.7,

	slope_start = 84, slope_length1 = 0.015,
	slope_intermediate = 69, slope_length2 = 0.1,
	boost = 8, boost_slide = 0, boost_slide_shape = 1, boost_slide_curve = 0,

	cutoff = 119, cutoff_slide = 0, cutoff_slide_shape = 1, cutoff_slide_curve = 0,
	res = 0.2, res_slide = 0, res_slide_shape = 1, res_slide_curve = 0,

	gate = 1,

	out_bus = 0|

	var snd, env;

	note = note.midicps;
	decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
	amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
	pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

	cutoff = cutoff.midicps;
	cutoff = cutoff.varlag(cutoff_slide, cutoff_slide_curve, cutoff_slide_shape);

	slope_start = slope_start.midicps;
	slope_intermediate = slope_intermediate.midicps;

	res = res.linlin(0, 1, 1, 0);
	res = res.varlag(res_slide, res_slide_curve, res_slide_shape);

	boost = boost.varlag(boost_slide, boost_slide_curve, boost_slide_shape);

    snd = SinOsc.ar(
		Env(
			[slope_start, slope_intermediate, note],
			[slope_length1, slope_length2],
			\exp
		).ar * [-0.1, 0, 0.1].midiratio);

	snd = Mix(snd);
    snd = (snd * 10).tanh;
    snd = (snd * 5.1).atan;
    snd = BPeakEQ.ar(snd, XLine.kr(2000, 100, 0.3), 2.0, boost);
    snd = snd + DelayC.ar(snd, 0.01, SinOsc.ar(4).range(0.0, 0.001));
	snd = RLPF.ar(snd, cutoff, res);
	snd = ((snd * 0.6).clip(-1, 1));

	env = Env.new(
		[0, attack_level, decay_level, sustain_level, 0],
		[attack,decay,0,release],
		\lin,
		3
	);

	snd = Pan2.ar(snd * EnvGen.kr(env, gate, doneAction: 2), pan);
	Out.ar(out_bus, snd * amp * 0.5);

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");
)