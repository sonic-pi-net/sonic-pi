/*
Port of snappizz simplified FM Rhodes Synthesizer, taken from here:

  https://sccode.org/1-522

Original notes:

FM Rhodes Synthesizer

Native SuperCollider port of STK's Rhodey. This should be preferred over the StkInst version because:

- It uses much less CPU.
- It is easier to modify.
- It doesn't require sc3-plugins or a correct setting of StkGlobals.
- It's beginner-friendly because it uses only basic UGens: SinOsc, EnvGen, Mix, Pan2, Out.

*/

(
SynthDef('sonic-pi-rhodey', {
    |
	note = 69, note_slide = 0, note_slide_shape = 1, note_slide_curve = 0,
	amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
	pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
	attack = 0.001, decay = 1, sustain = 0, release = 1,
	attack_level = 1, decay_level = -1, sustain_level = 0,

	lfo_width = 0.3, lfo_width_slide = 0, lfo_width_slide_shape = 1, lfo_width_slide_curve = 0,
	lfo_rate = 0.4, lfo_rate_slide = 0, lfo_rate_slide_shape = 1, lfo_rate_slide_curve = 0,

    // all of these range from 0 to 1
    vel = 0.8,
	mod_index = 0.2, mod_index_slide = 0, mod_index_slide_shape = 1, mod_index_slide_curve = 0,
	mix = 0.2, mix_slide = 0, mix_slide_shape = 1, mix_slide_curve = 0,

	out_bus = 0|

    var env, env1, env2, env3, env4;
    var osc1, osc2, osc3, osc4, snd;

	note = note.midicps * 2;
	note = note.varlag(note_slide, note_slide_curve, note_slide_shape);
	decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
	amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
	pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

	mod_index = mod_index.varlag(mod_index_slide, mod_index_slide_curve, mod_index_slide_shape);
	mix = mix.varlag(mix_slide, mix_slide_curve, mix_slide_shape);
	lfo_width = lfo_width.varlag(lfo_width_slide, lfo_width_slide_curve, lfo_width_slide_shape);
	lfo_rate = lfo_rate.varlag(lfo_rate_slide, lfo_rate_slide_curve, lfo_rate_slide_shape);

    lfo_rate = lfo_rate * 12;

	env1 = EnvGen.ar(Env.new(
		[0, attack_level, decay_level, sustain_level, 0],
		[attack,decay*1.25,sustain,release],
		\lin
	), doneAction: 2);

	env2 = EnvGen.ar(Env.new(
		[0, attack_level, decay_level, sustain_level, 0],
		[attack,decay,sustain,release],
		\lin
	), doneAction: 2);

	env3 = EnvGen.ar(Env.new(
		[0, attack_level, decay_level, sustain_level, 0],
		[attack,decay*1.50,sustain,release],
		\lin
	), doneAction: 2);

	env4 = EnvGen.ar(Env.new(
		[0, attack_level, decay_level, sustain_level, 0],
		[attack,decay*1.50,sustain,release],
		\lin
	), doneAction: 2);

    osc4 = SinOsc.ar(note * 0.5) * 2pi * 2 * 0.535887 * mod_index * env4 * vel;
    osc3 = SinOsc.ar(note, osc4) * env3 * vel;
    osc2 = SinOsc.ar(note * 15) * 2pi * 0.108819 * env2 * vel;
    osc1 = SinOsc.ar(note, osc2) * env1 * vel;
    snd = Mix((osc3 * (1 - mix)) + (osc1 * mix));
    snd = snd * (SinOsc.ar(lfo_rate) * lfo_width + 1);

    // using the doneAction: 2 on the other envs can create clicks (bc of the linear curve maybe?)

	env = Env.new(
		[0, attack_level, decay_level, sustain_level, 0],
		[attack,decay,sustain,release],
		\lin
	);

    snd = snd * EnvGen.kr(env, doneAction: 2);
    snd = Pan2.ar(snd, pan, amp);

    Out.ar(out_bus, snd);
}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)