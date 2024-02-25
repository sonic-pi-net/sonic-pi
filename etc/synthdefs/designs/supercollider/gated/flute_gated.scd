// Copyright notices

// Perry Cook's Slide Flute: A Simple Flute Physical Model
// https://ccrma.stanford.edu/software/clm/compmus/clm-tutorials/pm.html#s-f

// Source for SuperCollider adapation taken from:
// https://github.com/everythingwillbetakenaway/Synthdefs/blob/master/flute.scd

// Originally found at http://ecmc.rochester.edu/ecmc/docs/supercollider/scbook/Ch21_Interface_Investigations/ixi%20SC%20tutorial/ixi_SC_tutorial_10.html
//by Wilson, Cottle and Collins
//also available at Bruno Ruviaro Collection https://github.com/brunoruviaro/SynthDefs-for-Patterns/blob/master/flute.scd

(
SynthDef('sonic-pi-flute_gated', {|
	note = 40, note_slide = 0, note_slide_shape = 1, note_slide_curve = 0,
	amp = 0.4, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
	pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
	attack = 0, decay = 0, sustain = 0, release = 1,
	attack_level = 1, decay_level = -1, sustain_level = 1,
	noise_attack = 0.06, noise_decay = 0.2, noise_sustain = 0, noise_release = 0.2,
	noise_attack_level = 0.99, noise_decay_level = -1, noise_sustain_level = 0.9,
	vibrato_attack = 0.5, vibrato_decay = 0.5, vibrato_sustain = 0, vibrato_release = 0.5,
	vibrato_attack_level = 0, vibrato_decay_level = -1, vibrato_sustain_level = 1,
	ibreath = 0.13, ibreath_slide = 0, ibreath_slide_shape = 1, ibreath_slide_curve = 0,
	ifeedbk1 = 0.5,	ifeedbk1_slide = 0, ifeedbk1_slide_shape = 1, ifeedbk1_slide_curve = 0,
	ifeedbk2 = 0.57, ifeedbk2_slide = 0, ifeedbk2_slide_shape = 1, ifeedbk2_slide_curve = 0,
	gate = 1,
	out_bus = 0|

	var kenv1, kenv2, kenvibr, kvibr, sr, cr, block;
	var poly, signalOut;
	var aflow1, asum1, asum2, afqc, atemp1, ax, apoly, asum3, avalue, atemp2, aflute1;
	var ifeedbk2_clipped, fdbckArray;

	sr = SampleRate.ir;
	cr = ControlRate.ir;
	block = cr.reciprocal;

	note = note.midicps;
	note = note.varlag(note_slide, note_slide_curve, note_slide_shape);
	decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
	amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
	pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

	decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
	noise_decay_level = Select.kr(noise_decay_level < 0, [noise_decay_level, noise_sustain_level]);
	vibrato_decay_level = Select.kr(vibrato_decay_level < 0, [vibrato_decay_level, vibrato_sustain_level]);

	ibreath = ibreath.varlag(ibreath_slide, ibreath_slide_curve, ibreath_slide_shape);
	ifeedbk1 = ifeedbk1.varlag(ifeedbk1_slide, ifeedbk1_slide_curve, ifeedbk1_slide_shape);
	ifeedbk2 = ifeedbk2.varlag(ifeedbk2_slide, ifeedbk2_slide_curve, ifeedbk2_slide_shape);

	ibreath = 0.7 * ibreath;
	ifeedbk1 = 0.8 * ifeedbk1;
	ifeedbk2 = 0.7 * ifeedbk2;
	ifeedbk2_clipped = Select.kr((ifeedbk1 + ifeedbk2) > 1.1, [ifeedbk2, (1.1 - ifeedbk1)]);

	// Noise envelope
	kenv1 = EnvGen.kr(Env.new(
		[0, noise_attack_level, noise_decay_level, noise_sustain_level, 0],
		[noise_attack,noise_decay,noise_sustain,noise_release],
		\linear,
		3
	),
	gate
	);

	// Amp envelope
	kenv2 = EnvGen.kr(Env.new(
		[0, attack_level, decay_level, sustain_level, 0],
		[attack,decay,sustain,release],
		\sine,
		3
	),
	gate,
	doneAction: 2
	);

	// vibrato envelope
	kenvibr = EnvGen.kr(Env.new(
		[0, vibrato_attack_level, vibrato_decay_level, vibrato_sustain_level, 0],
		[vibrato_attack,vibrato_decay,vibrato_sustain,vibrato_release],
		\linear,
		3
	),
	gate
	);

	// Create air flow and vibrato
	// The actual tone is created by feeding back this noise signal (see LocalOut below).
	// Hence, kenv1 determines if one hears anything.
	aflow1 = LFClipNoise.ar( sr, kenv1 );
	kvibr = SinOsc.ar( 5, 0, 0.1 * kenvibr );

	asum1 = ( ibreath * aflow1 ) + kenv1 + kvibr;
	afqc = note.reciprocal - ( asum1/20000 ) - ( 9/sr ) + ( note/12000000 ) - block;

	fdbckArray = LocalIn.ar( 1 );

	aflute1 = fdbckArray;
	asum2 = asum1 + ( aflute1 * ifeedbk1 );

	ax = DelayC.ar( asum2, note.reciprocal - block * 0.5, afqc * 0.5 - ( asum1/note/cr ) + 0.001 );

	apoly = ax - ( ax.cubed );
	asum3 = apoly + ( aflute1 * ifeedbk2_clipped );
	avalue = LPF.ar( asum3, 2000 );

	aflute1 = DelayC.ar( avalue, note.reciprocal - block, afqc );

	fdbckArray = [ aflute1 ];

	LocalOut.ar( fdbckArray );

	signalOut = avalue;

	signalOut = Pan2.ar(Mix(signalOut) * kenv2, pan);

	Out.ar( out_bus, signalOut * amp );
}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");
)