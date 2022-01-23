/*
Additive tonewheel organ with optional rotary speaker
Created for Sonic Pi by Ben Marx based on work of
- Chris Wigington: https://actlab.us/actlab/cwigington/projone.html
  Released under both MIT and GPLv3 by personal note to the author on 08.12.2021
- Zé Craum: http://sccode.org/1-5aD
  Published there under GPL v3
Published with Sonic Pi under GPL v3, see:
  https://www.gnu.org/licenses/gpl-3.0.en.html
Date of modification: 02.01.2022

An additive synth similar to a tonewheel organ, adding several sine signals with individual amplitudes, and
an optional rotary speaker. The tonewheel organ's rotary speaker affects sound in (at least) 3 ways:

- The frequency changes due to a Doppler effect, so that the pitch oscillates around the base frequency (note).
- The amplitude changes. When the horns rotate, they sound louder when they point towards the listener.
- The pan changes: When the horns point sideways, they sound louder on the side they point to.

Further details can be found at:
- https://www.soundonsound.com/techniques/synthesizing-tonewheel-organs-part-1
- http://www.goodeveca.net/RotorOrgan/ToneWheelSpec.html
- https://en.wikipedia.org/wiki/Leslie_speaker

Note that this synth uses calculated harmonics, while real tonewheel organs are slightly detuned,
due to limitations of the wheel geometry. See:
- No detuning due to fixed tonewheels: http://www.goodeveca.net/RotorOrgan/ToneWheelSpec.html/

Using these detuned frequencies might make this synth more natural -- maybe an option for the
tonewheel de luxe :)
*/

(
SynthDef('sonic-pi-organ_tonewheel', {|
	note = 60, note_slide = 0, note_slide_shape = 1, note_slide_curve = 0,
	amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
	pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
	attack = 0.01, decay = 0, sustain = 1, release = 0.01,
	attack_level = 1, decay_level = -1, sustain_level = 1,

	// Organ voices (drawbars) amplitudes
	bass = 8, bass_slide = 0, bass_slide_shape = 1, bass_slide_curve = 0,
	quint = 8, quint_slide = 0, quint_slide_shape = 1, quint_slide_curve = 0,
	fundamental = 8, fundamental_slide = 0, fundamental_slide_shape = 1, fundamental_slide_curve = 0,
	oct = 8, oct_slide = 0, oct_slide_shape = 1, oct_slide_curve = 0,
	nazard = 0, nazard_slide = 0, nazard_slide_shape = 1, nazard_slide_curve = 0,
	blockflute = 0, blockflute_slide = 0, blockflute_slide_shape = 1, blockflute_slide_curve = 0,
	tierce = 0, tierce_slide = 0, tierce_slide_shape = 1, tierce_slide_curve = 0,
	larigot = 0, larigot_slide = 0, larigot_slide_shape = 1, larigot_slide_curve = 0,
	sifflute = 0, sifflute_slide = 0, sifflute_slide_shape = 1, sifflute_slide_curve = 0,

	// Rotary speaker arguments
	rs_freq = 6.7, rs_freq_slide = 0, rs_freq_slide_shape = 1, rs_freq_slide_curve = 0,
	rs_freq_var = 0.1,
	rs_pitch_depth = 0.008,
	rs_delay = 0,
	rs_onset = 0,
	rs_pan_depth = 0.05,
	rs_amplitude_depth = 0.2,

	out_bus = 0|

	var snd, eg, discs, drawbars;
	var pitch, lf, hf, lt, ht, lp, hp, lo, hi, ht_iphase, hp_iphase;

	note = note.midicps;
	note = note.varlag(note_slide, note_slide_curve, note_slide_shape);
	decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
	amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
	pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

	bass = bass.varlag(bass_slide, bass_slide_curve, bass_slide_shape);
	quint = quint.varlag(quint_slide, quint_slide_curve, quint_slide_shape);
	fundamental = fundamental.varlag(fundamental_slide, fundamental_slide_curve, fundamental_slide_shape);
	oct = oct.varlag(oct_slide, oct_slide_curve, oct_slide_shape);
	nazard = nazard.varlag(nazard_slide, nazard_slide_curve, nazard_slide_shape);
	blockflute = blockflute.varlag(blockflute_slide, blockflute_slide_curve, blockflute_slide_shape);
	tierce = tierce.varlag(tierce_slide, tierce_slide_curve, tierce_slide_shape);
	larigot = larigot.varlag(larigot_slide, larigot_slide_curve, larigot_slide_shape);
	sifflute = sifflute.varlag(sifflute_slide, sifflute_slide_curve, sifflute_slide_shape);

	rs_freq = rs_freq.varlag(rs_freq_slide, rs_freq_slide_curve, rs_freq_slide_shape);

	// The rotation frequency of the woofer is lower than the horn's and must not be negative.
	lf = rs_freq.linlin(0, 6.666667, -0.047619, 5.666667);
	lf = Select.kr(lf < 0, [lf, 0]);

	// If the rotary speaker is off, the horn's initial phase must be zero, so that the
	// loudness and pan are not affected.
	hf = rs_freq;
	ht_iphase = Select.kr(rs_freq, [0, -pi]);
	hp_iphase = Select.kr(rs_freq, [0, -pi/2]);

	// Loudness oscillation
	// Vibrato adds an oscillating proportion to the base frequency, so if the base frequency is 0, then the
	// vibrato result is 0. Hence the trick with DC.kr(1). This is used for pan only, as loudness oscillating
	// around 1 is good.
	lt = Vibrato.kr(
		DC.kr(1), lf, DC.kr(rs_amplitude_depth),
		DC.kr(rs_delay), DC.kr(rs_onset),
		DC.kr(rs_freq_var), DC.kr(rs_freq_var)
	);
	ht = Vibrato.kr(
		DC.kr(1), hf, DC.kr(rs_amplitude_depth),
		DC.kr(rs_delay), DC.kr(rs_onset),
		DC.kr(rs_freq_var), DC.kr(rs_freq_var), ht_iphase
	);

	// Pan
	lp = Vibrato.kr(
		DC.kr(1), lf, DC.kr(rs_pan_depth),
		DC.kr(rs_delay), DC.kr(rs_onset),
		DC.kr(rs_freq_var), DC.kr(rs_freq_var)
	) - DC.kr(1);
	hp = Vibrato.kr(
		DC.kr(1), hf, DC.kr(rs_pan_depth),
		DC.kr(rs_delay), DC.kr(rs_onset),
		DC.kr(rs_freq_var), DC.kr(rs_freq_var), hp_iphase
	) - DC.kr(1);

	// The vibrato adds the third rotary speaker component, the pitch
	pitch = Vibrato.kr(
		note, hf, DC.kr(rs_pitch_depth),
		DC.kr(rs_delay), DC.kr(rs_onset),
		DC.kr(rs_freq_var), DC.kr(rs_freq_var)
	);

	// Harmonics relationships between the tonewheel discs
	discs = [1/2,3/2,1,2,3,4,5,6,8];

	// Drawbar settings
	drawbars = [bass,quint,fundamental,oct,nazard,blockflute,tierce,larigot,sifflute];

	eg = EnvGen.kr(Env.new(
		[0, attack_level, decay_level, sustain_level, 0],
		[attack,decay,sustain,release],
		'sine'),
	doneAction:2);

	// The vibrato actually only makes the base frequency oscillate. It is then used as the frequency of a SinOsc.
	// This is the reason why the rotary speaker fx can't easily be split from the synth. The vibrato input to
	// the SinOsc yields a far better result than a PitchShift of an arbitrary signal.
	snd = Mix.new(SinOsc.ar(pitch*discs, 0, eg*drawbars/144)).distort;

	// The lower frequencies go into the woofer, the higher ones into the horn, both with their own rotations
	lo = Pan2.ar(LPF.ar(snd, 800, lt), lp + pan);
	hi = Pan2.ar(HPF.ar(snd, 800, ht), hp + pan);
	snd = Mix.new([lo,hi]).distort*4;

	Out.ar(out_bus, snd * amp);
}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");
)
