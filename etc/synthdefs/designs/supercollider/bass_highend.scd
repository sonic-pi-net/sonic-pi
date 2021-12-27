// Adapted for Sonic Pi from
//   https://raw.githubusercontent.com/supercollider/supercollider/develop/examples/demonstrations/stealthissound.scd
// Published there under GPL v3, so re-published under the same terms, see:
//   https://www.gnu.org/licenses/gpl-3.0.en.html
// Date of modification: 11.01.2021

(
SynthDef('sonic-pi-bass_highend',{|
	note = 40, note_slide = 0, note_slide_shape = 1, note_slide_curve = 0,
	amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
	pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
	attack = 0, decay = 0, sustain = 0, release = 1,
	attack_level = 1, decay_level = -1, sustain_level = 1,
	cutoff = 102, cutoff_slide = 0, cutoff_slide_shape = 1, cutoff_slide_curve = 0,
	res = 0.9, res_slide = 0, res_slide_shape = 1, res_slide_curve = 0,
	drive = 2.0, drive_slide = 0, drive_slide_shape = 1, drive_slide_curve = 0,
	out_bus = 0|

	var osc, snd, env, filterenv, ab;

	note = note.midicps;
	note = note.varlag(note_slide, note_slide_curve, note_slide_shape);
	decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
	amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
	pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

	cutoff = cutoff.midicps;
	cutoff = cutoff.varlag(cutoff_slide, cutoff_slide_curve, cutoff_slide_shape);

	res = res.linlin(0, 1, 1, 0);
	res = res.varlag(res_slide, res_slide_curve, res_slide_shape);

	drive = drive.varlag(drive_slide, drive_slide_curve, drive_slide_shape);

	osc = Mix(Saw.ar(note*[0.25,1,1.5],[0.5,0.4,0.1]));
	filterenv = EnvGen.ar(Env.adsr(0.0,0.5,0.2,0.2), doneAction:2);
	snd =  RLPF.ar(osc,cutoff*filterenv+100,res);

	ab = abs(snd);
	snd = (snd*(ab + drive)/(snd ** 2 + (drive - 1) * ab + 1));

	// Remove low end
	snd = BLowShelf.ar(snd, 300, 1.0, -12);

	// Dip at 1600Hz
	snd = BPeakEQ.ar(snd, 1600, 1.0, -6);

	env = Env.new(
		[0, attack_level, decay_level, sustain_level, 0],
		[attack,decay,sustain,release],
		\lin
	);

	snd = Pan2.ar(Mix(snd) * EnvGen.kr(env, doneAction: 2) * 2, pan);

	Out.ar(out_bus, snd * amp);

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");
)