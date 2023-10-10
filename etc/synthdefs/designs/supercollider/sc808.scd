/*
SuperCollider 808 implementation based on Yoshinosuke Horiuchi's free version. See: https://www.patreon.com/4H/posts
*/

(
SynthDef('sonic-pi-sc808_bassdrum', {|
	note = 34,
	amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
	pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
	ndecay = 30,
	attenuation=1,

	attack = 0, decay = 0, sustain = 0, release = 1,
	attack_level = 1, decay_level = -1, sustain_level = 1,
	out_bus = 0
	|

	var snd, fenv, env, trienv, sig, sub, punch, pfenv;

	note = note.midicps;

	env = EnvGen.kr(Env.new([0.11, 1, 0], [0, ndecay], -7),doneAction:2);
	trienv = EnvGen.kr(Env.new([0.11, 0.6, 0], [0, ndecay], -230),doneAction:0);
	fenv = Env([note*7, note*1.35, note], [0.05/attenuation, 0.6], -14).kr; // Mod 0.05 -> 0.5
	pfenv = Env([note*7, note*1.35, note], [0.03/attenuation, 0.6], -10).kr; // Mod 0.03 -> 0.3
	sig = SinOsc.ar(fenv, pi/2) * env;
	sub = LFTri.ar(fenv, pi/2) * trienv * 0.05;
	punch = SinOsc.ar(pfenv, pi/2) * env * 2;
	punch = HPF.ar(punch, 350);
	snd = (sig + sub + punch) * 2.5;

	amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
	pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
	decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
	env = Env.new(
		[0, attack_level, decay_level, sustain_level, 0],
		[attack,decay,sustain,release],
		\lin
	);

	snd = Limiter.ar(snd, 1) * amp;
	snd = Pan2.ar(Mix(snd) * EnvGen.kr(env, doneAction: 2), pan);

	Out.ar(out_bus, snd);

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled");
)

(
SynthDef('sonic-pi-sc808_snare', {|
	note = 65,
	amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
	pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
	attack = 0, decay = 0, sustain = 0, release = 0.15,
	attack_level = 1, decay_level = -1, sustain_level = 1,
	hpf = 93, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
	lpf = 121, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,
	mix = 0.7,
	head_hpf = 30,
	out_bus = 0|

	var noiseEnv, atkEnv, snd, noise, osc1, osc2, sum, note2, env, head;

	note2 = (note - 11).midicps;
	note = note.midicps;

	hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
	lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);

	lpf = lpf.midicps;
	hpf = hpf.midicps;
  head_hpf = head_hpf.midicps;

	amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
	pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
	decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
	env = Env.new(
		[0, attack_level, decay_level, sustain_level, 0],
		[attack,decay,sustain,release],
		\lin
	);

	noiseEnv = EnvGen.kr(Env.new(
		[0, attack_level, decay_level, sustain_level, 0],
		[attack + 0.001,decay,sustain, release],
		\lin
	), doneAction:0);

	atkEnv = EnvGen.kr(Env.perc(0.001, 0.8,curve:-95), doneAction:0);
	noise = WhiteNoise.ar;
	noise = HPF.ar(noise, hpf);
	noise = LPF.ar(noise, lpf);
	noise = noise * noiseEnv;
	osc1 = SinOsc.ar(note2, pi/2) * 0.6;
	osc2 = SinOsc.ar(note, pi/2) * 0.7;
  head = (osc1+osc2) * atkEnv * 2.5;
	snd = (mix * noise) + ((1 - mix) * head);
	snd = amp * snd * 2;
	snd = Pan2.ar(Mix(snd) * EnvGen.kr(env, doneAction: 2), pan);
	snd = HPF.ar(snd, head_hpf);
	Out.ar(out_bus, snd);
}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)


SynthDef('sonic-pi-sc808_clap', {|
	amp = 1,
	pan = 0,
	cutoff_lo = 71,
	cutoff_hi = 84,
	rev = 1,

	out_bus = 0|

	var atkenv, atk, decay, sum, denv;
	var reverb, revgen;

	cutoff_lo = cutoff_lo.midicps;
	cutoff_hi = cutoff_hi.midicps;

	// Use doneAction 0, so the synth is not freed yet
	// The reverb is running much longer
	atkenv = EnvGen.kr(Env.new([0.5,1,0],[0, 0.3], -160), doneAction:0);
	denv = EnvGen.kr(Env.dadsr(0.026, 0, 6, 0, 1, 1, curve:-157), doneAction:0);
	atk = WhiteNoise.ar * atkenv * 1.4;
	decay = WhiteNoise.ar * denv;
	sum = atk + decay * amp;
	sum = HPF.ar(sum, cutoff_lo);
	sum = BPF.ar(sum, cutoff_hi, 0.5);
	sum = 1.5 * sum;

	revgen = EnvGen.kr(Env.perc(0.1, 4, curve: -9), doneAction:2);
	reverb = WhiteNoise.ar * revgen * 0.02;
	reverb = HPF.ar(reverb, cutoff_lo);
	reverb = LPF.ar(reverb, cutoff_hi);
	reverb = reverb * amp * rev;
	Out.ar(out_bus, Pan2.ar(sum + reverb, pan));

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_tomlo', {|
	amp = 1,
	note = 40,
	pan = 0,

	out_bus = 0|

	var sig, fenv, env;

	note = note.midicps;

	env = EnvGen.kr(Env.new([0.4, 1, 0], [0, 20], -250),doneAction:2);
	fenv = Env([note*1.25, note*1.125, note], [0.1, 0.5], -4).kr;
	sig = SinOsc.ar(fenv, pi/2) * env;
	sig = sig * amp;

	Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_tommid', {|
	amp = 1,
	note = 44,
	pan = 0,

	out_bus = 0|

	var sig, fenv, env;

	note = note.midicps;

	env = EnvGen.kr(Env.new([0.4, 1, 0], [0, 16], -250),doneAction:2);
	fenv = Env([note*1.33333, note*1.125, note], [0.1, 0.5], -4).kr;
	sig = SinOsc.ar(fenv, pi/2) * env;
	sig = sig * amp;

	Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_tomhi', {|
	amp = 1,
	note = 52,
	pan = 0,

	out_bus = 0|

	var sig, fenv, env;

	note = note.midicps;

	env = EnvGen.kr(Env.new([0.4, 1, 0], [0, 11], -250),doneAction:2);
	fenv = Env([note*1.333333, note*1.121212, note], [0.1, 0.5], -4).kr;
	sig = SinOsc.ar(fenv, pi/2) * env;
	sig = sig * amp;

	Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_congalo', {|

	amp = 1,
	note = 52,
	pan = 0,

	out_bus = 0|

	var sig, fenv, env;

	note = note.midicps;

	env = EnvGen.kr(Env.new([0.15, 1, 0], [0, 18], -250),doneAction:2);
	fenv = Env([note*1.333333, note*1.121212, note], [0.1, 0.5], -4).kr;
	sig = SinOsc.ar(fenv, pi/2) * env;
	sig = sig * amp;

	Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_congamid', {|

	amp = 1,
	note = 57,
	pan = 0,

	out_bus = 0|

	var sig, fenv, env;

	note = note.midicps;

	env = EnvGen.kr(Env.new([0.15, 1, 0], [0, 9], -250),doneAction:2);
	fenv = Env([note*1.24, note*1.12, note], [0.1, 0.5], -4).kr;
	sig = SinOsc.ar(fenv, pi/2) * env;
	sig = sig * amp;

	Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_congahi', {|

	amp = 1,
	note = 65,
	pan = 0,

	out_bus = 0|

	var sig, fenv, env;

	note = note.midicps;

	env = EnvGen.kr(Env.new([0.15, 1, 0], [0, 6], -250),doneAction:2);
	fenv = Env([note*1.22972, note*1.08108, note], [0.1, 0.5], -4).kr;
	sig = SinOsc.ar(fenv, pi/2) * env;
	sig = sig * amp;

	Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_rimshot', {|
	note = 92,
	note2 = 70,
	amp = 1,
	pan = 0,
	cutoff_lo = 63,
	cutoff_hi = 118,
	out_bus = 0|

	var env, sig, punch, tri1, tri2;

	note = note.midicps;
	note2 = note2.midicps;
	cutoff_lo = cutoff_lo.midicps;
	cutoff_hi = cutoff_hi.midicps;

	env = EnvGen.kr(Env.new([1, 1, 0], [0.00272, 0.07], -42), doneAction:2);
	tri1 = LFTri.ar(note * 1.1, 1) * env;
	tri2 = LFPulse.ar(note2 * 1.1, width:0.8) * env;
	punch = WhiteNoise.ar * env * 0.46;
	sig = tri1 + tri2 + punch;
	sig = BPeakEQ.ar(sig, 464, 0.44, 8);
	sig = HPF.ar(sig, cutoff_lo);
	sig = LPF.ar(sig, cutoff_hi);
	sig = Pan2.ar(sig * amp, pan);
	Out.ar(out_bus, sig);

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_claves', {|

	amp = 1,
	note = 99,
	pan = 0,

	out_bus = 0|

	var sig, env;

	note = note.midicps;

	env = EnvGen.kr(Env.new([1, 1, 0], [0, 0.1], -20), doneAction:2);
	sig = SinOsc.ar(note, pi/2) * env * amp;

	Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_maracas', {|

	amp = 1,
	cutoff = 113,
	pan = 0,

	out_bus = 0|

	var sig, env;

	cutoff = cutoff.midicps;

	env = EnvGen.kr(Env.new([0.3, 1, 0], [0.027, 0.07], -250), doneAction:2);
	sig = WhiteNoise.ar * env * amp;
	sig = HPF.ar(sig, cutoff);

	Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_cowbell', {|

	note = 79.58979585613574,
	note2 = 72.50534928521387,
	amp = 1,
	pan = 0,
	cutoff_lo = 59,
	cutoff_hi = 109,

	out_bus = 0|

	var sig, pul1, pul2, env, atk, atkenv, datk;

	note = note.midicps;
	note2 = note2.midicps;
	cutoff_lo = cutoff_lo.midicps;
	cutoff_hi = cutoff_hi.midicps;

	atkenv = EnvGen.kr(Env.perc(0, 1, 1, -215),doneAction:0);
	env = EnvGen.kr(Env.perc(0.01, 9.5, 1, -90),doneAction:2);
	pul1 = LFPulse.ar(note);
	pul2 = LFPulse.ar(note2);
	atk = (pul1 + pul2) * atkenv * 6;
	datk = (pul1 + pul2) * env;
	sig = (atk + datk) * amp / 6;
	sig = HPF.ar(sig, cutoff_lo);
	sig = LPF.ar(sig, cutoff_hi);

	Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_closed_hihat', {|

	amp = 1,
	pan = 0,
	decay=0.42,
	cutoff_lo = 121.05875888638981,
	cutoff_hi = 121.25219487074914,

	out_bus = 0|

	var sig, sighi,siglow, sum, env, osc1, osc2, osc3, osc4, osc5, osc6;

	cutoff_lo = cutoff_lo.midicps;
	cutoff_hi = cutoff_hi.midicps;

	env = EnvGen.kr(Env.perc(0.005, decay, 1, -30),doneAction:2);
	osc1 = LFPulse.ar(203.52);
	osc2 = LFPulse.ar(366.31);
	osc3 = LFPulse.ar(301.77);
	osc4 = LFPulse.ar(518.19);
	osc5 = LFPulse.ar(811.16);
	osc6 = LFPulse.ar(538.75);
	sighi = (osc1 + osc2 + osc3 + osc4 + osc5 + osc6);
    siglow = (osc1 + osc2 + osc3 + osc4 + osc5 + osc6);
    sighi = BPF.ar(sighi, cutoff_lo, 1);
    sighi = HPF.ar(sighi, cutoff_hi);
    siglow = BBandPass.ar(siglow, 8900, 0.8);
    siglow = BHiPass.ar(siglow, 9000, 0.3);
    sig = BPeakEQ.ar((siglow+sighi), 9700, 0.8, 0.7);
    sig = sig * env * amp * 12;

	Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_open_hihat', {|

	amp = 1,
	pan = 0,
	decay=0.5,

	out_bus = 0|

	var sig, siga, sigb, env1, env2, osc1, osc2, osc3, osc4, osc5, osc6, sum;

	env1 = EnvGen.kr(Env.perc(0.1, decay, curve:-3), doneAction:2);
	env2 = EnvGen.kr(Env.new([0, 1, 0], [0, decay*5], curve:-150), doneAction:0);
	osc1 = LFPulse.ar(203.52) * 0.6;
	osc2 = LFPulse.ar(366.31) * 0.6;
	osc3 = LFPulse.ar(301.77) * 0.6;
	osc4 = LFPulse.ar(518.19) * 0.6;
	osc5 = LFPulse.ar(811.16) * 0.6;
	osc6 = LFPulse.ar(538.75) * 0.6;
	sig = osc1 + osc2 + osc3 + osc4 + osc5 +osc6;
	sig = BLowShelf.ar(sig, 990, 2, -3);
	sig = BPF.ar(sig, 7700);
	sig = BPeakEQ.ar(sig, 7200, 0.5, 5);
	sig = BHiPass4.ar(sig, 8100, 0.7);
	sig = BHiShelf.ar(sig, 9400, 1, 5);
	siga = sig * env1 * 0.6;
	sigb = sig * env2;
	sum = siga + sigb;
	sum = LPF.ar(sum, 4000);
	sum = sum * amp * 7;

	Out.ar(out_bus, Pan2.ar(sum, pan));

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_cymbal', {|

	amp = 1,
	pan = 0,
	decay=2,
	tone=0.002,

	out_bus = 0|

	var sig, sig1, sig2, sig2a, sig2b, sig3, env1, env2, env2b, env3, osc1, osc2, osc3, osc4, osc5, osc6, sum;
	env1 = EnvGen.kr(Env.perc(0.3, decay, curve:-3), doneAction:2);
	env2 = EnvGen.kr(Env.new([0, 0.6, 0], [0.1, decay*0.7], -5), doneAction:0);
	env2b = EnvGen.kr(Env.new([0, 0.3, 0], [0.1, decay*20], -120), doneAction:0);
	env3 = EnvGen.kr(Env.new([0, 1, 0], [0, decay*5], curve:-150), doneAction:0);
	osc1 = LFPulse.ar(203.52) * 0.6;
	osc2 = LFPulse.ar(366.31) * 0.6;
	osc3 = LFPulse.ar(301.77) * 0.6;
	osc4 = LFPulse.ar(518.19) * 0.6;
	osc5 = LFPulse.ar(811.16) * 0.6;
	osc6 = LFPulse.ar(538.75) * 0.6;
	sig = osc1 + osc2 + osc3 + osc4 + osc5 +osc6;
	sig1 = BLowShelf.ar(sig, 2000, 1, 5);
	sig1 = BPF.ar(sig1, 3000);
	sig1 = BPeakEQ.ar(sig1, 2400, 0.5, 5);
	sig1 = BHiPass.ar(sig1, 1550, 0.7);
	sig1 = LPF.ar(sig1, 3000);
	sig1 = BLowShelf.ar(sig1, 1000, 1, 0);
	sig1 = sig1 * env1 * tone;
	sig2 = BLowShelf.ar(sig, 990, 2, -5);
	sig2 = BPF.ar(sig2, 7400);
	sig2 = BPeakEQ.ar(sig2, 7200, 0.5, 5);
	sig2 = BHiPass4.ar(sig2, 6800, 0.7);
	sig2 = BHiShelf.ar(sig2, 10000, 1, -4);
	sig2a = sig2 * env2 * 0.3;
	sig2b = sig2 * env2b * 0.6;
	sig3 = BLowShelf.ar(sig, 990, 2, -15);
	sig3 = BPF.ar(sig3, 6500);
	sig3 = BPeakEQ.ar(sig3, 7400, 0.35, 10);
	sig3 = BHiPass4.ar(sig3, 10500, 0.8, 2);
	sig3 = sig3 * env3;
	sum = sig1 + sig2a + sig2b + sig3;
	sum = LPF.ar(sum, 4000);
	sum = Pan2.ar(sum, 0);
	sum = sum * amp * 12;
	Out.ar(0, sum);

}).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");
)