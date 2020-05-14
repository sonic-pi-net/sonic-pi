# Adapted from https://github.com/everythingwillbetakenaway/Synthdefs/blob/master/everythingrhodes.scd
(
 SynthDef('sonic-pi-rodeo',{|
	 note = 52, note_slide = 0, note_slide_shape = 1, note_slide_curve = 0,
	 amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
	 pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
	 attack = 0, decay = 0, sustain = 0, release = 1,
	 attack_level = 1, decay_level = -1, sustain_level = 1,
   cutoff = 95, cutoff_slide = 0, cutoff_slide_shape = 1, cutoff_slide_curve = 1,
	 gate=1, rq=0.5,

	 use_chorus=0, use_compressor=0, amp_scale = 0.1,

	 out_bus = 0|

	 var snd, filter, filter_env, env, chorus;

	 note = note.midicps;
	 note = note.varlag(note_slide, note_slide_curve, note_slide_shape);
	 decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
	 amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
	 pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
   cutoff = cutoff.midicps;
   cutoff = cutoff.varlag(cutoff_slide, cutoff_slide_curve, cutoff_slide_shape);

	 snd = Pulse.ar(note*[1,33.5.midiratio],[0.2,0.1],[0.7,0.3]);

	 filter_env = EnvGen.ar(Env.adsr(0.0, 1.0, 0.8, 3.0), gate, doneAction:2);

	 //keyboard tracking filter cutoff
	 snd = BLowPass4.ar(snd,(cutoff * (filter_env.squared)) + 200 + note, rq);

	 chorus = Select.ar(use_chorus,
			 [
			 snd,
			 Splay.ar(Array.fill(4,{
					 var maxdelaytime= rrand(0.005,0.02);

					 DelayC.ar(snd[0], maxdelaytime,LFNoise1.kr(Rand(0.1,0.6),0.25*maxdelaytime,0.75*maxdelaytime) )
					 }))
			 ]);

	 snd = Select.ar(use_compressor,
			 [
			 snd,
			 Compander.ar(2*(snd + chorus),snd*amp_scale,0.4,1,4)
			 ]);

	 env = Env.new(
			 levels: [0, attack_level, decay_level, sustain_level, 0],
			 times: [attack, decay, sustain, release],
			 curve: \lin);

	 snd = Pan2.ar(Mix(snd) * EnvGen.kr(env, doneAction: 2), pan);

	 Out.ar(out_bus, snd * amp);

 }).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/")
);
