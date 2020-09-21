b = Buffer.read(s, "/Users/ethan/repos/sonic-pi/etc/buffers/rand-stream.wav");

SynthDef("sonic-pi-flock",
  {|bufnum = 0, num_boids = 20, seed = 0, min_speed = 0, max_speed = 3.5, max_force = 0.5,
	randomise_start_positions = 0, randomise_start_speeds = 1, randomise_start_angles = 1
	include_predators = 0,
    note = 52, note_slide = 0, note_slide_shape = 1, note_slide_curve = 0,
	amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
	pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
	attack = 0, decay = 0, sustain = 0, release = 1,
	attack_level = 1, decay_level = -1, sustain_level = 1, env_curve = 1,
 out_bus = 0|

  var snd, env, freq;

	decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
	note = note.varlag(note_slide, note_slide_curve, note_slide_shape);
	amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
	pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
  freq = note.midicps;
 	env = Env.new(
    [0, attack_level, decay_level, sustain_level, 0],
  	[attack, decay, sustain, release], env_curve
  );

	snd = Flock.ar(bufnum, freq, 0.0, seed, num_boids);
  snd = Pan2.ar(snd * amp * EnvGen.ar(env, doneAction: 2), pan);
  Out.ar(out_bus, snd)}
).load(s);
//writeDefFile("C:/Users/Ethan/repos/sonic-pi/etc/synthdefs/compiled");
a = Synth("sonic-pi-flock", [bufnum: b.bufnum, amp: 1, note: 60, sustain: 10, release: 10, seed: 0]);
a.free;