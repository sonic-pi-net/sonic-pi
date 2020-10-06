b = Buffer.read(s, "/Users/sam/Development/RPi/sonic-pi/etc/buffers/rand-stream.wav");

SynthDef("sonic-pi-flock",
  {|bufnum = 0, num_boids = 20, seed = 0, min_start_speed = 0, max_speed = 3.5, max_force = 0.5,
    note_mod_source = 3,
    note_mod_amount = 1, note_mod_amount_slide = 0, note_mod_amount_slide_shape = 1, note_mod_amount_slide_curve = 0,
    predator_probability = 0,
    note = 52, note_slide = 0, note_slide_shape = 1, note_slide_curve = 0,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    attack = 0, decay = 0, sustain = 0, release = 1,
    attack_level = 1, decay_level = -1, sustain_level = 1, env_curve = 1,
    out_bus = 0|

  var snd, env, freq;

  decay_level = Select.kr(decay_level < 0, [decay_level, sustain_level]);
  note = note.varlag(note_slide, note_slide_curve, note_slide_shape);
  note_mod_amount = note_mod_amount.varlag(note_mod_amount_slide, note_mod_amount_slide_curve, note_mod_amount_slide_shape);
  amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
  pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

  env = Env.new(
    [0, attack_level, decay_level, sustain_level, 0],
  	[attack, decay, sustain, release], env_curve
  );
  freq = note.midicps;

  // The custom Flock UGen generates the audio samples using a flocking algorithm.
  snd = Flock.ar(
    bufnum,
    freq,
    seed,
    num_boids,
    min_start_speed,
    max_speed,
    max_force,
    note_mod_source,
    note_mod_amount,
    predator_probability
  );
  snd = Pan2.ar(snd * amp * EnvGen.ar(env, doneAction: 2), pan);
  Out.ar(out_bus, snd)}
).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

// Use .add instead of .writeDefFile to test quickly. You could use the following Synth values:
// Synth("sonic-pi-flock", [bufnum: b.bufnum, amp: 1, note: 60, sustain: 10, release: 10, seed: 0, note_mod_source: 3, min_start_speed: 1, max_speed: 4, max_force: 1, note_mod_amount: 1, predator_probability: 0.1]);
