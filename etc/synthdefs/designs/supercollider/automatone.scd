b = Buffer.read(s, "/Users/sam/Development/RPi/sonic-pi/etc/buffers/rand-stream.wav");

SynthDef("sonic-pi-automatone",
  {|bufnum = 0, wolfram_code = 30,
    num_columns = 8, num_rows = 2, encoded_partials = 255, seed = 0, width = 1,
    odd_skew = 0, even_skew = 0, amp_tilt = 1, balance = 0, randomise = 0,
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

  env = Env.new(
    [0, attack_level, decay_level, sustain_level, 0],
    [attack, decay, sustain, release], env_curve
  );
  freq = note.midicps;

  // The custom ElementaryCA UGen generates the audio samples using an elementary cellular automaton algorithm.
  snd = ElementaryCA.ar(
    bufnum,
    freq,
    wolfram_code,
    num_columns,
    num_rows,
    encoded_partials,
    seed,
    width,
    odd_skew,
    even_skew,
    amp_tilt,
    balance,
    randomise
  );
  snd = Pan2.ar(snd * amp * EnvGen.ar(env, doneAction: 2), pan);
  Out.ar(out_bus, snd)}
).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/");

// Use .add instead of .writeDefFile to test quickly. You could use the following Synth values:
// Synth("sonic-pi-automatone", [bufnum: b.bufnum, note: 60, amp: 1, wolfram_code: 30, sustain: 5, release: 10, seed: 0, balance: 0.0, width: 1, odd_skew: 0.0]);
// It's handy to use the frequency scope to visualise the partials:
// FreqScope.new(512, 300, 0, server: s);
