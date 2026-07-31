// sonic-pi-mixer, re-authored in sclang from the original Overtone design.
//
// Two controls set the level, and they are deliberately on opposite sides
// of the limiter:
//
//  - pre_amp is DRIVE. It sits before the limiter, so raising it makes the
//    mix louder and denser by pushing the limiter harder. It changes the
//    sound, and its meter is gain reduction.
//  - amp is VOLUME. It sits after the limiter and is clamped to unity, so
//    it can only ever attenuate. It cannot change the sound, only how
//    loud the output is, and it cannot drive anything into the ceiling.
//
// That split is the standard console layout: input trim with a GR meter,
// output fader with a level meter. It is what makes "turn it down" safe to
// do mid-performance. With amp before the limiter, riding the master
// would change how much the limiter works and therefore the tone.
//
// The clamp on amp is what makes it safe to put the fader last: an
// unclamped post-limiter gain would undo the ceiling.
//
// Chain: merge hardware bus + mix bus -> Sanitize -> pre_amp (drive) ->
// switchable HPF/LPF -> force_mono -> invert_stereo -> LeakDC ->
// pre-limiter scope tap -> fixed 10Hz/20.5kHz safety filters -> SPLimiter2
// -> amp (volume, <= unity) -> hard clip at +-1 -> ReplaceOut.
//
// The limiter is SPLimiter rather than SuperCollider's Limiter because its
// latency lands on everything Sonic Pi emits, including live input
// monitored through the mixer. SPLimiter's latency is exactly its
// lookahead, where Limiter's is twice its dur. See SPLimiterCore.h in SuperSonic for the
// design and for why the ceiling holds without the clip having to work.
//
// The safety filters run BEFORE the limiter so that the limiter is the
// last thing to touch the signal and its ceiling is therefore the
// ceiling of the output. A 2-pole Butterworth overshoots on a step, so
// any filter downstream of the limiter rings back above the ceiling:
// put a clip after it and that ring becomes hard-clipping distortion,
// leave the clip before it and the ring escapes to the converter.
// Neither is necessary once nothing follows the limiter.
//
// clip2 is then a backstop that cannot engage while the limiter is
// active, and only does anything via limiter_bypass.

(
SynthDef('sonic-pi-mixer', {|
  in_bus = 0,
  pre_amp = 1, pre_amp_slide = 0.02, pre_amp_slide_shape = 1, pre_amp_slide_curve = 0,
  amp = 1, amp_slide = 0.02, amp_slide_shape = 1, amp_slide_curve = 0,
  hpf = 22, hpf_bypass = 0, hpf_slide = 0.02, hpf_slide_shape = 1, hpf_slide_curve = 0,
  lpf = 136, lpf_bypass = 0, lpf_slide = 0.02, lpf_slide_shape = 1, lpf_slide_curve = 0,
  force_mono = 0, invert_stereo = 0, limiter_bypass = 0, leak_dc_bypass = 0,
  out_bus = 0, pre_limiter_scope_num = 31, pre_limiter_max_frames = 1024|

  var l, r, mono, mono_l, mono_r, pre_amp_v, amp_v, hpf_freq, lpf_freq, limited;

  pre_amp_v = VarLag.kr(pre_amp, pre_amp_slide, pre_amp_slide_curve, pre_amp_slide_shape);
  // Clamped because amp is applied after the limiter: a value above unity
  // there would scale the limited signal back over the ceiling, which is
  // the one thing the chain must not allow.
  amp_v = Clip.kr(VarLag.kr(amp, amp_slide, amp_slide_curve, amp_slide_shape), 0, 1);
  hpf_freq = VarLag.kr(hpf, hpf_slide, hpf_slide_curve, hpf_slide_shape).midicps;
  lpf_freq = Clip.kr(VarLag.kr(lpf, lpf_slide, lpf_slide_curve, lpf_slide_shape), 0, 135.5).midicps;

  // Merge whatever was written directly to the hardware bus with the mix bus
  // (ReplaceOut below clobbers the hardware bus).
  l = Sanitize.ar(In.ar(out_bus) + In.ar(in_bus)) * pre_amp_v;
  r = Sanitize.ar(In.ar(out_bus + 1) + In.ar(in_bus + 1)) * pre_amp_v;

  l = Select.ar(hpf_bypass, [HPF.ar(l, hpf_freq), l]);
  r = Select.ar(hpf_bypass, [HPF.ar(r, hpf_freq), r]);
  l = Select.ar(lpf_bypass, [LPF.ar(l, lpf_freq), l]);
  r = Select.ar(lpf_bypass, [LPF.ar(r, lpf_freq), r]);

  mono = (l + r) / 2;
  mono_l = Select.ar(force_mono, [l, mono]);
  mono_r = Select.ar(force_mono, [r, mono]);

  l = Select.ar(invert_stereo, [mono_l, mono_r]);
  r = Select.ar(invert_stereo, [mono_r, mono_l]);

  l = Select.ar(leak_dc_bypass, [LeakDC.ar(l), l]);
  r = Select.ar(leak_dc_bypass, [LeakDC.ar(r), r]);

  // The limiter's input. Volume is applied after the limiter, so this
  // reading is independent of it: it shows how hard the mix is hitting the
  // ceiling, which is what drive needs to display.
  ScopeOut2.ar([l, r], pre_limiter_scope_num, pre_limiter_max_frames);

  l = LPF.ar(HPF.ar(l, 10), 20500);
  r = LPF.ar(HPF.ar(r, 10), 20500);

  // Stereo-linked: one gain for both channels, so limiting cannot move
  // the image.
  limited = SPLimiter2.ar(l, r, 0.99, 0.0015, 0.05);
  l = Select.ar(limiter_bypass, [limited[0], l]);
  r = Select.ar(limiter_bypass, [limited[1], r]);

  // Volume last, and only ever downwards.
  l = l * amp_v;
  r = r * amp_v;

  // Unreachable while the limiter is active, since amp cannot exceed unity.
  // Kept for the limiter_bypass path.
  l = l.clip2(1);
  r = r.clip2(1);

  ReplaceOut.ar(out_bus, [l, r]);
}).writeDefFile("../../compiled/".resolveRelative);
)
