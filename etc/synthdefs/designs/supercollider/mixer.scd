// sonic-pi-mixer, re-authored in sclang from the original Overtone design
// with three fixes over the original:
//  - LeakDC runs BEFORE the limiter (it ran after), so DC riding on the mix
//    no longer eats limiter headroom or turns into low-frequency junk via the
//    limiter's gain-riding.
//  - amp (the main volume) is applied BEFORE the limiter (it was after), so
//    the master volume can no longer hard-clip the output: cranking it drives
//    into the limiter instead. The final clip2 remains only as a mathematical
//    guarantee (reachable only via limiter_bypass).
//  - Sanitize at the input replaces NaN/Inf from broken user synths with
//    silence instead of passing them to the hardware.
//
// Chain: merge hardware bus + mix bus -> Sanitize -> pre_amp -> switchable
// HPF/LPF -> force_mono -> invert_stereo -> amp -> LeakDC -> pre-limiter scope
// tap -> Limiter -> hard clip at +-1 -> fixed 10Hz/20.5kHz safety filters ->
// ReplaceOut.

(
SynthDef('sonic-pi-mixer', {|
  in_bus = 0,
  pre_amp = 1, pre_amp_slide = 0.02, pre_amp_slide_shape = 1, pre_amp_slide_curve = 0,
  amp = 1, amp_slide = 0.02, amp_slide_shape = 1, amp_slide_curve = 0,
  hpf = 22, hpf_bypass = 0, hpf_slide = 0.02, hpf_slide_shape = 1, hpf_slide_curve = 0,
  lpf = 136, lpf_bypass = 0, lpf_slide = 0.02, lpf_slide_shape = 1, lpf_slide_curve = 0,
  force_mono = 0, invert_stereo = 0, limiter_bypass = 0, leak_dc_bypass = 0,
  out_bus = 0, pre_limiter_scope_num = 31, pre_limiter_max_frames = 1024|

  var l, r, mono, mono_l, mono_r, pre_amp_v, amp_v, hpf_freq, lpf_freq;

  pre_amp_v = VarLag.kr(pre_amp, pre_amp_slide, pre_amp_slide_curve, pre_amp_slide_shape);
  amp_v = VarLag.kr(amp, amp_slide, amp_slide_curve, amp_slide_shape);
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

  l = l * amp_v;
  r = r * amp_v;

  l = Select.ar(leak_dc_bypass, [LeakDC.ar(l), l]);
  r = Select.ar(leak_dc_bypass, [LeakDC.ar(r), r]);

  // The limiter's input, tapped after amp so the reading doesn't move when the
  // main volume does. Against the master output scope (slot 0) the difference
  // is the limiter's gain reduction and nothing else — only clip2 and the
  // fixed safety filters sit between this point and the output.
  ScopeOut2.ar([l, r], pre_limiter_scope_num, pre_limiter_max_frames);

  l = Select.ar(limiter_bypass, [Limiter.ar(l, 0.99, 0.01), l]);
  r = Select.ar(limiter_bypass, [Limiter.ar(r, 0.99, 0.01), r]);

  l = l.clip2(1);
  r = r.clip2(1);

  l = LPF.ar(HPF.ar(l, 10), 20500);
  r = LPF.ar(HPF.ar(r, 10), 20500);

  ReplaceOut.ar(out_bus, [l, r]);
}).writeDefFile("../../compiled/".resolveRelative);
)
