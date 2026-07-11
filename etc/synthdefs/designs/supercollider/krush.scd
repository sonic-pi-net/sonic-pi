// sonic-pi-fx_krush, re-authored in sclang from the original Overtone design
// (designs/overtone/.../fx.clj) with one fix: the waveshaper works on
// abs(input) so its output is unipolar — a large positive DC offset that
// wasted headroom and thumped on start/stop. LeakDC on the wet path strips
// the offset while keeping krush's rectified octave-up character intact.

"./makeFX.scd".loadRelative;

~makeFX.value("sonic-pi-fx_krush", {
  |dry_l, dry_r,
  gain = 5, gain_slide = 0, gain_slide_shape = 1, gain_slide_curve = 0,
  cutoff = 100, cutoff_slide = 0, cutoff_slide_shape = 1, cutoff_slide_curve = 0,
  res = 0, res_slide = 0, res_slide_shape = 1, res_slide_curve = 0|
  var cutoff_freq, abs_l, abs_r, sqr_l, sqr_r, shaped_l, shaped_r, wet_l, wet_r;

  gain = VarLag.kr(gain, gain_slide, gain_slide_curve, gain_slide_shape);
  cutoff = VarLag.kr(cutoff, cutoff_slide, cutoff_slide_curve, cutoff_slide_shape);
  res = res.linlin(0, 1, 1, 0);
  res = VarLag.kr(res, res_slide, res_slide_curve, res_slide_shape);
  cutoff_freq = cutoff.midicps;

  abs_l = dry_l.abs;
  sqr_l = abs_l.squared;
  shaped_l = (sqr_l + (gain * abs_l)) / (sqr_l + (abs_l * (gain - 1)) + 1);

  abs_r = dry_r.abs;
  sqr_r = abs_r.squared;
  shaped_r = (sqr_r + (gain * abs_r)) / (sqr_r + (abs_r * (gain - 1)) + 1);

  wet_l = LeakDC.ar(RLPF.ar(shaped_l, cutoff_freq, res));
  wet_r = LeakDC.ar(RLPF.ar(shaped_r, cutoff_freq, res));
  [wet_l, wet_r];
}, [], 2);
