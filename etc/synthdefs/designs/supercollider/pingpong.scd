"./makeFX.scd".loadRelative;

~makeFX.value("sonic-pi-fx_pingpong", {
  |dry_l, dry_r,
  phase = 0.25, phase_slide = 0, phase_slide_shape = 1, phase_slide_curve = 0, max_phase = 1,
  feedback = 0.5, feedback_slide = 0, feedback_slide_shape = 1, feedback_slide_curve = 0,
  pan_start = 1|
  var panned;

  phase = VarLag.kr(phase, phase_slide, phase_slide_curve, phase_slide_shape);
  feedback = VarLag.kr(feedback, feedback_slide, feedback_slide_curve, feedback_slide_shape);
  pan_start = pan_start.neg;
  panned = Balance2.ar(dry_l, dry_r, pan_start);
  PingPong.ar(LocalBuf(SampleRate.ir * max_phase, 2).clear, panned, phase, feedback, 1);
}, [], 2);