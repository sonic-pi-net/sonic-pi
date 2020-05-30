"./makeFX.scd".loadRelative;

~makeFX.value("sonic-pi-fx_ping_pong", {
  |dry_l, dry_r,
  phase = 0.25, phase_slide = 0, phase_slide_shape = 1, phase_slide_curve = 0, max_phase = 1,
  feedback = 0.5, feedback_slide = 0, feedback_slide_shape = 1, feedback_slide_curve = 0,
  pan_start = 1|
  var panned;

  phase = VarLag.kr(phase, phase_slide, phase_slide_curve, phase_slide_shape);
  feedback = VarLag.kr(feedback, feedback_slide, feedback_slide_curve, feedback_slide_shape);
  feedback = Clip.kr(feedback);
  pan_start = pan_start.neg;
  pan_start = Clip.kr(pan_start, -1, 1);
  panned = Balance2.ar(dry_l, dry_r, pan_start);
  Mix.new([PingPong.ar(LocalBuf(SampleRate.ir * max_phase, 2).clear, panned, phase, feedback, 1), [dry_l, dry_r]]);
}, [], 2);