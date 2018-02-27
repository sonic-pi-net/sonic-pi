var path_to_synthdefs = "/Users/ethan/repos/sonic-pi/etc/synthdefs/compiled/";

s = Server.local;
s.waitForBoot({
  ~makeFX = { |name, func, lags, num_channels = 2|
    SynthDef(name, { |in_bus = 0, out_bus = 0,
      pre_amp = 1, pre_amp_slide = 0, pre_amp_slide_shape = 1, pre_amp_slide_curve = 0,
      amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
      pre_mix = 1, pre_mix_slide = 0, pre_mix_slide_shape = 1, pre_mix_slide_curve = 0,
      mix = 1, mix_slide = 0, mix_slide_shape = 1, mix_slide_curve = 0|

      var fx_arg_in_bus, fx_arg_out_bus, fx_arg_mix, fx_arg_pre_mix, fx_arg_amp, fx_arg_pre_amp,
      fx_arg_in_l, fx_arg_in_r, fx_arg_inv_pre_mix, fx_arg_bypass_l, fx_arg_bypass_r, dry_l, dry_r,
      wet_l, wet_r, fin_l, fin_r;

      fx_arg_in_bus = in_bus;
      fx_arg_out_bus = out_bus;
      fx_arg_mix = Clip.kr(mix);
      fx_arg_mix = VarLag.kr(fx_arg_mix, mix_slide, mix_slide_curve, mix_slide_shape);
      fx_arg_mix = LinLin.kr(fx_arg_mix, 0, 1, -1, 1);

      fx_arg_pre_mix = Clip.kr(pre_mix);
      fx_arg_pre_mix = VarLag.kr(fx_arg_pre_mix, pre_mix_slide, pre_mix_slide_curve, pre_mix_slide_shape);

      fx_arg_amp = VarLag.kr(amp, amp_slide, amp_slide_curve, amp_slide_shape);
      fx_arg_pre_amp = VarLag.kr(pre_amp, pre_amp_slide, pre_amp_slide_curve, pre_amp_slide_shape);

      # fx_arg_in_l, fx_arg_in_r = In.ar(fx_arg_in_bus, 2) * fx_arg_pre_amp;
      fx_arg_inv_pre_mix = 1 - fx_arg_pre_mix;
      fx_arg_bypass_l = fx_arg_in_l * fx_arg_inv_pre_mix;
      fx_arg_bypass_r = fx_arg_in_r * fx_arg_inv_pre_mix;
      dry_l = fx_arg_pre_mix * fx_arg_in_l;
      dry_r = fx_arg_pre_mix * fx_arg_in_r;

      # wet_l, wet_r = SynthDef.wrap(func, lags, [dry_l, dry_r]);
      wet_l = wet_l + fx_arg_bypass_l;
      wet_r = wet_r + fx_arg_bypass_r;
      fin_l = XFade2.ar(fx_arg_in_l, wet_l, fx_arg_mix, fx_arg_amp);
      fin_r = XFade2.ar(fx_arg_in_r, wet_r, fx_arg_mix, fx_arg_amp);
      Out.ar(fx_arg_out_bus, [fin_l, fin_r]);
    }, []).writeDefFile(path_to_synthdefs);
  };

  ~makeFX.value("sonic-pi-fx_echo",
    { |dry_l, dry_r,
      phase = 0.25, phase_slide = 0, phase_slide_shape = 1, phase_slide_curve = 0
      decay = 2, decay_slide = 0, decay_slide_shape = 1, decay_slide_curve = 0
      max_phase = 2|

      phase = VarLag.kr(phase, phase_slide, phase_slide_curve, phase_slide_shape);
      decay = VarLag.kr(decay, decay_slide, decay_slide_curve, decay_slide_shape);
      CombN.ar([dry_l, dry_r], max_phase, phase, decay) + [dry_l, dry_r]
  },[],2);
});
