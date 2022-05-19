// Autotune style plugin based on the orignal Antares patent (US Patent US5973252 - now expired). Uses Cycfi's Bitstream Autocorrelation (BACF) for pitch tracking.
// See https://github.com/xavriley/qlibugens for details of the SC UGens

(
SynthDef('sonic-pi-fx_autotuner_two', {|
    //standard args
    pre_amp=1,
    pre_amp_slide=0,
    pre_amp_slide_shape=1,
    pre_amp_slide_curve=0,
    amp=1,
    amp_slide=0,
    amp_slide_shape=1,
    amp_slide_curve=0,
    mix=1,
    mix_slide=0,
    mix_slide_shape=1,
    mix_slide_curve=0,
    pre_mix=1,
    pre_mix_slide=0,
    pre_mix_slide_shape=1,
    pre_mix_slide_curve=0
    out_bus=0,
    in_bus=0,
    // args specific to this synth
    note = -1,
    note_slide=0,
    note_slide_shape=1,
    note_slide_curve=0,
    min_freq = 50,
    transpose=0,
    key=0,
    scale=0|

    var pitch_ratio,
    in,
    snd,
    freq,
    // args for Sonic Pi plumbing
    fxArgMix,
    fxArgPreMix,
    fxArgAmp,
    fxArgPreAmp,
    inLeft,
    inRight,
    fxArgInvPreMix,
    fxArgBypassL,
    fxArgBypassR,
    dryL,
    dryR,
    wetL,
    wetR,
    finL,
    finR;

    // plumbing for Sonic Pi standard args
    fxArgMix = VarLag.kr(mix.clip(0,1), mix_slide, mix_slide_curve, mix_slide_shape);
    fxArgMix = LinLin.kr(fxArgMix, 0, 1, -1, 1);

    fxArgPreMix = VarLag.kr(pre_mix.clip(0,1), pre_mix_slide, pre_mix_slide_curve, pre_mix_slide_shape);

    fxArgAmp = VarLag.kr(amp, amp_slide, amp_slide_curve, amp_slide_shape);
    fxArgPreAmp = VarLag.kr(pre_amp, pre_amp_slide, pre_amp_slide_curve, pre_amp_slide_shape);

    # inLeft, inRight = In.ar(in_bus, 2) * fxArgPreAmp;
    fxArgInvPreMix = 1 - fxArgPreMix;

    fxArgBypassL = fxArgInvPreMix * inLeft;
    fxArgBypassR = fxArgInvPreMix * inRight;

    dryL = fxArgPreMix * inLeft;
    dryR = fxArgPreMix * inRight;

    // note represents a midi "target" pitch to tune to
    // without that arg, it works as a normal autotune, locking the
    // input to the nearest midi note
    note = note.varlag(note_slide, note_slide_curve, note_slide_shape);

    wetL = BitstreamPitchCorrection.ar(inLeft, min_freq, transpose: transpose, key: key, scale: scale, target_note: note);
    wetR = wetL;

    // plumbing for Sonic Pi output
    wetL = wetL + fxArgBypassL;
    wetR = wetR + fxArgBypassR;

    finL = XFade2.ar(inLeft, wetL, fxArgMix, fxArgAmp);
    finR = XFade2.ar(inRight, wetR, fxArgMix, fxArgAmp);

    Out.ar(out_bus, [finL, finR]);
}
).writeDefFile("/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/compiled/")
)
