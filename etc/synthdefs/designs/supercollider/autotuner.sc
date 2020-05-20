//PitchShiftPA is based on formant preserving pitch-synchronous overlap-add re-synthesis, as developed by Keith Lent
//based on real-time implementation by Juan Pampin, combined with non-real-time implementation by Joseph Anderson
//This synthdef is based on the pseudo-UGen by Marcin Pączkowski, using GrainBuf and a circular buffer at https://github.com/dyfer/PitchShiftPA

(
SynthDef('sonic-pi-fx_autotuner', {|
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
    note = 0,
    note_slide=0,
    note_slide_shape=1,
    note_slide_curve=0,
    formant_ratio = 1,
    formant_ratio_slide = 0,
    formant_ratio_slide_shape = 1,
    formant_ratio_slide_curve = 0,
    min_freq = 10,
    max_formant_ratio = 10,
    grains_period = 2,
    transpose=0,
    time_dispersion|

    var pitch_ratio,
    in,
    localbuf,
    grainDur,
    wavePeriod,
    trigger,
    freqPhase,
    maxdelaytime,
    grainFreq,
    bufSize,
    delayWritePhase,
    grainPos,
    snd,
    freq,
    freqAsMidi,
    quantizedMidi,
    harmonize,
    midiDiff,
    absolutelyMinValue,
    numChannels,
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

    absolutelyMinValue = 0.01; // used to ensure positive values before reciprocating
    numChannels = 1;

    // I'm not sure it makes much sense to process each channel separately
    in = Mix([dryL, dryR]).asArray.wrapExtend(numChannels);
    //in = [dryL, dryR].asArray.wrapExtend(numChannels);

    // note represents a midi "target" pitch to tune to
    // without that arg, it works as a normal autotune, locking the
    // input to the nearest midi note
    note = note.varlag(note_slide, note_slide_curve, note_slide_shape);

    formant_ratio = formant_ratio.varlag(formant_ratio_slide, formant_ratio_slide_curve, formant_ratio_slide_shape);

    freq = Pitch.kr(in)[0].asArray.wrapExtend(numChannels);
    freqAsMidi = freq.asArray[0].cpsmidi;
    // "quantize" (round) pitch information
    quantizedMidi = freq.asArray[0].cpsmidi.softRound(1, 0, 1); //quantize to integers, therefore semitones

    // using an if statement caused performance issues for the next line
    midiDiff = Select.kr(note.clip(0, 1), [(quantizedMidi - freqAsMidi), (note - freqAsMidi)]);

    //optionally harmonize
    harmonize = [transpose].midiratio; //single

    // This doesn't currently work without making sure the number of channels matches
    // harmonize = [0, 4].midiratio; //major thirds

    pitch_ratio = midiDiff.midiratio * harmonize;

    //multichanel expansion
    [pitch_ratio, formant_ratio].do({ arg item;
        item.isKindOf(Collection).if({ numChannels = max(numChannels, item.size) });
    });

    pitch_ratio = pitch_ratio.asArray.wrapExtend(numChannels);

    min_freq = min_freq.max(absolutelyMinValue);
    maxdelaytime = min_freq.reciprocal;

    freq = freq.max(min_freq);

    wavePeriod = freq.reciprocal;
    grainDur = grains_period * wavePeriod;
    grainFreq = freq * pitch_ratio;

    if(formant_ratio.notNil, { //regular version

        formant_ratio = formant_ratio.asArray.wrapExtend(numChannels);

        max_formant_ratio = max_formant_ratio.max(absolutelyMinValue);
        formant_ratio = formant_ratio.clip(max_formant_ratio.reciprocal, max_formant_ratio);

        bufSize = ((SampleRate.ir * maxdelaytime * max_formant_ratio) + (SampleRate.ir * ControlDur.ir)).roundUp; //extra padding for maximum delay time
        freqPhase = LFSaw.ar(freq, 1).range(0, wavePeriod) + ((formant_ratio.max(1) - 1) * grainDur);//phasor offset for formant shift up - in seconds; positive here since phasor is subtracted from the delayWritePhase

    }, { //slightly lighter version, without formant manipulation

        formant_ratio = 1 ! numChannels;

        bufSize = ((SampleRate.ir * maxdelaytime) + (SampleRate.ir * ControlDur.ir)).roundUp; //extra padding for maximum delay time
        freqPhase = LFSaw.ar(freq, 1).range(0, wavePeriod);
    });

    localbuf = numChannels.collect({LocalBuf(bufSize, 1).clear});
    delayWritePhase = numChannels.collect({|ch| BufWr.ar(in[ch], localbuf[ch], Phasor.ar(0, 1, 0, BufFrames.kr(localbuf[ch])))});
    grainPos = (delayWritePhase / BufFrames.kr(localbuf)) - (freqPhase / BufDur.kr(localbuf)); //scaled to 0-1 for use in GrainBuf
    if(time_dispersion.isNil, {
        trigger = Impulse.ar(grainFreq);
    }, {
        trigger = Impulse.ar(grainFreq + (LFNoise0.kr(grainFreq) * time_dispersion));
    });
    // # wetL, wetR = numChannels.collect({|ch| GrainBuf.ar(1, trigger[ch], grainDur[ch], localbuf[ch], formant_ratio[ch], grainPos[ch])});
    wetL = numChannels.collect({|ch| GrainBuf.ar(1, trigger[ch], grainDur[ch], localbuf[ch], formant_ratio[ch], grainPos[ch])});
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
