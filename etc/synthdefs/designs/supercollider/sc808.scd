/*
SuperCollider 808 implementation based on Yoshinosuke Horiuchi's free version. See: https://www.patreon.com/4H/posts
*/

(
SynthDef('sonic-pi-sc808_bassdrum', {|
    note = 34,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    click = 0.11
    decay = 2,
    decay_curve = -7
    attenuation=1,
    out_bus = 0|

    var snd, fenv, env, trienv, sig, sub, punch, pfenv;

    note = note.midicps;
    amp = 2 * amp;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    trienv = EnvGen.kr(Env.new([click, 0.6, 0], [0, decay], -230),doneAction:0);
    fenv = Env([note*7, note*1.35, note], [0.05/attenuation, 0.6], -14).kr; // Mod 0.05 -> 0.5
    pfenv = Env([note*7, note*1.35, note], [0.03/attenuation, 0.6], -10).kr; // Mod 0.03 -> 0.3
    sig = SinOsc.ar(fenv, pi/2) * env;
    sub = LFTri.ar(fenv, pi/2) * trienv * 0.05;
    punch = SinOsc.ar(pfenv, pi/2) * env * 2;
    punch = HPF.ar(punch, 350);
    sig = (sig + sub + punch) * 2.5;
    sig = Limiter.ar(sig, 0.5) * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    sig = Pan2.ar(sig, pan);
    Out.ar(out_bus, sig);

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

SynthDef('sonic-pi-sc808_snare', {|
    note = 65,
    detune = -11,

    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 93, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    lpf = 121, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,
    mix = 0.7,
    head_hpf = 30,
    decay = 4.2,
    decay_curve = -115,
    click = 0.999,
    out_bus = 0|

    var noiseEnv, atkEnv, sig, noise, osc1, osc2, snd, note2;
    mix = Clip.kr(mix, 0, 1);
    click = Clip.kr(0.1 - (click / 10.0), 0, 0.1);
    note2 = note + detune;
    note = note.midicps;
    note2 = note2.midicps;
    lpf = lpf.midicps;
    hpf = hpf.midicps;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
    lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);

    noiseEnv = EnvGen.kr(Env.perc(click, decay, 1, decay_curve), doneAction:2);
    atkEnv = EnvGen.kr(Env.perc(click, 0.8,curve:-95), doneAction:0);

    noise = WhiteNoise.ar;
    noise = HPF.ar(noise, hpf);
    noise = LPF.ar(noise, lpf);
    noise = noise * noiseEnv;
    osc1 = SinOsc.ar(note2, pi/2) * 0.6;
    osc2 = SinOsc.ar(note, pi/2) * 0.7;
    snd = (osc1+osc2) * atkEnv * 2.5;
    snd = (mix * noise) + ((1 - mix) * snd);
    snd = amp * snd * 2;
    snd = HPF.ar(snd, head_hpf);
    DetectSilence.ar(snd, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(snd, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)


(
SynthDef('sonic-pi-sc808_clap', {|
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 71, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    lpf = 84, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,
    click = 0.5,
    decay = 0.3,
    decay_curve = -160
    rev = 1,

    out_bus = 0|

    var atkenv, atk, sig, denv, sum;
    var reverb, revgen;

    lpf = lpf.midicps;
    hpf = hpf.midicps;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
    lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);

    // Use doneAction 0, so the synth is not freed yet
    // The reverb is running much longer
    atkenv = EnvGen.kr(Env.new([click,1,0],[0, decay], decay_curve), doneAction:0);
    denv = EnvGen.kr(Env.dadsr(0.026, 0, 6, 0, 1, 1, curve: decay_curve), doneAction:0);
    atk = WhiteNoise.ar * atkenv * 1.4;
    decay = WhiteNoise.ar * denv;
    sum = atk + decay * amp;
    sum = HPF.ar(sum, hpf);
    sum = BPF.ar(sum, lpf, 0.5);
    sum = 1.5 * sum;

    revgen = EnvGen.kr(Env.perc(0.1, decay+ 4, curve: -9), doneAction:2);
    reverb = WhiteNoise.ar * revgen * 0.02;
    reverb = HPF.ar(reverb, hpf);
    reverb = LPF.ar(reverb, lpf);
    reverb = reverb * amp * rev;
    sum = sum + reverb;
    DetectSilence.ar(sum, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sum, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_tomlo', {|
    note = 40,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay = 20
    click = 0.4
    detune1 = 1.25
    detune2 = 1.125
    decay_curve = -250
    out_bus = 0|

    var sig, fenv, env;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    note = note.midicps;

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    fenv = Env([note*detune1, note*detune2, note], [0.1, 0.5], -4).kr;
    sig = SinOsc.ar(fenv, pi/2) * env;
    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_tommid', {|
    note = 44,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay = 16
    click = 0.4
    detune1 = 1.3333
    detune2 = 1.125
    decay_curve = -250
    out_bus = 0|

    var sig, fenv, env;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    note = note.midicps;

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    fenv = Env([note*detune1, note*detune2, note], [0.1, 0.5], -4).kr;
    sig = SinOsc.ar(fenv, pi/2) * env;
    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_tomhi', {|
    note = 52,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay = 11
    click = 0.4
    detune1 = 1.3333
    detune2 = 1.121212
    decay_curve = -250
    out_bus = 0|

    var sig, fenv, env;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    note = note.midicps;

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    fenv = Env([note*detune1, note*detune2, note], [0.1, 0.5], -4).kr;
    sig = SinOsc.ar(fenv, pi/2) * env;
    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_congalo', {|
    note = 52,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay_curve = -250,
    decay = 18
    detune1 = 1.333333,
    detune2 = 1.121212,
    click = 0.15

    out_bus = 0|

    var sig, fenv, env;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    note = note.midicps;

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    fenv = Env([note*detune1, note*detune2, note], [0.1, 0.5], -4).kr;
    sig = SinOsc.ar(fenv, pi/2) * env;

    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_congamid', {|
    note = 57,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay_curve = -250,
    decay = 18
    detune1 = 1.333333,
    detune2 = 1.121212,
    click = 0.13

    out_bus = 0|

    var sig, fenv, env;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    note = note.midicps;

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    fenv = Env([note*detune1, note*detune2, note], [0.1, 0.5], -4).kr;
    sig = SinOsc.ar(fenv, pi/2) * env;

    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_congahi', {|
    note = 52,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay_curve = -250,
    decay = 18
    detune1 = 1.333333,
    detune2 = 1.121212,
    click = 0.15

    out_bus = 0|

    var sig, fenv, env;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    note = note.midicps;

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    fenv = Env([note*detune1, note*detune2, note], [0.1, 0.5], -4).kr;
    sig = SinOsc.ar(fenv, pi/2) * env;

    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_rimshot', {|
    note = 92,
    detune = -22,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 63, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    lpf = 118, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,
    decay = 0.07,
    decay_curve = -42,
    click = 1,
    out_bus = 0|

    var env, sig, punch, tri1, tri2, note2;

    note2 = note + detune;
    note = note.midicps;
    note2 = note2.midicps;

    lpf = lpf.midicps;
    hpf = hpf.midicps;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
    lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);

    env = EnvGen.kr(Env.new([click, 1, 0], [0.00272, decay], decay_curve), doneAction:2);
    tri1 = LFTri.ar(note * 1.1, 1) * env;
    tri2 = LFPulse.ar(note2 * 1.1, width:0.8) * env;
    punch = WhiteNoise.ar * env * 0.46;
    sig = tri1 + tri2 + punch;
    sig = BPeakEQ.ar(sig, 464, 0.44, 8);
    sig = HPF.ar(sig, hpf);
    sig = LPF.ar(sig, lpf);
    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

SynthDef('sonic-pi-sc808_claves', {|
    note = 99,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay = 0.1,
    decay_curve = -20,
    click = 1,
    out_bus = 0|

    var sig, env;

    note = note.midicps;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve), doneAction:2);
    sig = SinOsc.ar(note, pi/2) * env * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_maracas', {|
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 113, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    click = 0.027,
    decay = 0.07,
    decay_curve = -250

    out_bus = 0|

    var sig, env;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);

    hpf = hpf.midicps;

    env = EnvGen.kr(Env.new([0.3, 1, 0], [click, decay], decay_curve), doneAction:2);
    sig = WhiteNoise.ar * env * amp;
    sig = HPF.ar(sig, hpf);
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");

(
SynthDef('sonic-pi-sc808_cowbell', {|
    note = 79.58979585613574,
    detune = -7.08444657092187
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 59, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    lpf = 109, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,
    decay = 9.5,
    decay_curve = -90,
    out_bus = 0|

    var sig, pul1, pul2, env, atk, atkenv, datk, note2;
    note2 = note + detune;
    note = note.midicps;
    note2 = note2.midicps;
    hpf = hpf.midicps;
    lpf = lpf.midicps;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
    lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);

    atkenv = EnvGen.kr(Env.perc(0, 1, 1, -215),doneAction:0);
    env = EnvGen.kr(Env.perc(0.01, decay, 1, decay_curve),doneAction:2);
    pul1 = LFPulse.ar(note);
    pul2 = LFPulse.ar(note2);
    atk = (pul1 + pul2) * atkenv * 6;
    datk = (pul1 + pul2) * env;
    sig = (atk + datk) * amp / 6;
    sig = HPF.ar(sig, hpf);
    sig = LPF.ar(sig, lpf);
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_closed_hihat', {|
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 121.25219487074914, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    lpf = 121.05875888638981, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,

    decay=0.42,
    decay_curve = -30,
    out_bus = 0|

    var sig, sighi,siglow, sum, env, osc1, osc2, osc3, osc4, osc5, osc6;

    hpf = hpf.midicps;
    lpf = lpf.midicps;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
    lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);

    env = EnvGen.kr(Env.perc(0.005, decay, 1, decay_curve),doneAction:2);
    osc1 = LFPulse.ar(203.52);
    osc2 = LFPulse.ar(366.31);
    osc3 = LFPulse.ar(301.77);
    osc4 = LFPulse.ar(518.19);
    osc5 = LFPulse.ar(811.16);
    osc6 = LFPulse.ar(538.75);
    sighi = (osc1 + osc2 + osc3 + osc4 + osc5 + osc6);
    siglow = (osc1 + osc2 + osc3 + osc4 + osc5 + osc6);
    sighi = BPF.ar(sighi, lpf, 1);
    sighi = HPF.ar(sighi, hpf);
    siglow = BBandPass.ar(siglow, 8900, 0.8);
    siglow = BHiPass.ar(siglow, 9000, 0.3);
    sig = BPeakEQ.ar((siglow+sighi), 9700, 0.8, 0.7);
    sig = sig * env * amp * 12;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_open_hihat', {|
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 118.551, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    lpf = 107.213, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,

    decay=0.5,
    decay_curve = -3,
    out_bus = 0|

    var sig, siga, sigb, env1, env2, osc1, osc2, osc3, osc4, osc5, osc6, sum;

    hpf = hpf.midicps;
    lpf = lpf.midicps;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
    lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);

    env1 = EnvGen.kr(Env.perc(0.1, decay, curve: decay_curve), doneAction:2);
    env2 = EnvGen.kr(Env.new([0, 1, 0], [0, decay*5], curve:-150), doneAction:0);
    osc1 = LFPulse.ar(203.52) * 0.6;
    osc2 = LFPulse.ar(366.31) * 0.6;
    osc3 = LFPulse.ar(301.77) * 0.6;
    osc4 = LFPulse.ar(518.19) * 0.6;
    osc5 = LFPulse.ar(811.16) * 0.6;
    osc6 = LFPulse.ar(538.75) * 0.6;
    sig = osc1 + osc2 + osc3 + osc4 + osc5 +osc6;
    sig = BLowShelf.ar(sig, 990, 2, -3);
    sig = BPF.ar(sig, hpf);
    sig = BPeakEQ.ar(sig, 7200, 0.5, 5);
    sig = BHiPass4.ar(sig, 8100, 0.7);
    sig = BHiShelf.ar(sig, 9400, 1, 5);
    siga = sig * env1 * 0.6;
    sigb = sig * env2;
    sum = siga + sigb;
    sum = LPF.ar(sum, lpf);
    sum = sum * amp * 7;

    DetectSilence.ar(sum, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sum, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_cymbal', {|
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,

    decay=2,
    decay_curve = -3
    tone=0.25,

    out_bus = 0|

    var sig, sig1, sig2, sig2a, sig2b, sig3, env1, env2, env2b, env3, osc1, osc2, osc3, osc4, osc5, osc6, sum;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    tone = tone * 0.008;
    env1 = EnvGen.kr(Env.perc(0.3, decay, curve:decay_curve), doneAction:2);
    env2 = EnvGen.kr(Env.new([0, 0.6, 0], [0.1, decay*0.7], -5), doneAction:0);
    env2b = EnvGen.kr(Env.new([0, 0.3, 0], [0.1, decay*20], -120), doneAction:0);
    env3 = EnvGen.kr(Env.new([0, 1, 0], [0, decay*5], curve:-150), doneAction:0);
    osc1 = LFPulse.ar(203.52) * 0.6;
    osc2 = LFPulse.ar(366.31) * 0.6;
    osc3 = LFPulse.ar(301.77) * 0.6;
    osc4 = LFPulse.ar(518.19) * 0.6;
    osc5 = LFPulse.ar(811.16) * 0.6;
    osc6 = LFPulse.ar(538.75) * 0.6;
    sig = osc1 + osc2 + osc3 + osc4 + osc5 +osc6;
    sig1 = BLowShelf.ar(sig, 2000, 1, 5);
    sig1 = BPF.ar(sig1, 3000);
    sig1 = BPeakEQ.ar(sig1, 2400, 0.5, 5);
    sig1 = BHiPass.ar(sig1, 1550, 0.7);
    sig1 = LPF.ar(sig1, 3000);
    sig1 = BLowShelf.ar(sig1, 1000, 1, 0);
    sig1 = sig1 * env1 * tone;
    sig2 = BLowShelf.ar(sig, 990, 2, -5);
    sig2 = BPF.ar(sig2, 7400);
    sig2 = BPeakEQ.ar(sig2, 7200, 0.5, 5);
    sig2 = BHiPass4.ar(sig2, 6800, 0.7);
    sig2 = BHiShelf.ar(sig2, 10000, 1, -4);
    sig2a = sig2 * env2 * 0.3;
    sig2b = sig2 * env2b * 0.6;
    sig3 = BLowShelf.ar(sig, 990, 2, -15);
    sig3 = BPF.ar(sig3, 6500);
    sig3 = BPeakEQ.ar(sig3, 7400, 0.35, 10);
    sig3 = BHiPass4.ar(sig3, 10500, 0.8, 2);
    sig3 = sig3 * env3;
    sum = sig1 + sig2a + sig2b + sig3;
    sum = LPF.ar(sum, 4000);
    sum = sum * amp * 12;
    DetectSilence.ar(sum, doneAction: Done.freeSelf);
    Out.ar(0, Pan2.ar(sum, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)/*
SuperCollider 808 implementation based on Yoshinosuke Horiuchi's free version. See: https://www.patreon.com/4H/posts
*/

(
SynthDef('sonic-pi-sc808_bassdrum', {|
    note = 34,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    click = 0.11
    decay = 2,
    decay_curve = -7
    attenuation=1,
    out_bus = 0|

    var snd, fenv, env, trienv, sig, sub, punch, pfenv;

    note = note.midicps;
    amp = 2 * amp;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);


    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    trienv = EnvGen.kr(Env.new([click, 0.6, 0], [0, decay], -230),doneAction:0);
    fenv = Env([note*7, note*1.35, note], [0.05/attenuation, 0.6], -14).kr; // Mod 0.05 -> 0.5
    pfenv = Env([note*7, note*1.35, note], [0.03/attenuation, 0.6], -10).kr; // Mod 0.03 -> 0.3
    sig = SinOsc.ar(fenv, pi/2) * env;
    sub = LFTri.ar(fenv, pi/2) * trienv * 0.05;
    punch = SinOsc.ar(pfenv, pi/2) * env * 2;
    punch = HPF.ar(punch, 350);
    sig = (sig + sub + punch) * 2.5;
    sig = Limiter.ar(sig, 0.5) * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    sig = Pan2.ar(sig, pan);
    Out.ar(out_bus, sig);

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

SynthDef('sonic-pi-sc808_snare', {|
    note = 65,
    detune = -11,

    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 93, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    lpf = 121, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,
    mix = 0.7,
    head_hpf = 30,
    decay = 4.2,
    decay_curve = -115,
    click = 0.999,
    out_bus = 0|

    var noiseEnv, atkEnv, sig, noise, osc1, osc2, snd, note2;
    mix = Clip.kr(mix, 0, 1);
    click = Clip.kr(0.1 - (click / 10.0), 0, 0.1);
    note2 = note + detune;
    note = note.midicps;
    note2 = note2.midicps;
    lpf = lpf.midicps;
    hpf = hpf.midicps;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
    lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);

    noiseEnv = EnvGen.kr(Env.perc(click, decay, 1, decay_curve), doneAction:2);
    atkEnv = EnvGen.kr(Env.perc(click, 0.8,curve:-95), doneAction:0);

    noise = WhiteNoise.ar;
    noise = HPF.ar(noise, hpf);
    noise = LPF.ar(noise, lpf);
    noise = noise * noiseEnv;
    osc1 = SinOsc.ar(note2, pi/2) * 0.6;
    osc2 = SinOsc.ar(note, pi/2) * 0.7;
    snd = (osc1+osc2) * atkEnv * 2.5;
    snd = (mix * noise) + ((1 - mix) * snd);
    snd = amp * snd * 2;
    snd = HPF.ar(snd, head_hpf);
    DetectSilence.ar(snd, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(snd, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)


(
SynthDef('sonic-pi-sc808_clap', {|
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 71, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    lpf = 84, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,
    click = 0.5,
    decay = 0.3,
    decay_curve = -160
    rev = 1,

    out_bus = 0|

    var atkenv, atk, sig, denv, sum;
    var reverb, revgen;

    lpf = lpf.midicps;
    hpf = hpf.midicps;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
    lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);


    // Use doneAction 0, so the synth is not freed yet
    // The reverb is running much longer
    atkenv = EnvGen.kr(Env.new([click,1,0],[0, decay], decay_curve), doneAction:0);
    denv = EnvGen.kr(Env.dadsr(0.026, 0, 6, 0, 1, 1, curve: decay_curve), doneAction:0);
    atk = WhiteNoise.ar * atkenv * 1.4;
    decay = WhiteNoise.ar * denv;
    sum = atk + decay * amp;
    sum = HPF.ar(sum, hpf);
    sum = BPF.ar(sum, lpf, 0.5);
    sum = 1.5 * sum;

    revgen = EnvGen.kr(Env.perc(0.1, decay+ 4, curve: -9), doneAction:2);
    reverb = WhiteNoise.ar * revgen * 0.02;
    reverb = HPF.ar(reverb, hpf);
    reverb = LPF.ar(reverb, lpf);
    reverb = reverb * amp * rev;
    sum = sum + reverb;
    DetectSilence.ar(sum, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sum, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_tomlo', {|
    note = 40,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay = 20
    click = 0.4
    detune1 = 1.25
    detune2 = 1.125
    decay_curve = -250
    out_bus = 0|

    var sig, fenv, env;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    note = note.midicps;

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    fenv = Env([note*detune1, note*detune2, note], [0.1, 0.5], -4).kr;
    sig = SinOsc.ar(fenv, pi/2) * env;
    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_tommid', {|
    note = 44,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay = 16
    click = 0.4
    detune1 = 1.3333
    detune2 = 1.125
    decay_curve = -250
    out_bus = 0|

    var sig, fenv, env;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    note = note.midicps;

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    fenv = Env([note*detune1, note*detune2, note], [0.1, 0.5], -4).kr;
    sig = SinOsc.ar(fenv, pi/2) * env;
    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_tomhi', {|
    note = 52,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay = 11
    click = 0.4
    detune1 = 1.3333
    detune2 = 1.121212
    decay_curve = -250
    out_bus = 0|

    var sig, fenv, env;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    note = note.midicps;

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    fenv = Env([note*detune1, note*detune2, note], [0.1, 0.5], -4).kr;
    sig = SinOsc.ar(fenv, pi/2) * env;
    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_congalo', {|
    note = 52,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay_curve = -250,
    decay = 18
    detune1 = 1.333333,
    detune2 = 1.121212,
    click = 0.15

    out_bus = 0|

    var sig, fenv, env;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    note = note.midicps;

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    fenv = Env([note*detune1, note*detune2, note], [0.1, 0.5], -4).kr;
    sig = SinOsc.ar(fenv, pi/2) * env;

    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_congamid', {|
    note = 57,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay_curve = -250,
    decay = 18
    detune1 = 1.333333,
    detune2 = 1.121212,
    click = 0.13

    out_bus = 0|

    var sig, fenv, env;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    note = note.midicps;

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    fenv = Env([note*detune1, note*detune2, note], [0.1, 0.5], -4).kr;
    sig = SinOsc.ar(fenv, pi/2) * env;

    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_congahi', {|
    note = 52,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay_curve = -250,
    decay = 18
    detune1 = 1.333333,
    detune2 = 1.121212,
    click = 0.15

    out_bus = 0|

    var sig, fenv, env;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    note = note.midicps;

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve),doneAction:2);
    fenv = Env([note*detune1, note*detune2, note], [0.1, 0.5], -4).kr;
    sig = SinOsc.ar(fenv, pi/2) * env;

    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_rimshot', {|
    note = 92,
    detune = -22,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 63, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    lpf = 118, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,
    decay = 0.07,
    decay_curve = -42,
    click = 1,
    out_bus = 0|

    var env, sig, punch, tri1, tri2, note2;

    note2 = note + detune;
    note = note.midicps;
    note2 = note2.midicps;

    lpf = lpf.midicps;
    hpf = hpf.midicps;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
    lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);

    env = EnvGen.kr(Env.new([click, 1, 0], [0.00272, decay], decay_curve), doneAction:2);
    tri1 = LFTri.ar(note * 1.1, 1) * env;
    tri2 = LFPulse.ar(note2 * 1.1, width:0.8) * env;
    punch = WhiteNoise.ar * env * 0.46;
    sig = tri1 + tri2 + punch;
    sig = BPeakEQ.ar(sig, 464, 0.44, 8);
    sig = HPF.ar(sig, hpf);
    sig = LPF.ar(sig, lpf);
    sig = sig * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

SynthDef('sonic-pi-sc808_claves', {|


    note = 99,
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    decay = 0.1,
    decay_curve = -20,
    click = 1,


    out_bus = 0|

    var sig, env;

    note = note.midicps;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

    env = EnvGen.kr(Env.new([click, 1, 0], [0, decay], decay_curve), doneAction:2);
    sig = SinOsc.ar(note, pi/2) * env * amp;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");

SynthDef('sonic-pi-sc808_maracas', {|
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 113, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    click = 0.027,
    decay = 0.07,
    decay_curve = -250

    out_bus = 0|

    var sig, env;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);

    hpf = hpf.midicps;

    env = EnvGen.kr(Env.new([0.3, 1, 0], [click, decay], decay_curve), doneAction:2);
    sig = WhiteNoise.ar * env * amp;
    sig = HPF.ar(sig, hpf);
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");

(
SynthDef('sonic-pi-sc808_cowbell', {|
    note = 79.58979585613574,
    detune = -7.08444657092187
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 59, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    lpf = 109, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,
    decay = 9.5,
    decay_curve = -90,
    out_bus = 0|

    var sig, pul1, pul2, env, atk, atkenv, datk, note2;
    note2 = note + detune;
    note = note.midicps;
    note2 = note2.midicps;
    hpf = hpf.midicps;
    lpf = lpf.midicps;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
    lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);

    atkenv = EnvGen.kr(Env.perc(0, 1, 1, -215),doneAction:0);
    env = EnvGen.kr(Env.perc(0.01, decay, 1, decay_curve),doneAction:2);
    pul1 = LFPulse.ar(note);
    pul2 = LFPulse.ar(note2);
    atk = (pul1 + pul2) * atkenv * 6;
    datk = (pul1 + pul2) * env;
    sig = (atk + datk) * amp / 6;
    sig = HPF.ar(sig, hpf);
    sig = LPF.ar(sig, lpf);
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_closed_hihat', {|
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 121.25219487074914, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    lpf = 121.05875888638981, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,

    decay=0.42,
    decay_curve = -30,
    out_bus = 0|

    var sig, sighi,siglow, sum, env, osc1, osc2, osc3, osc4, osc5, osc6;

    hpf = hpf.midicps;
    lpf = lpf.midicps;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);

    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
    lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);

    env = EnvGen.kr(Env.perc(0.005, decay, 1, decay_curve),doneAction:2);
    osc1 = LFPulse.ar(203.52);
    osc2 = LFPulse.ar(366.31);
    osc3 = LFPulse.ar(301.77);
    osc4 = LFPulse.ar(518.19);
    osc5 = LFPulse.ar(811.16);
    osc6 = LFPulse.ar(538.75);
    sighi = (osc1 + osc2 + osc3 + osc4 + osc5 + osc6);
    siglow = (osc1 + osc2 + osc3 + osc4 + osc5 + osc6);
    sighi = BPF.ar(sighi, lpf, 1);
    sighi = HPF.ar(sighi, hpf);
    siglow = BBandPass.ar(siglow, 8900, 0.8);
    siglow = BHiPass.ar(siglow, 9000, 0.3);
    sig = BPeakEQ.ar((siglow+sighi), 9700, 0.8, 0.7);
    sig = sig * env * amp * 12;
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sig, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_open_hihat', {|
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,
    hpf = 118.551, hpf_slide = 0, hpf_slide_shape = 1, hpf_slide_curve = 1,
    lpf = 107.213, lpf_slide = 0, lpf_slide_shape = 1, lpf_slide_curve = 1,

    decay=0.5,
    decay_curve = -3,
    out_bus = 0|

    var sig, siga, sigb, env1, env2, osc1, osc2, osc3, osc4, osc5, osc6, sum;

    hpf = hpf.midicps;
    lpf = lpf.midicps;
    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    hpf = hpf.varlag(hpf_slide, hpf_slide_curve, hpf_slide_shape);
    lpf = lpf.varlag(lpf_slide, lpf_slide_curve, lpf_slide_shape);

    env1 = EnvGen.kr(Env.perc(0.1, decay, curve: decay_curve), doneAction:2);
    env2 = EnvGen.kr(Env.new([0, 1, 0], [0, decay*5], curve:-150), doneAction:0);
    osc1 = LFPulse.ar(203.52) * 0.6;
    osc2 = LFPulse.ar(366.31) * 0.6;
    osc3 = LFPulse.ar(301.77) * 0.6;
    osc4 = LFPulse.ar(518.19) * 0.6;
    osc5 = LFPulse.ar(811.16) * 0.6;
    osc6 = LFPulse.ar(538.75) * 0.6;
    sig = osc1 + osc2 + osc3 + osc4 + osc5 +osc6;
    sig = BLowShelf.ar(sig, 990, 2, -3);
    sig = BPF.ar(sig, hpf);
    sig = BPeakEQ.ar(sig, 7200, 0.5, 5);
    sig = BHiPass4.ar(sig, 8100, 0.7);
    sig = BHiShelf.ar(sig, 9400, 1, 5);
    siga = sig * env1 * 0.6;
    sigb = sig * env2;
    sum = siga + sigb;
    sum = LPF.ar(sum, lpf);
    sum = sum * amp * 7;

    DetectSilence.ar(sum, doneAction: Done.freeSelf);
    Out.ar(out_bus, Pan2.ar(sum, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)

(
SynthDef('sonic-pi-sc808_cymbal', {|
    amp = 1, amp_slide = 0, amp_slide_shape = 1, amp_slide_curve = 0,
    pan = 0, pan_slide = 0, pan_slide_shape = 1, pan_slide_curve = 0,

    decay=2,
    decay_curve = -3
    tone=0.25,

    out_bus = 0|

    var sig, sig1, sig2, sig2a, sig2b, sig3, env1, env2, env2b, env3, osc1, osc2, osc3, osc4, osc5, osc6, sum;

    amp = amp.varlag(amp_slide, amp_slide_curve, amp_slide_shape);
    pan = pan.varlag(pan_slide, pan_slide_curve, pan_slide_shape);
    tone = tone * 0.008;
    env1 = EnvGen.kr(Env.perc(0.3, decay, curve:decay_curve), doneAction:2);
    env2 = EnvGen.kr(Env.new([0, 0.6, 0], [0.1, decay*0.7], -5), doneAction:0);
    env2b = EnvGen.kr(Env.new([0, 0.3, 0], [0.1, decay*20], -120), doneAction:0);
    env3 = EnvGen.kr(Env.new([0, 1, 0], [0, decay*5], curve:-150), doneAction:0);
    osc1 = LFPulse.ar(203.52) * 0.6;
    osc2 = LFPulse.ar(366.31) * 0.6;
    osc3 = LFPulse.ar(301.77) * 0.6;
    osc4 = LFPulse.ar(518.19) * 0.6;
    osc5 = LFPulse.ar(811.16) * 0.6;
    osc6 = LFPulse.ar(538.75) * 0.6;
    sig = osc1 + osc2 + osc3 + osc4 + osc5 +osc6;
    sig1 = BLowShelf.ar(sig, 2000, 1, 5);
    sig1 = BPF.ar(sig1, 3000);
    sig1 = BPeakEQ.ar(sig1, 2400, 0.5, 5);
    sig1 = BHiPass.ar(sig1, 1550, 0.7);
    sig1 = LPF.ar(sig1, 3000);
    sig1 = BLowShelf.ar(sig1, 1000, 1, 0);
    sig1 = sig1 * env1 * tone;
    sig2 = BLowShelf.ar(sig, 990, 2, -5);
    sig2 = BPF.ar(sig2, 7400);
    sig2 = BPeakEQ.ar(sig2, 7200, 0.5, 5);
    sig2 = BHiPass4.ar(sig2, 6800, 0.7);
    sig2 = BHiShelf.ar(sig2, 10000, 1, -4);
    sig2a = sig2 * env2 * 0.3;
    sig2b = sig2 * env2b * 0.6;
    sig3 = BLowShelf.ar(sig, 990, 2, -15);
    sig3 = BPF.ar(sig3, 6500);
    sig3 = BPeakEQ.ar(sig3, 7400, 0.35, 10);
    sig3 = BHiPass4.ar(sig3, 10500, 0.8, 2);
    sig3 = sig3 * env3;
    sum = sig1 + sig2a + sig2b + sig3;
    sum = LPF.ar(sum, 4000);
    sum = sum * amp * 12;
    DetectSilence.ar(sum, doneAction: Done.freeSelf);
    Out.ar(0, Pan2.ar(sum, pan));

}).writeDefFile("/Users/sam/Development/sonic-pi/etc/synthdefs/compiled/");
)