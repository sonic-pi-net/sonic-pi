"./makeFX.scd".loadRelative;

~makeFX.value("sonic-pi-fx_vocoder", {
	|dry_l, dry_r|

	var voicedCarrier;
	var replacementSound;
	var snd, numBands, bandFreqs, bandMuls, carrier;
	var perc, chainMod, chainCar, chain, sig;

	numBands = 20;
	bandFreqs = (0..numBands - 1).linexp(0, numBands - 1, 50, 8000);
	bandMuls = [0.6,0.6,1.5,1.5,1.5,1.5,1,1,1,1,1,1,1.5,1.5,1.2,1.2,1.2,1.2,1.2,1.2];

	// modulator (i.e. voice) is taken from the right channel of input
	snd = dry_l;

	// carrier (i.e. a synth) is taken from the left channel of input
	voicedCarrier = dry_r;
	replacementSound = LPF.ar(HPF.ar(WhiteNoise.ar(mul: 1), 3000), 8000);

	// FFT based voiced/unvoiced detection
	chainMod = FFT(LocalBuf(1024), snd);
	// the following line gates the modulator signal
	chainMod = PV_MagAbove(chainMod, 0.2);
	chainCar = FFT(LocalBuf(1024), voicedCarrier);
	chain  = PV_MagMul(chainCar, chainMod);
	// perc = SpecPcile.kr(chain, 0.9).explin(2000, 8000, 0, 1);
	chain = PV_MagClip(chain, 50);

	sig = IFFT(chain);
	Mix.new([[sig, sig], [snd, snd]]);
}, [], 2);