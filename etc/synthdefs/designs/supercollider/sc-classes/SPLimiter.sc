// Class definitions for the SPLimiter UGens, which live in SuperSonic
// (app/external/supersonic/src/synth/plugins/SPLimiterUGen.cpp).
//
// sclang needs these to write a synthdef that references the UGens; they
// carry no DSP themselves. Copy this file into SuperCollider's Extensions
// directory (Platform.userExtensionDir) before compiling any synthdef
// under etc/synthdefs/designs/supercollider/ that uses them.
//
// Shared arguments:
//   level: ceiling, as a linear amplitude
//   lookahead: seconds; also exactly the latency the UGen adds, unlike
//               SuperCollider's Limiter, whose latency is twice its dur.
//               Scalar rate: it sizes an allocation, so it is fixed when
//               the synth is created.
//   release: seconds; recovery time constant for an isolated
//               transient. Recovery slows to 8x this when reduction
//               persists, which is what keeps sustained limiting from
//               modulating the low end.

// Mono.
SPLimiter : UGen {
	*ar {|in, level = 1.0, lookahead = 0.0015, release = 0.05|
		^this.multiNew('audio', in, level, lookahead, release);
	}

	checkInputs { ^this.checkSameRateAsFirstInput }
}

// Stereo with a linked detector: one gain, derived from whichever channel
// needs it most, applied to both. That is what keeps the stereo image
// still while limiting, two independent mono instances shift it on any
// peak that is not symmetric.
//
// Returns [left, right, gain]. The third output is the gain actually
// applied, so a meter can show measured gain reduction rather than
// inferring it from how far the input went over.
SPLimiter2 : MultiOutUGen {
	*ar {|left, right, level = 1.0, lookahead = 0.0015, release = 0.05|
		^this.multiNew('audio', left, right, level, lookahead, release);
	}

	init {|... theInputs|
		inputs = theInputs;
		^this.initOutputs(3, rate);
	}

	checkInputs { ^this.checkSameRateAsFirstInput }
}
