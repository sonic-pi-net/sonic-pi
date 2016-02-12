# FX

* [Bitcrusher](#bitcrusher)
* [krush](#krush)
* [Reverb](#reverb)
* [GVerb](#gverb)
* [Level Amplifier](#level-amplifier)
* [Echo](#echo)
* [Slicer](#slicer)
* [Pan Slicer](#pan-slicer)
* [Wobble](#wobble)
* [Techno from IXI Lang](#techno-from-ixi-lang)
* [Compressor](#compressor)
* [Whammy](#whammy)
* [Resonant Low Pass Filter](#resonant-low-pass-filter)
* [Normalised Resonant Low Pass Filter](#normalised-resonant-low-pass-filter)
* [Resonant High Pass Filter](#resonant-high-pass-filter)
* [Normalised Resonant High Pass Filter](#normalised-resonant-high-pass-filter)
* [High Pass Filter](#high-pass-filter)
* [Normalised High Pass Filter](#normalised-high-pass-filter)
* [Low Pass Filter](#low-pass-filter)
* [Normalised Low Pass Filter.](#normalised-low-pass-filter.)
* [Normaliser](#normaliser)
* [Distortion](#distortion)
* [Pan](#pan)
* [Band Pass Filter](#band-pass-filter)
* [Normalised Band Pass Filter](#normalised-band-pass-filter)
* [Resonant Band Pass Filter](#resonant-band-pass-filter)
* [Normalised Resonant Band Pass Filter](#normalised-resonant-band-pass-filter)
* [Band EQ Filter](#band-eq-filter)
* [Hyperbolic Tangent](#hyperbolic-tangent)
* [Pitch shift](#pitch-shift)
* [Ring Modulator](#ring-modulator)
* [Octaver](#octaver)
* [Vowel](#vowel)
* [Flanger](#flanger)

## Bitcrusher

### Key:
  :bitcrusher

### Doc:
  Creates lo-fi output by decimating and deconstructing the incoming audio by lowering both the sample rate and bit depth. The default sample rate for CD audio is 44100, so use values less than that for that crunchy chip-tune sound full of artefacts and bitty distortion. Similarly, the default bit depth for CD audio is 16, so use values less than that for lo-fi sound.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * sample_rate:
    - doc: The sample rate the audio will be resampled at. This represents the number of times per second the audio is sampled. The higher the sample rate, the closer to the original the sound will be, the lower the more low-fi it will sound. The highest sample rate is 44100 (full quality) and the lowest is ~100 (extremely low quality). Try values in between such as 1000, 3000, 8000...
    - default: 10000
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * bits:
    - doc: The bit depth of the resampled audio. Lower bit depths make the audio sound grainy and less defined. The highest bit depth is 16 (full quality) and the lowest is 1 (lowest quality).
    - default: 8
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## krush

### Key:
  :krush

### Doc:
  Krush that sound!

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * gain:
    - doc: Amount of crushing to serve
    - default: 5
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Reverb

### Key:
  :reverb

### Doc:
  Make the incoming signal sound more spacious or distant as if it were played in a large room or cave. Signal may also be dampened by reducing the amplitude of the higher frequencies.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 0.4
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * room:
    - doc: The room size - a value between 0 (no reverb) and 1 (maximum reverb).
    - default: 0.6
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * damp:
    - doc: High frequency dampening - a value between 0 (no dampening) and 1 (maximum dampening)
    - default: 0.5
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## GVerb

### Key:
  :gverb

### Doc:
  Make the incoming signal sound more spacious or distant as if it were played in a large room or cave. Similar to reverb but with a more spacious feel.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 0.4
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * spread:
    - doc: Stereo spread. Amount of stereo spread the reverb has over the left and right channels. A value of 0 means no spread at all - left and right stereo values of the incoming signal are preserved. A value of 1 means full spread - the left and right channels are fully mixed within the reverb - bleeding into each other.
    - default: 0.5
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * damp:
    - doc: High frequency rolloff. 0 is no damping (the reverb will ring out more) and 1 dampens the reverb signal completely
    - default: 0.5
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_damp:
    - doc: High frequency rolloff of input signal. 0 is no damping (the reverb will ring out more) and 1 dampens the reverb signal completely
    - default: 0.5
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * dry:
    - doc: Amount of original dry signal present in the effect. This is distinct from mix.
    - default: 1
    - constraints: must be a value greater than or equal to 0
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * room:
    - doc: The room size in squared metres
    - default: 10
    - constraints: must be a value greater than or equal to 0
    - May be changed whilst playing
  * release:
    - doc: Time for reverberation to complete in seconds
    - default: 3
    - constraints: must be a value greater than 0
    - May be changed whilst playing
  * ref_level:
    - doc: Reflection level
    - default: 0.7
    - constraints: must be a value greater than or equal to 0
    - May be changed whilst playing
  * tail_level:
    - doc: Tail level amount
    - default: 0.5
    - constraints: must be a value greater than or equal to 0
    - May be changed whilst playing


## Level Amplifier

### Key:
  :level

### Doc:
  Amplitude modifier. All FX have their own amp built in, so it may be the case that you don't specifically need an isolated amp FX. However, it is useful to be able to control the overall amplitude of a number of running synths. All sounds created in the FX block will have their amplitudes multipled by the amp level of this FX. For example, use an amp of 0 to silence all internal synths.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Echo

### Key:
  :echo

### Doc:
  Standard echo with variable phase duration (time between echoes) and decay (length of echo fade out). If you wish to have a phase duration longer than 2s, you need to specify the longest phase duration you'd like with the arg max_phase. Be warned, echo FX with very long phases can consume a lot of memory and take longer to initialise.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * phase:
    - doc: The time between echoes in beats.
    - default: 0.25
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * decay:
    - doc: The time it takes for the echoes to fade away in beats.
    - default: 2
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * max_phase:
    - doc: The maximum phase duration in beats.
    - default: 2
    - constraints: must be greater than zero
    - Can not be changed once set


## Slicer

### Key:
  :slicer

### Doc:
  Modulates the amplitude of the input signal with a specific control wave and phase duration. With the default pulse wave, slices the signal in and out, with the triangle wave, fades the signal in and out and with the saw wave, phases the signal in and then dramatically out. Control wave may be inverted with the arg invert_wave for more variety.

### Opts:
  * amp:
    - doc: The amplitude of the resulting effect.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * phase:
    - doc: The phase duration (in beats) of the slices
    - default: 0.25
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * amp_min:
    - doc: Minimum amplitude of the slicer
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp_max:
    - doc: Maximum amplitude of the slicer
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pulse_width:
    - doc: The width of the pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * phase_offset:
    - doc: Initial phase offset.
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * wave:
    - doc: Control waveform used to modulate the amplitude. 0=saw, 1=pulse, 2=tri, 3=sine
    - default: 1
    - constraints: must be one of the following values: [0, 1, 2, 3]
    - May be changed whilst playing
  * invert_wave:
    - doc: Invert control waveform (i.e. flip it on the y axis). 0=uninverted wave, 1=inverted wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * probability:
    - doc: Probability (as a value between 0 and 1) that a given slice will be replaced by the value of the  prob_pos opt (which defaults to 0, i.e. silence)
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * prob_pos:
    - doc: Position of the slicer that will be jumped to when the probability test passes as a value between 0 and 1
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * seed:
    - doc: Seed value for rand num generator used for probability test
    - default: 0
    - constraints: none
    - Can not be changed once set
  * smooth:
    - doc: Amount of time in seconds to transition from the current value to the next. Allows you to round off harsh edges in the slicer wave which may cause clicks.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * smooth_up:
    - doc: Amount of time in seconds to transition from the current value to the next only when the value is going up. This smoothing happens before the main smooth mechanism.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * smooth_down:
    - doc: Amount of time in seconds to transition from the current value to the next only when the value is going down. This smoothing happens before the main smooth mechanism.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Pan Slicer

### Key:
  :panslicer

### Doc:
  Slice the pan automatically from left to right. Behaves similarly to slicer and wobble FX but modifies stereo panning of sound in left and right speakers. Default slice wave form is square (hard slicing between left and right) however other wave forms can be set with the `wave:` opt.

### Opts:
  * amp:
    - doc: The amplitude of the resulting effect.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * phase:
    - doc: The phase duration (in beats) of the slices
    - default: 0.25
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * pan_min:
    - doc: Minimum pan value (-1 is the left speaker only)
    - default: -1
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pan_max:
    - doc: Maximum pan value (+1 is the right speaker only)
    - default: 1
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pulse_width:
    - doc: The width of the pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * phase_offset:
    - doc: Initial phase offset.
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * wave:
    - doc: Control waveform used to modulate the amplitude. 0=saw, 1=pulse, 2=tri, 3=sine
    - default: 1
    - constraints: must be one of the following values: [0, 1, 2, 3]
    - May be changed whilst playing
  * invert_wave:
    - doc: Invert control waveform (i.e. flip it on the y axis). 0=uninverted wave, 1=inverted wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * probability:
    - doc: Probability (as a value between 0 and 1) that a given slice will be replaced by the value of the  prob_pos opt (which defaults to 0, i.e. silence)
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * prob_pos:
    - doc: Position of the slicer that will be jumped to when the probability test passes as a value between 0 and 1
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * seed:
    - doc: Seed value for rand num generator used for probability test
    - default: 0
    - constraints: none
    - Can not be changed once set
  * smooth:
    - doc: Amount of time in seconds to transition from the current value to the next. Allows you to round off harsh edges in the slicer wave which may cause clicks.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * smooth_up:
    - doc: Amount of time in seconds to transition from the current value to the next only when the value is going up. This smoothing happens before the main smooth mechanism.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * smooth_down:
    - doc: Amount of time in seconds to transition from the current value to the next only when the value is going down. This smoothing happens before the main smooth mechanism.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Wobble

### Key:
  :wobble

### Doc:
  Versatile wobble FX. Will repeatedly modulate a range of filters (rlpf, rhpf) between two cutoff values using a range of control wave forms (saw, pulse, tri, sine). You may alter the phase duration of the wobble, and the resonance of the filter. Combines well with the dsaw synth for crazy dub wobbles. Cutoff value is at cutoff_min at the start of phase

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * phase:
    - doc: The phase duration (in beats) for filter modulation cycles
    - default: 0.5
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * cutoff_min:
    - doc: Minimum (MIDI) note that filter will move to whilst wobbling. Choose a lower note for a higher range of movement. Full range of movement is the distance between cutoff_max and cutoff_min
    - default: 60
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff_max:
    - doc: Maximum (MIDI) note that filter will move to whilst wobbling. Choose a higher note for a higher range of movement. Full range of movement is the distance between cutoff_max and cutoff_min
    - default: 120
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.8
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * phase_offset:
    - doc: Initial modulation phase offset (a value between 0 and 1).
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * wave:
    - doc: Wave shape of wobble. Use 0 for saw wave, 1 for pulse, 2 for triangle wave and 3 for a sine wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1, 2, 3]
    - May be changed whilst playing
  * invert_wave:
    - doc: Invert control waveform (i.e. flip it on the y axis). 0=uninverted wave, 1=inverted wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * pulse_width:
    - doc: Only valid if wave is type pulse.
    - default: 0.5
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * filter:
    - doc: Filter used for wobble effect. Use 0 for a resonant low pass filter or 1 for a resonant high pass filter
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * probability:
    - doc: Probability (as a value between 0 and 1) that a given wobble will be replaced by the value of the  prob_pos opt (which defaults to 0, i.e. min_cutoff)
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * prob_pos:
    - doc: Position of the wobble that will be jumped to when the probability test passes as a value between 0 and 1
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * seed:
    - doc: Seed value for rand num generator used for probability test
    - default: 0
    - constraints: none
    - Can not be changed once set
  * smooth:
    - doc: Amount of time in seconds to transition from the current value to the next. Allows you to round off harsh edges in the slicer wave which may cause clicks.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * smooth_up:
    - doc: Amount of time in seconds to transition from the current value to the next only when the value is going up. This smoothing happens before the main smooth mechanism.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * smooth_down:
    - doc: Amount of time in seconds to transition from the current value to the next only when the value is going down. This smoothing happens before the main smooth mechanism.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Techno from IXI Lang

### Key:
  :ixi_techno

### Doc:
  Moving resonant low pass filter between min and max cutoffs. Great for sweeping effects across long synths or samples.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * phase:
    - doc: The phase duration (in beats) for filter modulation cycles
    - default: 4
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * phase_offset:
    - doc: Initial modulation phase offset (a value between 0 and 1).
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * cutoff_min:
    - doc: Minimum (MIDI) note that filter will move to whilst wobbling. Choose a lower note for a higher range of movement. Full range of movement is the distance between cutoff_max and cutoff_min
    - default: 60
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff_max:
    - doc: Maximum (MIDI) note that filter will move to whilst wobbling. Choose a higher note for a higher range of movement. Full range of movement is the distance between cutoff_max and cutoff_min
    - default: 120
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.8
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Compressor

### Key:
  :compressor

### Doc:
  Compresses the dynamic range of the incoming signal. Equivalent to automatically turning the amp down when the signal gets too loud and then back up again when it's quiet. Useful for ensuring the containing signal doesn't overwhelm other aspects of the sound. Also a general purpose hard-knee dynamic range processor which can be tuned via the opts to both expand and compress the signal.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * threshold:
    - doc: Threshold value determining the break point between slope_below and slope_above.
    - default: 0.2
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * clamp_time:
    - doc: Time taken for the amplitude adjustments to kick in fully (in seconds). This is usually pretty small (not much more than 10 milliseconds). Also known as the time of the attack phase
    - default: 0.01
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * slope_above:
    - doc: Slope of the amplitude curve above the threshold. A value of 1 means that the output of signals with amplitude above the threshold will be unaffected. Greater values will magnify and smaller values will attenuate the signal.
    - default: 0.5
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * slope_below:
    - doc: Slope of the amplitude curve below the threshold. A value of 1 means that the output of signals with amplitude below the threshold will be unaffected. Greater values will magnify and smaller values will attenuate the signal.
    - default: 1
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * relax_time:
    - doc: Time taken for the amplitude adjustments to be released. Usually a little longer than clamp_time. If both times are too short, you can get some (possibly unwanted) artefacts. Also known as the time of the release phase.
    - default: 0.01
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Whammy

### Key:
  :whammy

### Doc:
  A cheap sounding transposition effect, with a slightly robotic edge. Good for adding alien sounds and harmonies to everything from beeps to guitar samples. It's similar to pitch shift although not as smooth sounding.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * transpose:
    - doc: This is how much to transpose the input, expressed as a midi pitch.
    - default: 12
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * max_delay_time:
    - doc: The max delay time to be used for the effect. This shouldn't need to be adjusted.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * deltime:
    - doc: The delay time to be used for the effect. This shouldn't need to be adjusted.
    - default: 0.05
    - constraints: must be zero or greater
    - Can not be changed once set
  * grainsize:
    - doc: The size of the initial grain used for transposition. This shouldn't need to be adjusted.
    - default: 0.075
    - constraints: must be zero or greater
    - Can not be changed once set


## Resonant Low Pass Filter

### Key:
  :rlpf

### Doc:
  Dampens the parts of the signal that are higher than the cutoff point (typically the crunchy fizzy harmonic overtones) and keeps the lower parts (typically the bass/mid of the sound). The resonant part of the resonant low pass filter emphasises/resonates the frequencies around the cutoff point. The amount of emphasis is controlled by the res opt with a higher res resulting in greater resonance. High amounts of resonance (rq ~1) can create a whistling sound around the cutoff frequency.

  Choose a higher cutoff to keep more of the high frequences/treble of the sound and a lower cutoff to make the sound more dull and only keep the bass.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.5
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Normalised Resonant Low Pass Filter

### Key:
  :nrlpf

### Doc:
  Dampens the parts of the signal that are higher than the cutoff point (typically the crunchy fizzy harmonic overtones) and keeps the lower parts (typically the bass/mid of the sound). The resonant part of the resonant low pass filter emphasises/resonates the frequencies around the cutoff point. The amount of emphasis is controlled by the res opt with a higher res resulting in greater resonance. High amounts of resonance (rq ~1) can create a whistling sound around the cutoff frequency.

  Choose a higher cutoff to keep more of the high frequences/treble of the sound and a lower cutoff to make the sound more dull and only keep the bass.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.5
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Resonant High Pass Filter

### Key:
  :rhpf

### Doc:
  Dampens the parts of the signal that are lower than the cutoff point (typically the bass of the sound) and keeps the higher parts (typically the crunchy fizzy harmonic overtones). The resonant part of the resonant high pass filter emphasises/resonates the frequencies around the cutoff point. The amount of emphasis is controlled by the res opt with a higher res resulting in greater resonance. High amounts of resonance (rq ~1) can create a whistling sound around the cutoff frequency.

  Choose a lower cutoff to keep more of the bass/mid and a higher cutoff to make the sound more light and crispy.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.5
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Normalised Resonant High Pass Filter

### Key:
  :nrhpf

### Doc:
  Dampens the parts of the signal that are lower than the cutoff point (typically the bass of the sound) and keeps the higher parts (typically the crunchy fizzy harmonic overtones). The resonant part of the resonant high pass filter emphasises/resonates the frequencies around the cutoff point. The amount of emphasis is controlled by the res opt with a higher res resulting in greater resonance. High amounts of resonance (rq ~1) can create a whistling sound around the cutoff frequency.

  Choose a lower cutoff to keep more of the bass/mid and a higher cutoff to make the sound more light and crispy.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.5
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## High Pass Filter

### Key:
  :hpf

### Doc:
  Dampens the parts of the signal that are lower than the cutoff point (typically the bass of the sound) and keeps the higher parts (typically the crunchy fizzy harmonic overtones). Choose a lower cutoff to keep more of the bass/mid and a higher cutoff to make the sound more light and crispy.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Normalised High Pass Filter

### Key:
  :nhpf

### Doc:
  A high pass filter chained to a normaliser. Ensures that the signal is both filtered by a standard high pass filter and then normalised to ensure the amplitude of the final output is constant. A high pass filter will reduce the amplitude of the resulting signal (as some of the sound has been filtered out) the normaliser can compensate for this loss (although will also have the side effect of flattening all dynamics). See doc for hpf.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Low Pass Filter

### Key:
  :lpf

### Doc:
  Dampens the parts of the signal that are higher than the cutoff point (typically the crunchy fizzy harmonic overtones) and keeps the lower parts (typically the bass/mid of the sound). Choose a higher cutoff to keep more of the high frequences/treble of the sound and a lower cutoff to make the sound more dull and only keep the bass.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Normalised Low Pass Filter.

### Key:
  :nlpf

### Doc:
  A low pass filter chained to a normaliser. Ensures that the signal is both filtered by a standard low pass filter and then normalised to ensure the amplitude of the final output is constant. A low pass filter will reduce the amplitude of the resulting signal (as some of the sound has been filtered out) the normaliser can compensate for this loss (although will also have the side effect of flattening all dynamics). See doc for lpf.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Normaliser

### Key:
  :normaliser

### Doc:
  Raise or lower amplitude of sound to a specified level. Evens out the amplitude of incoming sound across the frequency spectrum by flattening all dynamics.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * level:
    - doc: The peak output amplitude level at which to normalise the input.
    - default: 1
    - constraints: must be a value greater than or equal to 0
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Distortion

### Key:
  :distortion

### Doc:
  Distorts the signal reducing clarity in favour of raw crunchy noise.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * distort:
    - doc: Amount of distortion to be applied (as a value between 0 and 1)
    - default: 0.5
    - constraints: must be a value greater than or equal to 0,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Pan

### Key:
  :pan

### Doc:
  Specify where in the stereo field the sound should be heard. A value of -1 for pan will put the sound in the left speaker, a value of 1 will put the sound in the right speaker and values in between will shift the sound accordingly.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the sound is completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Band Pass Filter

### Key:
  :bpf

### Doc:
  Combines low pass and high pass filters to only allow a 'band' of frequencies through. If the band is very narrow (a low res value like 0.0001) then the BPF will reduce the original sound, almost down to a single frequency (controlled by the centre opt).

  With higher values for res we can simulate other filters e.g. telephone lines, by cutting off low and high frequencies.

Use FX `:band_eq` with a negative db for the opposite effect - to attenuate a given band of frequencies.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * centre:
    - doc: Centre frequency for the filter as a MIDI note.
    - default: 100
    - constraints: must be a value greater than or equal to 0
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.6
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Normalised Band Pass Filter

### Key:
  :nbpf

### Doc:
  Like the Band Pass Filter but normalised. The normaliser is useful here as some volume is lost when filtering the original signal.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * centre:
    - doc: Centre frequency for the filter as a MIDI note.
    - default: 100
    - constraints: must be a value greater than or equal to 0
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.6
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Resonant Band Pass Filter

### Key:
  :rbpf

### Doc:
  Like the Band Pass Filter but with a resonance (slight volume boost) around the target frequency. This can produce an interesting whistling effect, especially when used with larger values for the res opt.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * centre:
    - doc: Centre frequency for the filter as a MIDI note.
    - default: 100
    - constraints: must be a value greater than or equal to 0
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.5
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Normalised Resonant Band Pass Filter

### Key:
  :nrbpf

### Doc:
  Like the Band Pass Filter but normalised, with a resonance (slight volume boost) around the target frequency. This can produce an interesting whistling effect, especially when used with larger values for the res opt.

  The normaliser is useful here as some volume is lost when filtering the original signal.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * centre:
    - doc: Centre frequency for the filter as a MIDI note.
    - default: 100
    - constraints: must be a value greater than or equal to 0
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.5
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Band EQ Filter

### Key:
  :band_eq

### Doc:
  Attenuate or Boost a frequency band

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * freq:
    - doc: Centre frequency of the band in MIDI.
    - default: 100
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Width of the band as a value between 0 and 1
    - default: 0.6
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * db:
    - doc: Amount of boost or attenuation of the frequency band. A positive value boosts frequencies in the band, a negative value attenuates them.
    - default: 0.6
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Hyperbolic Tangent

### Key:
  :tanh

### Doc:
  Please write documentation!

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * krunch:
    - doc: Higher values progressively destroy the sound. Achieved through a balanced manipulation of pre_amp and amp such that the tanh is pushed harder with higher krunch values yet the overall amplitude stays similar.
    - default: 5
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Pitch shift

### Key:
  :pitch_shift

### Doc:
  Changes the pitch of a signal without affecting tempo. Does this mainly through the pitch parameter which takes a midi number to transpose by. You can also play with the other params to produce some interesting textures and sounds.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * window_size:
    - doc: Pitch shift works by chopping the input into tiny slices, then playing these slices at a higher or lower rate. If we make the slices small enough and overlap them, it sounds like the original sound with the pitch changed.

  The window_size is the length of the slices and is measured in seconds. It needs to be around 0.2 (200ms) or greater for pitched sounds like guitar or bass, and needs to be around 0.02 (20ms) or lower for percussive sounds like drum loops. You can experiment with this to get the best sound for your input.
    - default: 0.2
    - constraints: must be a value greater than 5.0e-05
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pitch:
    - doc: Pitch adjustment in semitones. 1 is up a semitone, 12 is up an octave, -12 is down an octave etc. Maximum upper limit of 24 (up 2 octaves). Lower limit of -72 (down 6 octaves). Decimal numbers can be used for fine tuning.
    - default: 0
    - constraints: must be a value greater than or equal to -72,must be a value less than or equal to 24
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pitch_dis:
    - doc: Pitch dispersion - how much random variation in pitch to add. Using a low value like 0.001 can help to "soften up" the metallic sounds, especially on drum loops. To be really technical, pitch_dispersion is the maximum random deviation of the pitch from the pitch ratio (which is set by the pitch param)
    - default: 0.0
    - constraints: must be a value greater than or equal to 0
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * time_dis:
    - doc: Time dispersion - how much random delay before playing each grain (measured in seconds). Again, low values here like 0.001 can help to soften up metallic sounds introduced by the effect. Large values are also fun as they can make soundscapes and textures from the input, although you will most likely lose the rhythm of the original. NB - This won't have an effect if it's larger than window_size.
    - default: 0.0
    - constraints: must be a value greater than or equal to 0
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Ring Modulator

### Key:
  :ring_mod

### Doc:
  Attack of the Daleks! Ring mod is a classic effect often used on soundtracks to evoke robots or aliens as it sounds hollow or metallic. We take a 'carrier' signal (a sine wave controlled by the freq opt) and modulate its amplitude using the signal given inside the fx block. This produces a wide variety of sounds - the best way to learn is to experiment!

### Opts:
  * freq:
    - doc: Frequency of the carrier signal (as a midi note).
    - default: 30
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_amp:
    - doc: Amplitude of the modulation
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Octaver

### Key:
  :octaver

### Doc:
  This effect adds three pitches based on the input sound. The first is the original sound transposed up an octave (super_amp), the second is the original sound transposed down an octave (sub_amp) and the third is the original sound transposed down two octaves (subsub_amp).

  The way the transpositions are done adds some distortion/fuzz, particularly to the lower octaves, whilst the upper octave has a 'cheap' quality. This effect is often used in guitar effects pedals but it can work with other sounds too. There's a great description of the science behind this on Wikipedia here: https://en.wikipedia.org/wiki/Octave_effect

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * super_amp:
    - doc: Volume of the signal 1 octave above the input
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * sub_amp:
    - doc: Volume of the signal 1 octave below the input
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * subsub_amp:
    - doc: Volume of the signal 2 octaves below the input
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Vowel

### Key:
  :vowel

### Doc:
  This effect filters the input to match a human voice singing a certain vowel sound. Human singing voice sounds are easily achieved with a source of a saw wave with a little vibrato.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * vowel_sound:
    - doc: 1,2,3,4,5 => A,E,I,O,U
    - default: 1
    - constraints: must be one of the following values: [1, 2, 3, 4, 5]
    - May be changed whilst playing
  * voice:
    - doc: 0,1,2,3,4 => Soprano,Alto,Counter Tenor, Tenor, Bass
    - default: 0
    - constraints: must be one of the following values: [0, 1, 2, 3, 4]
    - May be changed whilst playing


## Flanger

### Key:
  :flanger

### Doc:
  Mix the incoming signal with a copy of itself which has a rate modulating faster and slower than the original. Creates a swirling/whooshing effect.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mix:
    - doc: The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplification applied to the input signal immediately before it is passed to the FX.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * phase:
    - doc: Phase duration in beats of flanger modulation.
    - default: 4
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * phase_offset:
    - doc: Initial modulation phase offset (a value between 0 and 1).
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * wave:
    - doc: Wave type - 0 saw, 1 pulse, 2 triangle, 3 sine, 4 cubic. Different waves will produce different flanging modulation effects.
    - default: 4
    - constraints: must be one of the following values: [0, 1, 2, 3, 4]
    - May be changed whilst playing
  * invert_wave:
    - doc: Invert flanger control waveform (i.e. flip it on the y axis). 0=uninverted wave, 1=inverted wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * stereo_invert_wave:
    - doc: Make the flanger control waveform in the left ear an inversion of the control waveform in the right ear. 0=uninverted wave, 1=inverted wave. This happens after the standard wave inversion with param :invert_wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * delay:
    - doc: Amount of delay time between original and flanged version of audio.
    - default: 5
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * max_delay:
    - doc: Max delay time. Used to set internal buffer size.
    - default: 20
    - constraints: must be zero or greater
    - Can not be changed once set
  * depth:
    - doc: Flange depth - greater depths produce a more prominent effect.
    - default: 5
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * decay:
    - doc: Flange decay time in ms
    - default: 2
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * feedback:
    - doc: Amount of feedback.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * invert_flange:
    - doc: Invert flanger signal. 0=no inversion, 1=inverted signal.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing


