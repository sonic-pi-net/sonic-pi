# Synths

* [Dull Bell](#dull-bell)
* [Pretty Bell](#pretty-bell)
* [Sine Wave](#sine-wave)
* [Sine Wave](#sine-wave)
* [Saw Wave](#saw-wave)
* [Pulse Wave](#pulse-wave)
* [Pulse Wave with sub](#pulse-wave-with-sub)
* [Square Wave](#square-wave)
* [Triangle Wave](#triangle-wave)
* [Detuned Saw wave](#detuned-saw-wave)
* [Detuned Pulse Wave](#detuned-pulse-wave)
* [Detuned Triangle Wave](#detuned-triangle-wave)
* [Basic FM synthesis](#basic-fm-synthesis)
* [Basic FM synthesis with frequency modulation.](#basic-fm-synthesis-with-frequency-modulation.)
* [Modulated Saw Wave](#modulated-saw-wave)
* [Modulated Detuned Saw Waves](#modulated-detuned-saw-waves)
* [Modulated Sine Wave](#modulated-sine-wave)
* [Modulated Sine Wave](#modulated-sine-wave)
* [Modulated Triangle Wave](#modulated-triangle-wave)
* [Modulated Pulse](#modulated-pulse)
* [TB-303 Emulation](#tb-303-emulation)
* [Supersaw](#supersaw)
* [Hoover](#hoover)
* [The Prophet](#the-prophet)
* [Zawa](#zawa)
* [Dark Ambience](#dark-ambience)
* [Growl](#growl)
* [Hollow](#hollow)
* [Mono Sample Player](#mono-sample-player)
* [Stereo Sample Player](#stereo-sample-player)
* [Blade Runner style strings](#blade-runner-style-strings)
* [SynthPiano](#synthpiano)
* [SynthPluck](#synthpluck)
* [Sound In](#sound-in)
* [Noise](#noise)
* [Pink Noise](#pink-noise)
* [Brown Noise](#brown-noise)
* [Grey Noise](#grey-noise)
* [Clip Noise](#clip-noise)
* [Basic Mono Sample Player (no env)](#basic-mono-sample-player-(no-env))
* [Basic Stereo Sample Player (no env)](#basic-stereo-sample-player-(no-env))
* [Basic Mixer](#basic-mixer)
* [Main Mixer](#main-mixer)

## Dull Bell

### Key:
  :dull_bell

### Doc:
  A simple dull discordant bell sound.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set


## Pretty Bell

### Key:
  :pretty_bell

### Doc:
  A pretty bell sound. Works well with short attacks and long decays.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set


## Sine Wave

### Key:
  :beep

### Doc:
  A simple pure sine wave. The sine wave is the simplest, purest sound there is and is the fundamental building block of all noise. The mathematician Fourier demonstrated that any sound could be built out of a number of sine waves (the more complex the sound, the more sine waves needed). Have a play combining a number of sine waves to design your own sounds!

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set


## Sine Wave

### Key:
  :sine

### Doc:
  A simple pure sine wave. The sine wave is the simplest, purest sound there is and is the fundamental building block of all noise. The mathematician Fourier demonstrated that any sound could be built out of a number of sine waves (the more complex the sound, the more sine waves needed). Have a play combining a number of sine waves to design your own sounds!

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set


## Saw Wave

### Key:
  :saw

### Doc:
  A saw wave with a low pass filter. Great for using with FX such as the built in low pass filter (available via the cutoff arg) due to the complexity and thickness of the sound.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set


## Pulse Wave

### Key:
  :pulse

### Doc:
  A simple pulse wave with a low pass filter. This defaults to a square wave, but the timbre can be changed dramatically by adjusting the pulse_width arg between 0 and 1. The pulse wave is thick and heavy with lower notes and is a great ingredient for bass sounds.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pulse_width:
    - doc: The width of the pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Pulse Wave with sub

### Key:
  :subpulse

### Doc:
  A pulse wave with a sub sine wave passed through a low pass filter. The pulse wave is thick and heavy with lower notes and is a great ingredient for bass sounds - especially with the sub wave.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pulse_width:
    - doc: The width of the pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * sub_amp:
    - doc: Amplitude for the additional sine wave.
    - default: 1
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * sub_detune:
    - doc: Amount of detune from the note for the additional sine wave. Default is -12
    - default: -12
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Square Wave

### Key:
  :square

### Doc:
  A simple square wave with a low pass filter. The square wave is thick and heavy with lower notes and is a great ingredient for bass sounds. If you wish to modulate the width of the square wave see the synth pulse.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Triangle Wave

### Key:
  :tri

### Doc:
  A simple triangle wave with a low pass filter.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pulse_width:
    - doc: The width of the pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Detuned Saw wave

### Key:
  :dsaw

### Doc:
  A pair of detuned saw waves passed through a low pass filter. Two saw waves with slightly different frequencies generates a nice thick sound which is the basis for a lot of famous synth sounds. Thicken the sound by increasing the detune value, or create an octave-playing synth by choosing a detune of 12 (12 MIDI notes is an octave).

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * detune:
    - doc: Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds.
    - default: 0.1
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Detuned Pulse Wave

### Key:
  :dpulse

### Doc:
  A pair of detuned pulse waves passed through a low pass filter. Two pulse waves with slightly different frequencies generates a nice thick sound which can be used as a basis for some nice bass sounds. Thicken the sound by increasing the detune value, or create an octave-playing synth by choosing a detune of 12 (12 MIDI notes is an octave). Each pulse wave can also have individual widths (although the default is for the detuned pulse to mirror the width of the main pulse).

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * detune:
    - doc: Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds.
    - default: 0.1
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pulse_width:
    - doc: The width of the pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * dpulse_width:
    - doc: The width of the second detuned pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.
    - default: pulse_width
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Detuned Triangle Wave

### Key:
  :dtri

### Doc:
  A pair of detuned triangle waves passed through a low pass filter. Two pulse waves with slightly different frequencies generates a nice thick sound which can be used as a basis for some nice bass sounds. Thicken the sound by increasing the detune value, or create an octave-playing synth by choosing a detune of 12 (12 MIDI notes is an octave).

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * detune:
    - doc: Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds.
    - default: 0.1
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Basic FM synthesis

### Key:
  :fm

### Doc:
  A sine wave with a fundamental frequency which is modulated at audio rate by another sine wave with a specific modulation, division and depth. Useful for generating a wide range of sounds by playing with the divisor and depth params. Great for deep powerful bass and crazy 70s sci-fi sounds.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * divisor:
    - doc: Modifies the frequency of the modulator oscillator relative to the carrier. Don't worry too much about what this means - just try different numbers out!
    - default: 2
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * depth:
    - doc: Modifies the depth of the carrier wave used to modify fundamental frequency. Don't worry too much about what this means - just try different numbers out!
    - default: 1
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Basic FM synthesis with frequency modulation.

### Key:
  :mod_fm

### Doc:
  The FM synth modulating between two notes - the duration of the modulation can be modified using the mod_phase arg, the range (number of notes jumped between) by the mod_range arg and the width of the jumps by the mod_width param. The FM synth is a sine wave with a fundamental frequency which is modulated at audio rate by another sine wave with a specific modulation, division and depth. Useful for generating a wide range of sounds by playing with the `:divisor` and `:depth` params. Great for deep powerful bass and crazy 70s sci-fi sounds.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * divisor:
    - doc: Modifies the frequency of the modulator oscillator relative to the carrier. Don't worry too much about what this means - just try different numbers out!
    - default: 2
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * depth:
    - doc: Modifies the depth of the carrier wave used to modify fundamental frequency. Don't worry too much about what this means - just try different numbers out!
    - default: 1
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase:
    - doc: Phase duration in beats of oscillations between the two notes. Time it takes to switch between the notes.
    - default: 0.25
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: none
    - May be changed whilst playing
  * mod_pulse_width:
    - doc: The width of the modulated pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Only valid if mod wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
  * mod_phase_offset:
    - doc: Initial modulation phase offset (a value between 0 and 1).
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * mod_invert_wave:
    - doc: Invert mod waveform (i.e. flip it on the y axis). 0=normal wave, 1=inverted wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * mod_wave:
    - doc: Wave shape of mod wave. 0=saw wave, 1=pulse, 2=triangle wave and 3=sine wave.
    - default: 1
    - constraints: must be one of the following values: [0, 1, 2, 3]
    - May be changed whilst playing


## Modulated Saw Wave

### Key:
  :mod_saw

### Doc:
  A saw wave passed through a low pass filter which modulates between two separate notes via a variety of control waves.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase:
    - doc: Phase duration in beats of oscillations between the two notes. Time it takes to switch between the notes.
    - default: 0.25
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_pulse_width:
    - doc: The width of the modulated pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Only valid if mod wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase_offset:
    - doc: Initial modulation phase offset (a value between 0 and 1).
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * mod_invert_wave:
    - doc: Invert mod waveform (i.e. flip it on the y axis). 0=normal wave, 1=inverted wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * mod_wave:
    - doc: Wave shape of mod wave. 0=saw wave, 1=pulse, 2=triangle wave and 3=sine wave.
    - default: 1
    - constraints: must be one of the following values: [0, 1, 2, 3]
    - May be changed whilst playing


## Modulated Detuned Saw Waves

### Key:
  :mod_dsaw

### Doc:
  A pair of detuned saw waves (see the dsaw synth) which are modulated between two fixed notes at a given rate.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase:
    - doc: Phase duration in beats of oscillations between the two notes. Time it takes to switch between the notes.
    - default: 0.25
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_pulse_width:
    - doc: The width of the modulated pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Only valid if mod wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase_offset:
    - doc: Initial modulation phase offset (a value between 0 and 1).
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * mod_invert_wave:
    - doc: Invert mod waveform (i.e. flip it on the y axis). 0=normal wave, 1=inverted wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * mod_wave:
    - doc: Wave shape of mod wave. 0=saw wave, 1=pulse, 2=triangle wave and 3=sine wave.
    - default: 1
    - constraints: must be one of the following values: [0, 1, 2, 3]
    - May be changed whilst playing
  * detune:
    - doc: Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds.
    - default: 0.1
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Modulated Sine Wave

### Key:
  :mod_sine

### Doc:
  A sine wave passed through a low pass filter which modulates between two separate notes via a variety of control waves.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase:
    - doc: Phase duration in beats of oscillations between the two notes. Time it takes to switch between the notes.
    - default: 0.25
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_pulse_width:
    - doc: The width of the modulated pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Only valid if mod wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase_offset:
    - doc: Initial modulation phase offset (a value between 0 and 1).
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * mod_invert_wave:
    - doc: Invert mod waveform (i.e. flip it on the y axis). 0=normal wave, 1=inverted wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * mod_wave:
    - doc: Wave shape of mod wave. 0=saw wave, 1=pulse, 2=triangle wave and 3=sine wave.
    - default: 1
    - constraints: must be one of the following values: [0, 1, 2, 3]
    - May be changed whilst playing


## Modulated Sine Wave

### Key:
  :mod_beep

### Doc:
  A sine wave passed through a low pass filter which modulates between two separate notes via a variety of control waves.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase:
    - doc: Phase duration in beats of oscillations between the two notes. Time it takes to switch between the notes.
    - default: 0.25
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_pulse_width:
    - doc: The width of the modulated pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Only valid if mod wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase_offset:
    - doc: Initial modulation phase offset (a value between 0 and 1).
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * mod_invert_wave:
    - doc: Invert mod waveform (i.e. flip it on the y axis). 0=normal wave, 1=inverted wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * mod_wave:
    - doc: Wave shape of mod wave. 0=saw wave, 1=pulse, 2=triangle wave and 3=sine wave.
    - default: 1
    - constraints: must be one of the following values: [0, 1, 2, 3]
    - May be changed whilst playing


## Modulated Triangle Wave

### Key:
  :mod_tri

### Doc:
  A triangle wave passed through a low pass filter which modulates between two separate notes via a variety of control waves.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase:
    - doc: Phase duration in beats of oscillations between the two notes. Time it takes to switch between the notes.
    - default: 0.25
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_pulse_width:
    - doc: The width of the modulated pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Only valid if mod wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase_offset:
    - doc: Initial modulation phase offset (a value between 0 and 1).
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * mod_invert_wave:
    - doc: Invert mod waveform (i.e. flip it on the y axis). 0=normal wave, 1=inverted wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * mod_wave:
    - doc: Wave shape of mod wave. 0=saw wave, 1=pulse, 2=triangle wave and 3=sine wave.
    - default: 1
    - constraints: must be one of the following values: [0, 1, 2, 3]
    - May be changed whilst playing


## Modulated Pulse

### Key:
  :mod_pulse

### Doc:
  A pulse wave with a low pass filter modulating between two notes via a variety of control waves (see mod_wave: arg). The pulse wave defaults to a square wave, but the timbre can be changed dramatically by adjusting the pulse_width arg between 0 and 1.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase:
    - doc: Phase duration in beats of oscillations between the two notes. Time it takes to switch between the notes.
    - default: 0.25
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_pulse_width:
    - doc: The width of the modulated pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Only valid if mod wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * mod_phase_offset:
    - doc: Initial modulation phase offset (a value between 0 and 1).
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * mod_invert_wave:
    - doc: Invert mod waveform (i.e. flip it on the y axis). 0=normal wave, 1=inverted wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * mod_wave:
    - doc: Wave shape of mod wave. 0=saw wave, 1=pulse, 2=triangle wave and 3=sine wave.
    - default: 1
    - constraints: must be one of the following values: [0, 1, 2, 3]
    - May be changed whilst playing
  * pulse_width:
    - doc: The width of the pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## TB-303 Emulation

### Key:
  :tb303

### Doc:
  Emulation of the classic Roland TB-303 Bass Line synthesiser. Overdrive the res (i.e. use very large values) for that classic late 80s acid sound.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: The maximum cutoff value as a MIDI note
    - default: 120
    - constraints: must be a value less than or equal to 130
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff_min:
    - doc: The minimum cutoff value.
    - default: 30
    - constraints: must be a value less than or equal to 130
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff_attack:
    - doc: Attack time for cutoff filter. Amount of time (in beats) for sound to reach full cutoff value. Default value is set to match amp envelope's attack value.
    - default: attack
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * cutoff_decay:
    - doc: Decay time for cutoff filter. Amount of time (in beats) for sound to move from full cutoff value (cutoff attack level) to the cutoff sustain level. Default value is set to match amp envelope's decay value.
    - default: decay
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * cutoff_sustain:
    - doc: Amount of time for cutoff value to remain at sustain level in beats. Default value is set to match amp envelope's sustain value.
    - default: sustain
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * cutoff_release:
    - doc: Amount of time (in beats) for sound to move from cutoff sustain value to cutoff min value. Default value is set to match amp envelope's release value.
    - default: release
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * cutoff_attack_level:
    - doc: The peak cutoff (value of cutoff at peak of attack) as a value between 0 and 1 where 0 is the :cutoff_min and 1 is the :cutoff value
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * cutoff_decay_level:
    - doc: The level of cutoff after the decay phase as a value between 0 and 1 where 0 is the :cutoff_min and 1 is the :cutoff value
    - default: cutoff_sustain_level
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * cutoff_sustain_level:
    - doc: The sustain cutoff (value of cutoff at sustain time) as a value between 0 and 1 where 0 is the :cutoff_min and 1 is the :cutoff value.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.9
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * wave:
    - doc: Wave type - 0 saw, 1 pulse, 2 triangle. Different waves will produce different sounds.
    - default: 0
    - constraints: must be one of the following values: [0, 1, 2]
    - May be changed whilst playing
  * pulse_width:
    - doc: The width of the pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Supersaw

### Key:
  :supersaw

### Doc:
  Thick swirly saw waves sparkling and moving about to create a rich trancy sound.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 130
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.7
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Hoover

### Key:
  :hoover

### Doc:
  Classic early 90's rave synth - 'a sort of slurry chorussy synth line like the classic Dominator by Human Resource'. Based on Dan Stowell's implementation in SuperCollider and Daniel Turczanski's port to Overtone. Works really well with portamento (see docs for the 'control' method).

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0.05
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 130
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.1
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## The Prophet

### Key:
  :prophet

### Doc:
  Dark and swirly, this synth uses Pulse Width Modulation (PWM) to create a timbre which continually moves around. This effect is created using the pulse ugen which produces a variable width square wave. We then control the width of the pulses using a variety of LFOs - sin-osc and lf-tri in this case. We use a number of these LFO modulated pulse ugens with varying LFO type and rate (and phase in some cases) to provide the LFO with a different starting point. We then mix all these pulses together to create a thick sound and then feed it through a resonant low pass filter (rlpf). For extra bass, one of the pulses is an octave lower (half the frequency) and its LFO has a little bit of randomisation thrown into its frequency component for that extra bit of variety.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 110
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.7
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Zawa

### Key:
  :zawa

### Doc:
  Saw wave with oscillating timbre. Produces moving saw waves with a unique character controllable with the control oscillator (usage similar to mod synths).

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.9
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * phase:
    - doc: Phase duration in beats of timbre modulation.
    - default: 1
    - constraints: must be greater than zero
    - May be changed whilst playing
    - Scaled with current BPM value
    - Has slide parameters for shaping changes
  * phase_offset:
    - doc: Initial phase offset of the sync wave (a value between 0 and 1).
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * wave:
    - doc: Wave shape controlling freq sync saw wave. 0=saw wave, 1=pulse, 2=triangle wave and 3=sine wave.
    - default: 3
    - constraints: must be one of the following values: [0, 1, 2, 3]
    - May be changed whilst playing
  * invert_wave:
    - doc: Invert sync freq control waveform (i.e. flip it on the y axis). 0=uninverted wave, 1=inverted wave.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * range:
    - doc: Range of the associated sync saw in MIDI notes from the main note. Modifies timbre.
    - default: 24
    - constraints: must be a value between 0 and 90 inclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * disable_wave:
    - doc: Enable and disable sync control wave (setting to 1 will stop timbre movement).
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * pulse_width:
    - doc: The width of the pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Dark Ambience

### Key:
  :dark_ambience

### Doc:
  A slow rolling bass with a sparkle of light trying to escape the darkness. Great for an ambient sound.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 110
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.7
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * detune1:
    - doc: Distance (in MIDI notes) between the main note and the second component of sound. Affects thickness, sense of tuning and harmony.
    - default: 12
    - constraints: none
    - Can not be changed once set
    - Has slide parameters for shaping changes
  * detune2:
    - doc: Distance (in MIDI notes) between the main note and the third component of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound.
    - default: 24
    - constraints: none
    - Can not be changed once set
    - Has slide parameters for shaping changes
  * noise:
    - doc: Noise source. Has a subtle effect on the timbre of the sound. 0=pink noise (the default), 1=brown noise, 2=white noise, 3=clip noise and 4=grey noise
    - default: 0
    - constraints: must be one of the following values: [0, 1, 2, 3, 4]
    - May be changed whilst playing
  * ring:
    - doc: Amount of ring in the sound. Lower values create a more rough sound, higher values produce a sound with more focus.
    - default: 0.2
    - constraints: must be a value between 0.1 and 50 inclusively
    - May be changed whilst playing
  * room:
    - doc: Room size in squared metres used to calculate the reverb.
    - default: 70
    - constraints: must be a value greater than or equal to 0.1,must be a value less than or equal to 300
    - Can not be changed once set
  * reverb_time:
    - doc: How long in beats the reverb should go on for.
    - default: 100
    - constraints: must be zero or greater
    - Can not be changed once set


## Growl

### Key:
  :growl

### Doc:
  A deep rumbling growl with a bright sine shining through at higher notes.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0.1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 130
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.7
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Hollow

### Key:
  :hollow

### Doc:
  A hollow breathy sound constructed from random noise

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 90
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Only functional if a cutoff value is specified. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0.99
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * noise:
    - doc: Noise source. Has a subtle effect on the timbre of the sound. 0=pink noise, 1=brown noise (the default), 2=white noise, 3=clip noise and 4=grey noise
    - default: 1
    - constraints: must be one of the following values: [0, 1, 2, 3, 4]
    - May be changed whilst playing
  * norm:
    - doc: Normalise the audio (make quieter parts of the sample louder and louder parts quieter) - this is similar to the normaliser FX. This may emphasise any clicks caused by clipping.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing


## Mono Sample Player

### Key:
  :mono_player

### Doc:
  

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplitude multiplier which takes place immediately before any internal FX such as the low pass filter, compressor or pitch modification. Use this opt if you want to overload the compressor.
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
  * attack:
    - doc: Duration of the attack phase of the envelope.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay:
    - doc: Duration of the decay phase of the envelope.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain:
    - doc: Duration of the sustain phase of the envelope. When -1 (the default) will auto-stretch.
    - default: -1
    - constraints: must either be a positive value or -1
    - Can not be changed once set
  * release:
    - doc: Duration of the release phase of the envelope.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff_attack:
    - doc: Attack time for cutoff filter. Amount of time (in beats) for sound to reach full cutoff value. Default value is set to match amp envelope's attack value.
    - default: attack
    - constraints: must be zero or greater
    - Can not be changed once set
  * cutoff_decay:
    - doc: Decay time for cutoff filter. Amount of time (in beats) for sound to move from full cutoff value (cutoff attack level) to the cutoff sustain level. Default value is set to match amp envelope's decay value.
    - default: decay
    - constraints: must be zero or greater
    - Can not be changed once set
  * cutoff_sustain:
    - doc: Amount of time for cutoff value to remain at sustain level in beats. When -1 (the default) will auto-stretch.
    - default: sustain
    - constraints: must either be a positive value or -1
    - Can not be changed once set
  * cutoff_release:
    - doc: Amount of time (in beats) for sound to move from cutoff sustain value to cutoff min value. Default value is set to match amp envelope's release value.
    - default: release
    - constraints: must be zero or greater
    - Can not be changed once set
  * cutoff_attack_level:
    - doc: The peak cutoff (value of cutoff at peak of attack) as a MIDI note.
    - default: cutoff
    - constraints: must be a value between 0 and 130 inclusively
    - Can not be changed once set
  * cutoff_decay_level:
    - doc: The level of cutoff after the decay phase as a MIDI note.
    - default: cutoff
    - constraints: must be a value between 0 and 130 inclusively
    - Can not be changed once set
  * cutoff_sustain_level:
    - doc: The sustain cutoff (value of cutoff at sustain time) as a MIDI note.
    - default: cutoff
    - constraints: must be a value between 0 and 130 inclusively
    - Can not be changed once set
  * cutoff_env_curve:
    - doc: Select the shape of the curve between levels in the cutoff envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff_min:
    - doc: The minimum cutoff value.
    - default: 30
    - constraints: must be a value less than or equal to 130
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * rate:
    - doc: Rate with which to play back - default is 1. Playing the sample at rate 2 will play it back at double the normal speed. This will have the effect of doubling the frequencies in the sample and halving the playback time. Use rates lower than 1 to slow the sample down. Negative rates will play the sample in reverse.
    - default: 1
    - constraints: must not be zero
    - Can not be changed once set
  * start:
    - doc: A fraction (between 0 and 1) representing where in the sample to start playback. 1 represents the end of the sample, 0.5 half-way through etc.
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * finish:
    - doc: A fraction (between 0 and 1) representing where in the sample to finish playback. 1 represents the end of the sample, 0.5 half-way through etc.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * res:
    - doc: Filter resonance as a value between 0 and 1. Only functional if a cutoff value is specified. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * norm:
    - doc: Normalise the audio (make quieter parts of the sample louder and louder parts quieter) - this is similar to the normaliser FX. This may emphasise any clicks caused by clipping.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * pitch:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set
    - Has slide parameters for shaping changes
  * window_size:
    - doc: Pitch shift works by chopping the input into tiny slices, then playing these slices at a higher or lower rate. If we make the slices small enough and overlap them, it sounds like the original sound with the pitch changed.

  The window_size is the length of the slices and is measured in seconds. It needs to be around 0.2 (200ms) or greater for pitched sounds like guitar or bass, and needs to be around 0.02 (20ms) or lower for percussive sounds like drum loops. You can experiment with this to get the best sound for your input.
    - default: 0.2
    - constraints: must be a value greater than 5.0e-05
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
  * compress:
    - doc: Enable the compressor. This sits at the end of the internal FX chain immediately before the `amp:` opt. Therefore to drive the compressor use the `pre_amp:` opt which will amplify the signal before it hits any internal FX. The compressor compresses the dynamic range of the incoming signal. Equivalent to automatically turning the amp down when the signal gets too loud and then back up again when it's quiet. Useful for ensuring the containing signal doesn't overwhelm other aspects of the sound. Also a general purpose hard-knee dynamic range processor which can be tuned via the opts to both expand and compress the signal.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * threshold:
    - doc: Threshold value determining the break point between slope_below and slope_above. Only valid if the compressor is enabled by turning on the comp: opt.
    - default: 0.2
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * clamp_time:
    - doc: Time taken for the amplitude adjustments to kick in fully (in seconds). This is usually pretty small (not much more than 10 milliseconds). Also known as the time of the attack phase. Only valid if the compressor is enabled by turning on the comp: opt.
    - default: 0.01
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * slope_above:
    - doc: Slope of the amplitude curve above the threshold. A value of 1 means that the output of signals with amplitude above the threshold will be unaffected. Greater values will magnify and smaller values will attenuate the signal. Only valid if the compressor is enabled by turning on the comp: opt.
    - default: 0.5
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * slope_below:
    - doc: Slope of the amplitude curve below the threshold. A value of 1 means that the output of signals with amplitude below the threshold will be unaffected. Greater values will magnify and smaller values will attenuate the signal. Only valid if the compressor is enabled by turning on the comp: opt.
    - default: 1
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * relax_time:
    - doc: Time taken for the amplitude adjustments to be released. Usually a little longer than clamp_time. If both times are too short, you can get some (possibly unwanted) artefacts. Also known as the time of the release phase. Only valid if the compressor is enabled by turning on the comp: opt.
    - default: 0.01
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Stereo Sample Player

### Key:
  :stereo_player

### Doc:
  

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: Amplitude multiplier which takes place immediately before any internal FX such as the low pass filter, compressor or pitch modification. Use this opt if you want to overload the compressor.
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
  * attack:
    - doc: Duration of the attack phase of the envelope.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay:
    - doc: Duration of the decay phase of the envelope.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain:
    - doc: Duration of the sustain phase of the envelope. When -1 (the default) will auto-stretch.
    - default: -1
    - constraints: must either be a positive value or -1
    - Can not be changed once set
  * release:
    - doc: Duration of the release phase of the envelope.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff_attack:
    - doc: Attack time for cutoff filter. Amount of time (in beats) for sound to reach full cutoff value. Default value is set to match amp envelope's attack value.
    - default: attack
    - constraints: must be zero or greater
    - Can not be changed once set
  * cutoff_decay:
    - doc: Decay time for cutoff filter. Amount of time (in beats) for sound to move from full cutoff value (cutoff attack level) to the cutoff sustain level. Default value is set to match amp envelope's decay value.
    - default: decay
    - constraints: must be zero or greater
    - Can not be changed once set
  * cutoff_sustain:
    - doc: Amount of time for cutoff value to remain at sustain level in beats. When -1 (the default) will auto-stretch.
    - default: sustain
    - constraints: must either be a positive value or -1
    - Can not be changed once set
  * cutoff_release:
    - doc: Amount of time (in beats) for sound to move from cutoff sustain value to cutoff min value. Default value is set to match amp envelope's release value.
    - default: release
    - constraints: must be zero or greater
    - Can not be changed once set
  * cutoff_attack_level:
    - doc: The peak cutoff (value of cutoff at peak of attack) as a MIDI note.
    - default: cutoff
    - constraints: must be a value between 0 and 130 inclusively
    - Can not be changed once set
  * cutoff_decay_level:
    - doc: The level of cutoff after the decay phase as a MIDI note.
    - default: cutoff
    - constraints: must be a value between 0 and 130 inclusively
    - Can not be changed once set
  * cutoff_sustain_level:
    - doc: The sustain cutoff (value of cutoff at sustain time) as a MIDI note.
    - default: cutoff
    - constraints: must be a value between 0 and 130 inclusively
    - Can not be changed once set
  * cutoff_env_curve:
    - doc: Select the shape of the curve between levels in the cutoff envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff_min:
    - doc: The minimum cutoff value.
    - default: 30
    - constraints: must be a value less than or equal to 130
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * rate:
    - doc: Rate with which to play back - default is 1. Playing the sample at rate 2 will play it back at double the normal speed. This will have the effect of doubling the frequencies in the sample and halving the playback time. Use rates lower than 1 to slow the sample down. Negative rates will play the sample in reverse.
    - default: 1
    - constraints: must not be zero
    - Can not be changed once set
  * start:
    - doc: A fraction (between 0 and 1) representing where in the sample to start playback. 1 represents the end of the sample, 0.5 half-way through etc.
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * finish:
    - doc: A fraction (between 0 and 1) representing where in the sample to finish playback. 1 represents the end of the sample, 0.5 half-way through etc.
    - default: 1
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * res:
    - doc: Filter resonance as a value between 0 and 1. Only functional if a cutoff value is specified. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * norm:
    - doc: Normalise the audio (make quieter parts of the sample louder and louder parts quieter) - this is similar to the normaliser FX. This may emphasise any clicks caused by clipping.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * pitch:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set
    - Has slide parameters for shaping changes
  * window_size:
    - doc: Pitch shift works by chopping the input into tiny slices, then playing these slices at a higher or lower rate. If we make the slices small enough and overlap them, it sounds like the original sound with the pitch changed.

  The window_size is the length of the slices and is measured in seconds. It needs to be around 0.2 (200ms) or greater for pitched sounds like guitar or bass, and needs to be around 0.02 (20ms) or lower for percussive sounds like drum loops. You can experiment with this to get the best sound for your input.
    - default: 0.2
    - constraints: must be a value greater than 5.0e-05
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
  * compress:
    - doc: Enable the compressor. This sits at the end of the internal FX chain immediately before the `amp:` opt. Therefore to drive the compressor use the `pre_amp:` opt which will amplify the signal before it hits any internal FX. The compressor compresses the dynamic range of the incoming signal. Equivalent to automatically turning the amp down when the signal gets too loud and then back up again when it's quiet. Useful for ensuring the containing signal doesn't overwhelm other aspects of the sound. Also a general purpose hard-knee dynamic range processor which can be tuned via the opts to both expand and compress the signal.
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing
  * threshold:
    - doc: Threshold value determining the break point between slope_below and slope_above. Only valid if the compressor is enabled by turning on the comp: opt.
    - default: 0.2
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * clamp_time:
    - doc: Time taken for the amplitude adjustments to kick in fully (in seconds). This is usually pretty small (not much more than 10 milliseconds). Also known as the time of the attack phase. Only valid if the compressor is enabled by turning on the comp: opt.
    - default: 0.01
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * slope_above:
    - doc: Slope of the amplitude curve above the threshold. A value of 1 means that the output of signals with amplitude above the threshold will be unaffected. Greater values will magnify and smaller values will attenuate the signal. Only valid if the compressor is enabled by turning on the comp: opt.
    - default: 0.5
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * slope_below:
    - doc: Slope of the amplitude curve below the threshold. A value of 1 means that the output of signals with amplitude below the threshold will be unaffected. Greater values will magnify and smaller values will attenuate the signal. Only valid if the compressor is enabled by turning on the comp: opt.
    - default: 1
    - constraints: none
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * relax_time:
    - doc: Time taken for the amplitude adjustments to be released. Usually a little longer than clamp_time. If both times are too short, you can get some (possibly unwanted) artefacts. Also known as the time of the release phase. Only valid if the compressor is enabled by turning on the comp: opt.
    - default: 0.01
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Blade Runner style strings

### Key:
  :blade

### Doc:
  Straight from the 70s, evoking the mists of Blade Runner, this simple electro-style string synth is based on filtered saw waves and a variable vibrato.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * vibrato_rate:
    - doc: Number of wobbles per second. For realism this should be between 6 and 8, maybe even faster for really high notes.
    - default: 6
    - constraints: must be a value greater than or equal to 0.0,must be a value less than or equal to 20.0
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * vibrato_depth:
    - doc: Amount of variation around the central note. 1 is the sensible maximum (but you can go up to 5 if you want a special effect), 0 would mean no vibrato. Works well around 0.15 but you can experiment.
    - default: 0.15
    - constraints: must be a value greater than or equal to 0.0,must be a value less than or equal to 5.0
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * vibrato_delay:
    - doc: How long in seconds before the vibrato kicks in.
    - default: 0.5
    - constraints: must be zero or greater
    - Can not be changed once set
  * vibrato_onset:
    - doc: How long in seconds before the vibrato reaches full power.
    - default: 0.1
    - constraints: must be zero or greater
    - Can not be changed once set


## SynthPiano

### Key:
  :piano

### Doc:
  A basic piano synthesiser. Note that due to the plucked nature of this synth the envelope opts such as `attack:`, `sustain:` and `release:` do not work as expected. They can only shorten the natural length of the note, not prolong it. Also, the `note:` opt will only honour whole tones.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`. Note that the piano synth can only play whole tones such as 60 and does not handle floats such as 60.3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * vel:
    - doc: Velocity of keypress. 
    - default: 0.2
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. With the piano synth, this opt can only have the effect of shortening the attack phase, not prolonging it.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level). With the piano synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. With the piano synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. With the piano synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * hard:
    - doc: Hardness of keypress. 
    - default: 0.5
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * stereo_width:
    - doc: Width of the stereo effect (which makes low notes sound towards the left, high notes towards the right). 0 to 1.
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set


## SynthPluck

### Key:
  :pluck

### Doc:
  A basic plucked string synthesiser that uses Karplus-Strong synthesis. Note that due to the plucked nature of this synth the envelope opts such as `attack:`, `sustain:` and `release:` do not work as expected. They can only shorten the natural length of the note, not prolong it. Also, the `note:` opt will only honour whole tones.

### Opts:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`. Note that the piano synth can only play whole tones such as 60 and does not handle floats such as 60.3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. With the piano synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. With the piano synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level). With the piano synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * noise_amp:
    - doc: Amplitude of source (pink) noise.
    - default: 0.8
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set
  * max_delay_time:
    - doc: Maximum length of the delay line buffer.
    - default: 0.125
    - constraints: must be a value between 0.125 and 1 inclusively
    - Can not be changed once set
  * pluck_decay:
    - doc: How long the pluck takes to stabilise on a note. This doesn't have a dramatic effect on the sound.
    - default: 30
    - constraints: must be a value between 1 and 100 inclusively
    - Can not be changed once set
  * coef:
    - doc: Coefficient of the internal OnePole filter. Values around zero are resonant and bright, values towards 1 sound more dampened and cutoff. It's a little bit like playing nearer the soundhole/fingerboard for values near zero and more toward the bridge for values approaching one, although this isn't an exact comparison.
    - default: 0.3
    - constraints: must be a value between -1 and 1 inclusively
    - Can not be changed once set


## Sound In

### Key:
  :sound_in

### Doc:
  Please write documentation!

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * input:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set


## Noise

### Key:
  :noise

### Doc:
  Noise that contains equal amounts of energy at every frequency - comparable to radio static. Useful for generating percussive sounds such as snares and hand claps. Also useful for simulating wind or sea effects.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 110
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Pink Noise

### Key:
  :pnoise

### Doc:
  Noise whose spectrum falls off in power by 3 dB per octave. Useful for generating percussive sounds such as snares and hand claps. Also useful for simulating wind or sea effects.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 110
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Brown Noise

### Key:
  :bnoise

### Doc:
  Noise whose spectrum falls off in power by 6 dB per octave. Useful for generating percussive sounds such as snares and hand claps. Also useful for simulating wind or sea effects.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 110
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Grey Noise

### Key:
  :gnoise

### Doc:
  Generates noise which results from flipping random bits in a word. The spectrum is emphasised towards lower frequencies. Useful for generating percussive sounds such as snares and hand claps. Also useful for simulating wind or sea effects.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 110
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Clip Noise

### Key:
  :cnoise

### Doc:
  Generates noise whose values are either -1 or 1. This produces the maximum energy for the least peak to peak amplitude. Useful for generating percussive sounds such as snares and hand claps. Also useful for simulating wind or sea effects.

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * attack:
    - doc: Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * decay:
    - doc: Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * sustain:
    - doc: Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * release:
    - doc: Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
    - Scaled with current BPM value
  * attack_level:
    - doc: Amplitude level reached after attack phase and immediately before decay phase
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * decay_level:
    - doc: Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set
    - default: sustain_level
    - constraints: must be zero or greater
    - Can not be changed once set
  * sustain_level:
    - doc: Amplitude level reached after decay phase and immediately before release phase.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set
  * env_curve:
    - doc: Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed
    - default: 2
    - constraints: must be one of the following values: [1, 2, 3, 4, 6, 7]
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 110
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Basic Mono Sample Player (no env)

### Key:
  :basic_mono_player

### Doc:
  

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * rate:
    - doc: write me
    - default: 1
    - constraints: must not be zero
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Basic Stereo Sample Player (no env)

### Key:
  :basic_stereo_player

### Doc:
  

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
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
  * rate:
    - doc: write me
    - default: 1
    - constraints: must not be zero
    - Can not be changed once set
  * cutoff:
    - doc: MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 131
    - May be changed whilst playing
    - Has slide parameters for shaping changes
  * res:
    - doc: Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.
    - default: 0
    - constraints: must be zero or greater,must be a value less than 1
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Basic Mixer

### Key:
  :basic_mixer

### Doc:
  Please write documentation!

### Opts:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing
    - Has slide parameters for shaping changes


## Main Mixer

### Key:
  :main_mixer

### Doc:
  Please write documentation!

### Opts:
  * amp:
    - doc: write me
    - default: 1
    - constraints: none
    - Can not be changed once set
    - Has slide parameters for shaping changes
  * pre_amp:
    - doc: write me
    - default: 1
    - constraints: none
    - Can not be changed once set
    - Has slide parameters for shaping changes
  * hpf:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set
    - Has slide parameters for shaping changes
  * lpf:
    - doc: write me
    - default: 135.5
    - constraints: none
    - Can not be changed once set
    - Has slide parameters for shaping changes
  * hpf_bypass:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set
  * lpf_bypass:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set
  * force_mono:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set
  * invert_stereo:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set
  * limiter_bypass:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set
  * leak_dc_bypass:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set


