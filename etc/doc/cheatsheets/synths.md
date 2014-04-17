# Synths

* [Dull Bell](#dull-bell)
* [Pretty Bell](#pretty-bell)
* [Saw Wave](#saw-wave)
* [Sine Wave](#sine-wave)
* [Detuned Saw wave](#detuned-saw-wave)
* [Basic FM synthesis](#basic-fm-synthesis)
* [Modulated Saw Wave](#modulated-saw-wave)
* [Simple Modulated Saw Wave](#simple-modulated-saw-wave)
* [Modulated Detuned Saw Waves](#modulated-detuned-saw-waves)
* [Modulated Detuned Saw Waves Simple](#modulated-detuned-saw-waves-simple)
* [Modulated Sine Wave](#modulated-sine-wave)
* [Simple Modualted Sine Wave](#simple-modualted-sine-wave)
* [Modulated Triangle Wave](#modulated-triangle-wave)
* [Simple Modulated Triangle Wave](#simple-modulated-triangle-wave)
* [Modulated Pulse](#modulated-pulse)
* [Simple Modulated Pulse](#simple-modulated-pulse)
* [TB-303 Emulation](#tb-303-emulation)
* [Supersaw](#supersaw)
* [Supersaw Simple](#supersaw-simple)
* [The Prophet](#the-prophet)
* [Zawa](#zawa)
* [Mono Sample Player](#mono-sample-player)
* [Stereo Sample Player](#stereo-sample-player)
* [Basic Mono Sample Player - (no envelope)](#basic-mono-sample-player---(no-envelope))
* [Basic Stereo Sample Player - (no envelope)](#basic-stereo-sample-player---(no-envelope))

## Dull Bell

### Key:
  :dull_bell

### Doc:
  A simple dull dischordant bell sound.

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set



## Pretty Bell

### Key:
  :pretty_bell

### Doc:
  A simple pretty bell sound.

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set



## Saw Wave

### Key:
  :saw_beep

### Doc:
  A simple saw wave with a low pass filter.

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.1
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 0.3
    - constraints: must be zero or greater
    - Can not be changed once set



## Sine Wave

### Key:
  :beep

### Doc:
  A simple pure sine wave.

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.1
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 0.3
    - constraints: must be zero or greater
    - Can not be changed once set



## Detuned Saw wave

### Key:
  :dsaw

### Doc:
  A pair of detuned saw waves with a lop pass filter.

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 0.2
    - constraints: must be zero or greater
    - Can not be changed once set

  * cutoff:
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing

  * cutoff_slide:
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * detune:
    - doc: Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds.
    - default: 0.1
    - constraints: none
    - May be changed whilst playing

  * detune_slide:
    - doc: Amount of time (in seconds) for the detune value to change. A long detune_slide value means that the detune takes a long time to slide from the previous value to the new value. A detune_slide of 0 means that the detune instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing



## Basic FM synthesis

### Key:
  :fm

### Doc:
  A sine wave with a fundamental frequency which is modulated at audio rate by another sine wave with a specific modulation division and depth.

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set

  * divisor:
    - doc: Modifies the frequency of the modulator oscillator relative to the carrier. Don't worry too much about what this means - just try different numbers out!
    - default: 2
    - constraints: none
    - May be changed whilst playing

  * divisor_slide:
    - doc: Amount of time (in seconds) for the divisor value to change. A long divisor_slide value means that the divisor takes a long time to slide from the previous value to the new value. A divisor_slide of 0 means that the divisor instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * depth:
    - doc: Modifies the depth of the carrier wave used to modify fundamental frequency. Don't worry too much about what this means - just try different numbers out!
    - default: 1
    - constraints: none
    - May be changed whilst playing

  * depth_slide:
    - doc: Amount of time (in seconds) for the depth value to change. A long depth_slide value means that the depth takes a long time to slide from the previous value to the new value. A depth_slide of 0 means that the depth instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing



## Modulated Saw Wave

### Key:
  :mod_saw

### Doc:
  A saw wave which modulates between two separate notes.

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * cutoff:
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing

  * cutoff_slide:
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_rate:
    - doc: Number of times per second that the note switches between the two notes.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_rate_slide:
    - doc: Amount of time (in seconds) for the mod_rate value to change. A long mod_rate_slide value means that the mod_rate takes a long time to slide from the previous value to the new value. A mod_rate_slide of 0 means that the mod_rate instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range__slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * mod_width:
    - doc: The phase width of the modulation. Represents how even the gap between modulations is.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing

  * mod_width_slide:
    - doc: Amount of time (in seconds) for the mod_width value to change. A long mod_width_slide value means that the mod_width takes a long time to slide from the previous value to the new value. A mod_width_slide of 0 means that the mod_width instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing



## Simple Modulated Saw Wave

### Key:
  :mod_saw_s

### Doc:
  

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * mod_rate:
    - doc: Number of times per second that the note switches between the two notes.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_width:
    - doc: The phase width of the modulation. Represents how even the gap between modulations is.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing



## Modulated Detuned Saw Waves

### Key:
  :mod_dsaw

### Doc:
  

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * cutoff:
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing

  * cutoff_slide:
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_rate:
    - doc: Number of times per second that the note switches between the two notes.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_width:
    - doc: The phase width of the modulation. Represents how even the gap between modulations is.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing

  * detune:
    - doc: Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds.
    - default: 0.1
    - constraints: none
    - May be changed whilst playing



## Modulated Detuned Saw Waves Simple

### Key:
  :mod_dsaw_s

### Doc:
  

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * mod_rate:
    - doc: Number of times per second that the note switches between the two notes.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_rate_slide:
    - doc: Amount of time (in seconds) for the mod_rate value to change. A long mod_rate_slide value means that the mod_rate takes a long time to slide from the previous value to the new value. A mod_rate_slide of 0 means that the mod_rate instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range_slide:
    - doc: Amount of time (in seconds) for the mod_range value to change. A long mod_range_slide value means that the mod_range takes a long time to slide from the previous value to the new value. A mod_range_slide of 0 means that the mod_range instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_width:
    - doc: The phase width of the modulation. Represents how even the gap between modulations is.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing

  * mod_width_slide:
    - doc: Amount of time (in seconds) for the mod_width value to change. A long mod_width_slide value means that the mod_width takes a long time to slide from the previous value to the new value. A mod_width_slide of 0 means that the mod_width instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * detune:
    - doc: Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds.
    - default: 0.1
    - constraints: none
    - May be changed whilst playing

  * detune_slide:
    - doc: Amount of time (in seconds) for the detune value to change. A long detune_slide value means that the detune takes a long time to slide from the previous value to the new value. A detune_slide of 0 means that the detune instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing



## Modulated Sine Wave

### Key:
  :mod_sine

### Doc:
  

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * cutoff:
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing

  * cutoff_slide:
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_rate:
    - doc: Number of times per second that the note switches between the two notes.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_rate_slide:
    - doc: Amount of time (in seconds) for the mod_rate value to change. A long mod_rate_slide value means that the mod_rate takes a long time to slide from the previous value to the new value. A mod_rate_slide of 0 means that the mod_rate instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range_slide:
    - doc: Amount of time (in seconds) for the mod_range value to change. A long mod_range_slide value means that the mod_range takes a long time to slide from the previous value to the new value. A mod_range_slide of 0 means that the mod_range instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_width:
    - doc: The phase width of the modulation. Represents how even the gap between modulations is.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing

  * mod_width_slide:
    - doc: Amount of time (in seconds) for the mod_width value to change. A long mod_width_slide value means that the mod_width takes a long time to slide from the previous value to the new value. A mod_width_slide of 0 means that the mod_width instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing



## Simple Modualted Sine Wave

### Key:
  :mod_sine_s

### Doc:
  

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * mod_rate:
    - doc: Number of times per second that the note switches between the two notes.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_rate_slide:
    - doc: Amount of time (in seconds) for the mod_rate value to change. A long mod_rate_slide value means that the mod_rate takes a long time to slide from the previous value to the new value. A mod_rate_slide of 0 means that the mod_rate instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range_slide:
    - doc: Amount of time (in seconds) for the mod_range value to change. A long mod_range_slide value means that the mod_range takes a long time to slide from the previous value to the new value. A mod_range_slide of 0 means that the mod_range instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_width:
    - doc: The phase width of the modulation. Represents how even the gap between modulations is.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing

  * mod_width_slide:
    - doc: Amount of time (in seconds) for the mod_width value to change. A long mod_width_slide value means that the mod_width takes a long time to slide from the previous value to the new value. A mod_width_slide of 0 means that the mod_width instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing



## Modulated Triangle Wave

### Key:
  :mod_tri

### Doc:
  

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * cutoff:
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing

  * cutoff_slide:
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_rate:
    - doc: Number of times per second that the note switches between the two notes.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_rate_slide:
    - doc: Amount of time (in seconds) for the mod_rate value to change. A long mod_rate_slide value means that the mod_rate takes a long time to slide from the previous value to the new value. A mod_rate_slide of 0 means that the mod_rate instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range_slide:
    - doc: Amount of time (in seconds) for the mod_range value to change. A long mod_range_slide value means that the mod_range takes a long time to slide from the previous value to the new value. A mod_range_slide of 0 means that the mod_range instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_width:
    - doc: The phase width of the modulation. Represents how even the gap between modulations is.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing

  * mod_width_slide:
    - doc: Amount of time (in seconds) for the mod_width value to change. A long mod_width_slide value means that the mod_width takes a long time to slide from the previous value to the new value. A mod_width_slide of 0 means that the mod_width instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing



## Simple Modulated Triangle Wave

### Key:
  :mod_tri_s

### Doc:
  

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * mod_rate:
    - doc: Number of times per second that the note switches between the two notes.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_width:
    - doc: The phase width of the modulation. Represents how even the gap between modulations is.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing



## Modulated Pulse

### Key:
  :mod_pulse

### Doc:
  

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * cutoff:
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing

  * cutoff_slide:
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_rate:
    - doc: Number of times per second that the note switches between the two notes.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_rate__slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range_slide:
    - doc: Amount of time (in seconds) for the mod_range value to change. A long mod_range_slide value means that the mod_range takes a long time to slide from the previous value to the new value. A mod_range_slide of 0 means that the mod_range instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_width:
    - doc: The phase width of the modulation. Represents how even the gap between modulations is.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing

  * mod_width_slide:
    - doc: Amount of time (in seconds) for the mod_width value to change. A long mod_width_slide value means that the mod_width takes a long time to slide from the previous value to the new value. A mod_width_slide of 0 means that the mod_width instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pulse_width:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * pulse_width_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## Simple Modulated Pulse

### Key:
  :mod_pulse_s

### Doc:
  

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * mod_rate:
    - doc: Number of times per second that the note switches between the two notes.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_range:
    - doc: The size of gap between modulation notes. A gap of 12 is one octave.
    - default: 5
    - constraints: must be zero or greater
    - May be changed whilst playing

  * mod_width:
    - doc: The phase width of the modulation. Represents how even the gap between modulations is.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing

  * pulse_width:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set



## TB-303 Emulation

### Key:
  :tb303

### Doc:
  

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * cutoff:
    - doc: 
    - default: 80
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing

  * cutoff_slide:
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * cutoff_min:
    - doc: 
    - default: 30
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing

  * res:
    - doc: write me
    - default: 0.1
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * wave:
    - doc: Wave type - 0 saw, 1 pulse
    - default: 0
    - constraints: must be one of the following values: [0, 1]
    - May be changed whilst playing

  * pulse_width:
    - doc: Only valid if wave is type pulse.
    - default: 0.5
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pulse_width_slide:
    - doc: Time in seconds for pulse width to change. Only valid if wave is type pulse.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing



## Supersaw

### Key:
  :supersaw

### Doc:
  

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * cutoff:
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 130
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing

  * cutoff_slide:
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * res:
    - doc: write me
    - default: 0.3
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## Supersaw Simple

### Key:
  :supersaw_s

### Doc:
  

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set



## The Prophet

### Key:
  :prophet

### Doc:
  Dark and swirly, this synth uses Pulse Width Modulation
      (PWM) to create a timbre which continually moves around. This
      effect is created using the pulse ugen which produces a variable
      width square wave. We then control the width of the pulses using a
      variety of LFOs - sin-osc and lf-tri in this case. We use a number
      of these LFO modulated pulse ugens with varying LFO type and rate
      (and phase in some cases to provide the LFO with a different
      starting point. We then mix all these pulses together to create a
      thick sound and then feed it through a resonant low pass filter
      (rlpf).

      For extra bass, one of the pulses is an octave lower (half the
      frequency) and its LFO has a little bit of randomisation thrown
      into its frequency component for that extra bit of variety.

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.01
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 2
    - constraints: must be zero or greater
    - Can not be changed once set

  * cutoff:
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 110
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing

  * cutoff_slide:
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * res:
    - doc: write me
    - default: 0.3
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## Zawa

### Key:
  :zawa

### Doc:
  Write me

### Arguments:
  * note:
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3
    - default: 52
    - constraints: must be zero or greater
    - May be changed whilst playing

  * note_slide:
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.
    - default: 0.1
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.
    - default: 1
    - constraints: must be zero or greater
    - Can not be changed once set

  * cutoff:
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.
    - default: 100
    - constraints: must be zero or greater,must be a value less than 130
    - May be changed whilst playing

  * cutoff_slide:
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * rate:
    - doc: write me
    - default: 1
    - constraints: none
    - Can not be changed once set

  * rate_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * depth:
    - doc: write me
    - default: 1.5
    - constraints: none
    - Can not be changed once set

  * depth_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## Mono Sample Player

### Key:
  :mono_player

### Doc:
  

### Arguments:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: 
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: 
    - default: -1
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: 
    - default: 0
    - constraints: must either be a positive value or -1
    - Can not be changed once set

  * rate:
    - doc: 
    - default: 1
    - constraints: none
    - Can not be changed once set

  * start:
    - doc: 
    - default: 0
    - constraints: must be zero or greater,must be a value between 0 and 1 inclusively
    - Can not be changed once set

  * finish:
    - doc: 
    - default: 1
    - constraints: must be zero or greater,must be a value between 0 and 1 inclusively
    - Can not be changed once set



## Stereo Sample Player

### Key:
  :stereo_player

### Doc:
  

### Arguments:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * attack:
    - doc: 
    - default: 0
    - constraints: must be zero or greater
    - Can not be changed once set

  * sustain:
    - doc: 
    - default: -1
    - constraints: must be zero or greater
    - Can not be changed once set

  * release:
    - doc: 
    - default: 0
    - constraints: must either be a positive value or -1
    - Can not be changed once set

  * rate:
    - doc: 
    - default: 1
    - constraints: none
    - Can not be changed once set

  * start:
    - doc: 
    - default: 0
    - constraints: must be zero or greater,must be a value between 0 and 1 inclusively
    - Can not be changed once set

  * finish:
    - doc: 
    - default: 1
    - constraints: must be zero or greater,must be a value between 0 and 1 inclusively
    - Can not be changed once set



## Basic Mono Sample Player - (no envelope)

### Key:
  :basic_mono_player

### Doc:
  

### Arguments:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * rate:
    - doc: write me
    - default: 1
    - constraints: none
    - Can not be changed once set

  * rate_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## Basic Stereo Sample Player - (no envelope)

### Key:
  :basic_stereo_player

### Doc:
  

### Arguments:
  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * pan:
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.
    - default: 0
    - constraints: must be a value between -1 and 1 inclusively
    - May be changed whilst playing

  * pan_slide:
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * rate:
    - doc: write me
    - default: 1
    - constraints: none
    - Can not be changed once set

  * rate_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



