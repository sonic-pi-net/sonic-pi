# Synths

* [Pretty Bell](#pretty-bell)

## Dull Bell

### Key: 
  :dull_bell 

### Doc: 
  A simple dull dischordant bell sound.

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 1 
    - constraints: must be zero or greater
    - Not Modulatable  



## Pretty Bell

### Key: 
  :pretty_bell 

### Doc: 
  A simple pretty bell sound.

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 1 
    - constraints: must be zero or greater
    - Not Modulatable  



## Saw Wave

### Key: 
  :saw_beep 

### Doc: 
  A simple saw wave with a low pass filter.

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.1 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 0.3 
    - constraints: must be zero or greater
    - Not Modulatable  



## Sine Wave

### Key: 
  :beep 

### Doc: 
  A simple pure sine wave.

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.1 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 0.3 
    - constraints: must be zero or greater
    - Not Modulatable  



## Detuned Saw wave

### Key: 
  :dsaw 

### Doc: 
  A pair of detuned saw waves with a lop pass filter.

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.1 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 0.3 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - constraints: must be zero or greater,must be a value less than 130
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :detune
    - doc: Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds. 
    - default: 0.1 
    - constraints: none
    - Modulatable  

  * :detune_slide
    - doc: Amount of time (in seconds) for the detune value to change. A long detune_slide value means that the detune takes a long time to slide from the previous value to the new value. A detune_slide of 0 means that the detune instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  



## Basic FM synthesis

### Key: 
  :fm 

### Doc: 
  A sine wave with a fundamental frequency which is modulated at audio rate by another sine wave with a specific modulation division and depth.

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 1 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 1 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :divisor
    - doc: Modifies the frequency of the modulator oscillator relative to the carrier. Don't worry too much about what this means - just try different numbers out! 
    - default: 2 
    - constraints: none
    - Modulatable  

  * :divisor_slide
    - doc: Amount of time (in seconds) for the divisor value to change. A long divisor_slide value means that the divisor takes a long time to slide from the previous value to the new value. A divisor_slide of 0 means that the divisor instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :depth
    - doc: Modifies the depth of the carrier wave used to modify fundamental frequency. Don't worry too much about what this means - just try different numbers out! 
    - default: 1 
    - constraints: none
    - Modulatable  

  * :depth_slide
    - doc: Amount of time (in seconds) for the depth value to change. A long depth_slide value means that the depth takes a long time to slide from the previous value to the new value. A depth_slide of 0 means that the depth instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  



## Modulated Saw Wave

### Key: 
  :mod_saw 

### Doc: 
  A saw wave which modulates between two separate notes.

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - constraints: must be zero or greater,must be a value less than 130
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_rate
    - doc: Number of times per second that the note switches between the two notes. 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_rate_slide
    - doc: Amount of time (in seconds) for the mod_rate value to change. A long mod_rate_slide value means that the mod_rate takes a long time to slide from the previous value to the new value. A mod_rate_slide of 0 means that the mod_rate instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range
    - doc: The size of gap between modulation notes. A gap of 12 is one octave. 
    - default: 5 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range__slide
    - doc:  
    - default: 0 
    - constraints: none
    - Not Modulatable  

  * :mod_width
    - doc: The phase width of the modulation. Represents how even the gap between modulations is. 
    - default: 0.5 
    - constraints: must be a value between 0 and 1 exclusively
    - Modulatable  

  * :mod_width_slide
    - doc: Amount of time (in seconds) for the mod_width value to change. A long mod_width_slide value means that the mod_width takes a long time to slide from the previous value to the new value. A mod_width_slide of 0 means that the mod_width instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  



## Modulated Saw Wave (Simple)

### Key: 
  :mod_saw_s 

### Doc: 
  

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :slide
    - doc:  
    - default: 0 
    - constraints: none
    - Not Modulatable  

  * :mod_rate
    - doc: Number of times per second that the note switches between the two notes. 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range
    - doc: The size of gap between modulation notes. A gap of 12 is one octave. 
    - default: 5 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_width
    - doc: The phase width of the modulation. Represents how even the gap between modulations is. 
    - default: 0.5 
    - constraints: must be a value between 0 and 1 exclusively
    - Modulatable  



## Modulated Detuned Saw Waves

### Key: 
  :mod_dsaw 

### Doc: 
  

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :slide
    - doc:  
    - default: 0 
    - constraints: none
    - Not Modulatable  

  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - constraints: must be zero or greater,must be a value less than 130
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_rate
    - doc: Number of times per second that the note switches between the two notes. 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range
    - doc: The size of gap between modulation notes. A gap of 12 is one octave. 
    - default: 5 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_width
    - doc: The phase width of the modulation. Represents how even the gap between modulations is. 
    - default: 0.5 
    - constraints: must be a value between 0 and 1 exclusively
    - Modulatable  

  * :detune
    - doc: Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds. 
    - default: 0.1 
    - constraints: none
    - Modulatable  



## Modulated Detuned Saw Waves Simple

### Key: 
  :mod_dsaw_s 

### Doc: 
  

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :mod_rate
    - doc: Number of times per second that the note switches between the two notes. 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_rate_slide
    - doc: Amount of time (in seconds) for the mod_rate value to change. A long mod_rate_slide value means that the mod_rate takes a long time to slide from the previous value to the new value. A mod_rate_slide of 0 means that the mod_rate instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range
    - doc: The size of gap between modulation notes. A gap of 12 is one octave. 
    - default: 5 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range_slide
    - doc: Amount of time (in seconds) for the mod_range value to change. A long mod_range_slide value means that the mod_range takes a long time to slide from the previous value to the new value. A mod_range_slide of 0 means that the mod_range instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_width
    - doc: The phase width of the modulation. Represents how even the gap between modulations is. 
    - default: 0.5 
    - constraints: must be a value between 0 and 1 exclusively
    - Modulatable  

  * :mod_width_slide
    - doc: Amount of time (in seconds) for the mod_width value to change. A long mod_width_slide value means that the mod_width takes a long time to slide from the previous value to the new value. A mod_width_slide of 0 means that the mod_width instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :detune
    - doc: Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds. 
    - default: 0.1 
    - constraints: none
    - Modulatable  

  * :detune_slide
    - doc: Amount of time (in seconds) for the detune value to change. A long detune_slide value means that the detune takes a long time to slide from the previous value to the new value. A detune_slide of 0 means that the detune instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  



## Modualted Sine Wave

### Key: 
  :mod_sine 

### Doc: 
  

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - constraints: must be zero or greater,must be a value less than 130
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_rate
    - doc: Number of times per second that the note switches between the two notes. 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_rate_slide
    - doc: Amount of time (in seconds) for the mod_rate value to change. A long mod_rate_slide value means that the mod_rate takes a long time to slide from the previous value to the new value. A mod_rate_slide of 0 means that the mod_rate instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range
    - doc: The size of gap between modulation notes. A gap of 12 is one octave. 
    - default: 5 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range_slide
    - doc: Amount of time (in seconds) for the mod_range value to change. A long mod_range_slide value means that the mod_range takes a long time to slide from the previous value to the new value. A mod_range_slide of 0 means that the mod_range instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_width
    - doc: The phase width of the modulation. Represents how even the gap between modulations is. 
    - default: 0.5 
    - constraints: must be a value between 0 and 1 exclusively
    - Modulatable  

  * :mod_width_slide
    - doc: Amount of time (in seconds) for the mod_width value to change. A long mod_width_slide value means that the mod_width takes a long time to slide from the previous value to the new value. A mod_width_slide of 0 means that the mod_width instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  



## Modualted Sine Wave Simple

### Key: 
  :mod_sine_s 

### Doc: 
  

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :slide
    - doc:  
    - default: 0 
    - constraints: none
    - Not Modulatable  

  * :mod_rate
    - doc: Number of times per second that the note switches between the two notes. 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_rate_slide
    - doc: Amount of time (in seconds) for the mod_rate value to change. A long mod_rate_slide value means that the mod_rate takes a long time to slide from the previous value to the new value. A mod_rate_slide of 0 means that the mod_rate instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range
    - doc: The size of gap between modulation notes. A gap of 12 is one octave. 
    - default: 5 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range_slide
    - doc: Amount of time (in seconds) for the mod_range value to change. A long mod_range_slide value means that the mod_range takes a long time to slide from the previous value to the new value. A mod_range_slide of 0 means that the mod_range instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_width
    - doc: The phase width of the modulation. Represents how even the gap between modulations is. 
    - default: 0.5 
    - constraints: must be a value between 0 and 1 exclusively
    - Modulatable  

  * :mod_width_slide
    - doc: Amount of time (in seconds) for the mod_width value to change. A long mod_width_slide value means that the mod_width takes a long time to slide from the previous value to the new value. A mod_width_slide of 0 means that the mod_width instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  



## Modulated Triangle Wave

### Key: 
  :mod_tri 

### Doc: 
  

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - constraints: must be zero or greater,must be a value less than 130
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_rate
    - doc: Number of times per second that the note switches between the two notes. 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_rate_slide
    - doc: Amount of time (in seconds) for the mod_rate value to change. A long mod_rate_slide value means that the mod_rate takes a long time to slide from the previous value to the new value. A mod_rate_slide of 0 means that the mod_rate instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range
    - doc: The size of gap between modulation notes. A gap of 12 is one octave. 
    - default: 5 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range_slide
    - doc: Amount of time (in seconds) for the mod_range value to change. A long mod_range_slide value means that the mod_range takes a long time to slide from the previous value to the new value. A mod_range_slide of 0 means that the mod_range instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_width
    - doc: The phase width of the modulation. Represents how even the gap between modulations is. 
    - default: 0.5 
    - constraints: must be a value between 0 and 1 exclusively
    - Modulatable  

  * :mod_width_slide
    - doc: Amount of time (in seconds) for the mod_width value to change. A long mod_width_slide value means that the mod_width takes a long time to slide from the previous value to the new value. A mod_width_slide of 0 means that the mod_width instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  



## Modulated Triangle Wave Simple

### Key: 
  :mod_tri_s 

### Doc: 
  

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :slide
    - doc:  
    - default: 0 
    - constraints: none
    - Not Modulatable  

  * :mod_rate
    - doc: Number of times per second that the note switches between the two notes. 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range
    - doc: The size of gap between modulation notes. A gap of 12 is one octave. 
    - default: 5 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_width
    - doc: The phase width of the modulation. Represents how even the gap between modulations is. 
    - default: 0.5 
    - constraints: must be a value between 0 and 1 exclusively
    - Modulatable  



## Modulated Pulse

### Key: 
  :mod_pulse 

### Doc: 
  

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - constraints: must be zero or greater,must be a value less than 130
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_rate
    - doc: Number of times per second that the note switches between the two notes. 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_rate__slide
    - doc:  
    - default: 0 
    - constraints: none
    - Not Modulatable  

  * :mod_range
    - doc: The size of gap between modulation notes. A gap of 12 is one octave. 
    - default: 5 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range_slide
    - doc: Amount of time (in seconds) for the mod_range value to change. A long mod_range_slide value means that the mod_range takes a long time to slide from the previous value to the new value. A mod_range_slide of 0 means that the mod_range instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_width
    - doc: The phase width of the modulation. Represents how even the gap between modulations is. 
    - default: 0.5 
    - constraints: must be a value between 0 and 1 exclusively
    - Modulatable  

  * :mod_width_slide
    - doc: Amount of time (in seconds) for the mod_width value to change. A long mod_width_slide value means that the mod_width takes a long time to slide from the previous value to the new value. A mod_width_slide of 0 means that the mod_width instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pulse_width
    - doc:  
    - default: 0.5 
    - constraints: none
    - Not Modulatable  

  * :pulse_width_slide
    - doc:  
    - default: 0 
    - constraints: none
    - Not Modulatable  



## Modulated Pulse Simple

### Key: 
  :mod_pulse_s 

### Doc: 
  

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :mod_rate
    - doc: Number of times per second that the note switches between the two notes. 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_range
    - doc: The size of gap between modulation notes. A gap of 12 is one octave. 
    - default: 5 
    - constraints: must be zero or greater
    - Modulatable  

  * :mod_width
    - doc: The phase width of the modulation. Represents how even the gap between modulations is. 
    - default: 0.5 
    - constraints: must be a value between 0 and 1 exclusively
    - Modulatable  

  * :pulse_width
    - doc:  
    - default: 0.5 
    - constraints: none
    - Not Modulatable  



## tb-303

### Key: 
  :tb303 

### Doc: 
  

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :cutoff
    - doc:  
    - default: 80 
    - constraints: must be zero or greater,must be a value less than 130
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :cutoff_min
    - doc:  
    - default: 30 
    - constraints: must be zero or greater,must be a value less than 130
    - Modulatable  

  * :res
    - doc:  
    - default: 0.2 
    - constraints: none
    - Not Modulatable  

  * :res_slide
    - doc:  
    - default: 0 
    - constraints: none
    - Not Modulatable  

  * :wave
    - doc: Wave type - 0 saw, 1 pulse 
    - default: 0 
    - constraints: must be one of the following values: [0, 1]
    - Modulatable  

  * :pulse_width
    - doc: Only valid if wave is type pulse. 
    - default: 0.5 
    - constraints: must be zero or greater
    - Modulatable  

  * :pulse_width_slide
    - doc: Time in seconds for pulse width to change. Only valid if wave is type pulse. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  



## Supersaw

### Key: 
  :supersaw 

### Doc: 
  

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 130 
    - constraints: must be zero or greater,must be a value less than 130
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :res
    - doc:  
    - default: 0.3 
    - constraints: none
    - Not Modulatable  

  * :res_slide
    - doc:  
    - default: 0 
    - constraints: none
    - Not Modulatable  



## Supersaw Simple

### Key: 
  :supersaw_s 

### Doc: 
  

### Arguments:
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  



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
  * :note
    - doc: Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3 
    - default: 52 
    - constraints: must be zero or greater
    - Modulatable  

  * :note_slide
    - doc: Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc: Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release. 
    - default: 0.01 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc: Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release. 
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc: Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release. 
    - default: 2 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 110 
    - constraints: must be zero or greater,must be a value less than 130
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :res
    - doc:  
    - default: 0.3 
    - constraints: none
    - Not Modulatable  

  * :res_slide
    - doc:  
    - default: 0 
    - constraints: none
    - Not Modulatable  



## Mono Sample Player

### Key: 
  :mono_player 

### Doc: 
  

### Arguments:
  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc:  
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc:  
    - default: -1 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc:  
    - default: 0 
    - constraints: must either be a positive value or -1
    - Not Modulatable  

  * :rate
    - doc:  
    - default: 1 
    - constraints: none
    - Not Modulatable  

  * :start
    - doc:  
    - default: 0 
    - constraints: must be zero or greater,must be a value between 0 and 1 inclusively
    - Not Modulatable  

  * :end
    - doc:  
    - default: 1 
    - constraints: must be zero or greater,must be a value between 0 and 1 inclusively
    - Not Modulatable  



## Stereo Sample Player

### Key: 
  :stereo_player 

### Doc: 
  

### Arguments:
  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :attack
    - doc:  
    - default: 0 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :sustain
    - doc:  
    - default: -1 
    - constraints: must be zero or greater
    - Not Modulatable  

  * :release
    - doc:  
    - default: 0 
    - constraints: must either be a positive value or -1
    - Not Modulatable  

  * :rate
    - doc:  
    - default: 1 
    - constraints: none
    - Not Modulatable  

  * :start
    - doc:  
    - default: 0 
    - constraints: must be zero or greater,must be a value between 0 and 1 inclusively
    - Not Modulatable  

  * :end
    - doc:  
    - default: 1 
    - constraints: must be zero or greater,must be a value between 0 and 1 inclusively
    - Not Modulatable  



## Basic Mono Sample Player - (no envelope)

### Key: 
  :basic_mono_player 

### Doc: 
  

### Arguments:
  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :rate
    - doc:  
    - default: 1 
    - constraints: none
    - Not Modulatable  

  * :rate_slide
    - doc:  
    - default: 0 
    - constraints: none
    - Not Modulatable  



## Basic Stereo Sample Player - (no envelope)

### Key: 
  :basic_stereo_player 

### Doc: 
  

### Arguments:
  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.) 
    - default: 1 
    - constraints: must be zero or greater
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :pan
    - doc: Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly. 
    - default: 0 
    - constraints: must be a value between -1 and 1 inclusively
    - Modulatable  

  * :pan_slide
    - doc: Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position. 
    - default: 0 
    - constraints: must be zero or greater
    - Modulatable  

  * :rate
    - doc:  
    - default: 1 
    - constraints: none
    - Not Modulatable  

  * :rate_slide
    - doc:  
    - default: 0 
    - constraints: none
    - Not Modulatable  



