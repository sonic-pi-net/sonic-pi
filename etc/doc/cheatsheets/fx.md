# FX

* [FX Reverb](#fx-reverb)
* [FX Reverb](#fx-reverb)
* [FX Level Amplifier](#fx-level-amplifier)
* [FX Level Amplifier](#fx-level-amplifier)
* [FX Echo](#fx-echo)
* [FX Echo](#fx-echo)
* [FX Slicer](#fx-slicer)
* [FX Slicer](#fx-slicer)
* [FX Techno](#fx-techno)
* [FX Techno](#fx-techno)
* [FX Compressor](#fx-compressor)
* [FX Compressor](#fx-compressor)
* [FX Resonant Low Pass Filter](#fx-resonant-low-pass-filter)
* [FX Resonant Low Pass Filter](#fx-resonant-low-pass-filter)
* [FX Normalised Resonant Low Pass Filter](#fx-normalised-resonant-low-pass-filter)
* [FX Normalised Resonant Low Pass Filter](#fx-normalised-resonant-low-pass-filter)
* [FX Resonant High Pass Filter](#fx-resonant-high-pass-filter)
* [FX Resonant High Pass Filter](#fx-resonant-high-pass-filter)
* [FX Normalised Resonant High Pass Filter](#fx-normalised-resonant-high-pass-filter)
* [FX Normalised Resonant High Pass Filter](#fx-normalised-resonant-high-pass-filter)
* [FX High Pass Filter](#fx-high-pass-filter)
* [FX High Pass Filter](#fx-high-pass-filter)
* [FX Normalised High Pass Filter](#fx-normalised-high-pass-filter)
* [FX Normalised High Pass Filter](#fx-normalised-high-pass-filter)
* [FX Low Pass Filter](#fx-low-pass-filter)
* [FX Low Pass Filter](#fx-low-pass-filter)
* [FX Normalised Low Pass Filter](#fx-normalised-low-pass-filter)
* [FX Normalised Low Pass Filter](#fx-normalised-low-pass-filter)
* [FX Normaliser](#fx-normaliser)
* [FX Normaliser](#fx-normaliser)
* [FX Distortion](#fx-distortion)
* [FX Distortion](#fx-distortion)

## FX Reverb

### Key:
  :reverb

### Doc:
  Please write documentation!

### Arguments:
  * mix:
    - doc: write me
    - default: 0.4
    - constraints: none
    - Can not be changed once set

  * mix_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * room:
    - doc: write me
    - default: 0.6
    - constraints: none
    - Can not be changed once set

  * room_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * damp:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * damp_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Reverb

### Key:
  :replace_reverb

### Doc:
  Please write documentation!

### Arguments:
  * mix:
    - doc: write me
    - default: 0.4
    - constraints: none
    - Can not be changed once set

  * mix_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * room:
    - doc: write me
    - default: 0.6
    - constraints: none
    - Can not be changed once set

  * room_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * damp:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * damp_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Level Amplifier

### Key:
  :level

### Doc:
  Please write documentation!

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



## FX Level Amplifier

### Key:
  :replace_level

### Doc:
  Please write documentation!

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



## FX Echo

### Key:
  :echo

### Doc:
  Please write documentation!

### Arguments:
  * max_delay:
    - doc: The maximum delay time in seconds.
    - default: 1
    - constraints: must be greater than zero
    - Can not be changed once set

  * delay:
    - doc: The time between echoes in seconds.
    - default: 0.4
    - constraints: must be greater than zero
    - May be changed whilst playing

  * delay_slide:
    - doc: Slide time in seconds between delay values
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * decay:
    - doc: The time it takes for the echoes to fade away in seconds.
    - default: 8
    - constraints: must be greater than zero
    - May be changed whilst playing

  * decay_slide:
    - doc: Slide time in seconds between decay times
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing



## FX Echo

### Key:
  :replace_echo

### Doc:
  Please write documentation!

### Arguments:
  * max_delay:
    - doc: The maximum delay time in seconds.
    - default: 1
    - constraints: must be greater than zero
    - Can not be changed once set

  * delay:
    - doc: The time between echoes in seconds.
    - default: 0.4
    - constraints: must be greater than zero
    - May be changed whilst playing

  * delay_slide:
    - doc: Slide time in seconds between delay values
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * decay:
    - doc: The time it takes for the echoes to fade away in seconds.
    - default: 8
    - constraints: must be greater than zero
    - May be changed whilst playing

  * decay_slide:
    - doc: Slide time in seconds between decay times
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp:
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing



## FX Slicer

### Key:
  :slicer

### Doc:
  Please write documentation!

### Arguments:
  * rate:
    - doc: The frequency of the slices
    - default: 4
    - constraints: must be greater than zero
    - May be changed whilst playing

  * rate_slide:
    - doc: Slide time in seconds between rate values
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * width:
    - doc: The width of the slices - 0 - 1.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing

  * width_slide:
    - doc: Slide time in seconds between width values
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * phase:
    - doc: Initial phase.
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set

  * amp:
    - doc: The amplitude of the resulting effect.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: The slide lag time for amplitude changes.
    - default: 0.05
    - constraints: must be zero or greater
    - May be changed whilst playing



## FX Slicer

### Key:
  :replace_slicer

### Doc:
  Please write documentation!

### Arguments:
  * rate:
    - doc: The frequency of the slices
    - default: 4
    - constraints: must be greater than zero
    - May be changed whilst playing

  * rate_slide:
    - doc: Slide time in seconds between rate values
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * width:
    - doc: The width of the slices - 0 - 1.
    - default: 0.5
    - constraints: must be a value between 0 and 1 exclusively
    - May be changed whilst playing

  * width_slide:
    - doc: Slide time in seconds between width values
    - default: 0
    - constraints: must be zero or greater
    - May be changed whilst playing

  * phase:
    - doc: Initial phase.
    - default: 0
    - constraints: must be a value between 0 and 1 inclusively
    - Can not be changed once set

  * amp:
    - doc: The amplitude of the resulting effect.
    - default: 1
    - constraints: must be zero or greater
    - May be changed whilst playing

  * amp_slide:
    - doc: The slide lag time for amplitude changes.
    - default: 0.05
    - constraints: must be zero or greater
    - May be changed whilst playing



## FX Techno

### Key:
  :techno

### Doc:
  Please write documentation!

### Arguments:
  * rate:
    - doc: The frequency of filter modulation
    - default: 0.1
    - constraints: must be greater than zero
    - May be changed whilst playing

  * rate_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * cutoff_min:
    - doc: write me
    - default: 880
    - constraints: none
    - Can not be changed once set

  * cutoff_min_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * cutoff_max:
    - doc: write me
    - default: 12000
    - constraints: none
    - Can not be changed once set

  * cutoff_max_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * res:
    - doc: write me
    - default: 0.2
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Techno

### Key:
  :replace_techno

### Doc:
  Please write documentation!

### Arguments:
  * rate:
    - doc: The frequency of filter modulation
    - default: 0.1
    - constraints: must be greater than zero
    - May be changed whilst playing

  * rate_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * cutoff_min:
    - doc: write me
    - default: 880
    - constraints: none
    - Can not be changed once set

  * cutoff_min_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * cutoff_max:
    - doc: write me
    - default: 12000
    - constraints: none
    - Can not be changed once set

  * cutoff_max_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * res:
    - doc: write me
    - default: 0.2
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Compressor

### Key:
  :compressor

### Doc:
  Please write documentation!

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

  * threshold:
    - doc: write me
    - default: 0.2
    - constraints: none
    - Can not be changed once set

  * threshold_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * clamp_time:
    - doc: write me
    - default: 0.01
    - constraints: none
    - Can not be changed once set

  * clamp_time_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * slope_above:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * slope_above_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * slope_below:
    - doc: write me
    - default: 1
    - constraints: none
    - Can not be changed once set

  * slope_below_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * relax_time:
    - doc: write me
    - default: 0.01
    - constraints: none
    - Can not be changed once set

  * relax_time_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Compressor

### Key:
  :replace_compressor

### Doc:
  Please write documentation!

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

  * threshold:
    - doc: write me
    - default: 0.2
    - constraints: none
    - Can not be changed once set

  * threshold_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * clamp_time:
    - doc: write me
    - default: 0.01
    - constraints: none
    - Can not be changed once set

  * clamp_time_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * slope_above:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * slope_above_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * slope_below:
    - doc: write me
    - default: 1
    - constraints: none
    - Can not be changed once set

  * slope_below_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set

  * relax_time:
    - doc: write me
    - default: 0.01
    - constraints: none
    - Can not be changed once set

  * relax_time_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Resonant Low Pass Filter

### Key:
  :rlpf

### Doc:
  Please write documentation!

### Arguments:
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

  * res:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Resonant Low Pass Filter

### Key:
  :replace_rlpf

### Doc:
  Please write documentation!

### Arguments:
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

  * res:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Normalised Resonant Low Pass Filter

### Key:
  :norm_rlpf

### Doc:
  Please write documentation!

### Arguments:
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

  * res:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Normalised Resonant Low Pass Filter

### Key:
  :replace_norm_rlpf

### Doc:
  Please write documentation!

### Arguments:
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

  * res:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Resonant High Pass Filter

### Key:
  :rhpf

### Doc:
  Please write documentation!

### Arguments:
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

  * res:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Resonant High Pass Filter

### Key:
  :replace_rhpf

### Doc:
  Please write documentation!

### Arguments:
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

  * res:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Normalised Resonant High Pass Filter

### Key:
  :norm_rhpf

### Doc:
  Please write documentation!

### Arguments:
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

  * res:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Normalised Resonant High Pass Filter

### Key:
  :replace_norm_rhpf

### Doc:
  Please write documentation!

### Arguments:
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

  * res:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX High Pass Filter

### Key:
  :hpf

### Doc:
  Please write documentation!

### Arguments:
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



## FX High Pass Filter

### Key:
  :replace_hpf

### Doc:
  Please write documentation!

### Arguments:
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



## FX Normalised High Pass Filter

### Key:
  :norm_hpf

### Doc:
  Please write documentation!

### Arguments:
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

  * res:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Normalised High Pass Filter

### Key:
  :replace_norm_hpf

### Doc:
  Please write documentation!

### Arguments:
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

  * res:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Low Pass Filter

### Key:
  :lpf

### Doc:
  Please write documentation!

### Arguments:
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



## FX Low Pass Filter

### Key:
  :replace_lpf

### Doc:
  Please write documentation!

### Arguments:
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



## FX Normalised Low Pass Filter

### Key:
  :norm_lpf

### Doc:
  Please write documentation!

### Arguments:
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

  * res:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Normalised Low Pass Filter

### Key:
  :replace_norm_lpf

### Doc:
  Please write documentation!

### Arguments:
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

  * res:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * res_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Normaliser

### Key:
  :normaliser

### Doc:
  Please write documentation!

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



## FX Normaliser

### Key:
  :replace_normaliser

### Doc:
  Please write documentation!

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



## FX Distortion

### Key:
  :distortion

### Doc:
  Please write documentation!

### Arguments:
  * distort:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * distort_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



## FX Distortion

### Key:
  :replace_distortion

### Doc:
  Please write documentation!

### Arguments:
  * distort:
    - doc: write me
    - default: 0.5
    - constraints: none
    - Can not be changed once set

  * distort_slide:
    - doc: write me
    - default: 0
    - constraints: none
    - Can not be changed once set



