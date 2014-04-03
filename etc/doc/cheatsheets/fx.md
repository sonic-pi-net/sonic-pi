# FX

## FX Reverb

### Key: 
  :reverb 

### Doc: 
  Please write documentation!

### Arguments:
  * :mix
    - doc:  
    - default: 0.75 
    - Not Modulatable  

  * :mix_slide
    - doc:  
    - default: 0 
    - Not Modulatable  

  * :room
    - doc:  
    - default: 0.6 
    - Not Modulatable  

  * :room_slide
    - doc:  
    - default: 0 
    - Not Modulatable  

  * :damp
    - doc:  
    - default: 0.5 
    - Not Modulatable  

  * :damp_slide
    - doc:  
    - default: 0 
    - Not Modulatable  



## FX Level Amplifier

### Key: 
  :level 

### Doc: 
  Please write documentation!

### Arguments:
  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played. 
    - default: 1 
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amp to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - Modulatable  



## FX Echo

### Key: 
  :echo 

### Doc: 
  Please write documentation!

### Arguments:
  * :max_delay
    - doc: The maximum delay time in seconds. 
    - default: 1 
    - Not Modulatable  

  * :delay
    - doc: The time between echoes in seconds. 
    - default: 0.4 
    - Modulatable  

  * :delay_slide
    - doc: Slide time in seconds between delay values 
    - default: 0 
    - Modulatable  

  * :decay
    - doc: The time it takes for the echoes to fade away in seconds. 
    - default: 8 
    - Modulatable  

  * :decay_slide
    - doc: Slide time in seconds between decay times 
    - default: 0 
    - Modulatable  

  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played. 
    - default: 1 
    - Modulatable  



## FX Slicer

### Key: 
  :slicer 

### Doc: 
  Please write documentation!

### Arguments:
  * :rate
    - doc: The frequency of the slices 
    - default: 4 
    - Modulatable  

  * :rate_slide
    - doc: Slide time in seconds between rate values 
    - default: 0 
    - Modulatable  

  * :width
    - doc: The width of the slices - 0 - 1. 
    - default: 0.5 
    - Modulatable  

  * :width_slide
    - doc: Slide time in seconds between width values 
    - default: 0 
    - Modulatable  

  * :phase
    - doc: Initial phase. 
    - default: 0 
    - Not Modulatable  

  * :amp
    - doc: The amplitude of the resulting effect. 
    - default: 1 
    - Modulatable  

  * :amp_slide
    - doc: The slide lag time for amplitude changes. 
    - default: 0.05 
    - Modulatable  



## FX Techno

### Key: 
  :techno 

### Doc: 
  Please write documentation!

### Arguments:
  * :rate
    - doc: The frequency of filter modulation 
    - default: 0.1 
    - Modulatable  

  * :rate_slide
    - doc:  
    - default: 0 
    - Not Modulatable  

  * :cutoff_min
    - doc:  
    - default: 880 
    - Not Modulatable  

  * :cutoff_min_slide
    - doc:  
    - default: 0 
    - Not Modulatable  

  * :cutoff_max
    - doc:  
    - default: 12000 
    - Not Modulatable  

  * :cutoff_max_slide
    - doc:  
    - default: 0 
    - Not Modulatable  

  * :res
    - doc:  
    - default: 0.2 
    - Not Modulatable  

  * :res_slide
    - doc:  
    - default: 0 
    - Not Modulatable  



## FX Compressor

### Key: 
  :compressor 

### Doc: 
  Please write documentation!

### Arguments:
  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played. 
    - default: 1 
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amp to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - Modulatable  

  * :threshold
    - doc:  
    - default: 0.2 
    - Not Modulatable  

  * :threshold_slide
    - doc:  
    - default: 0 
    - Not Modulatable  

  * :clamp_time
    - doc:  
    - default: 0.01 
    - Not Modulatable  

  * :clamp_time_slide
    - doc:  
    - default: 0 
    - Not Modulatable  

  * :slope_above
    - doc:  
    - default: 0.5 
    - Not Modulatable  

  * :slope_above_slide
    - doc:  
    - default: 0 
    - Not Modulatable  

  * :slope_below
    - doc:  
    - default: 1 
    - Not Modulatable  

  * :slope_below_slide
    - doc:  
    - default: 0 
    - Not Modulatable  

  * :relax_time
    - doc:  
    - default: 0.01 
    - Not Modulatable  

  * :relax_time_slide
    - doc:  
    - default: 0 
    - Not Modulatable  



## FX Resonant Low Pass Filter

### Key: 
  :rlpf 

### Doc: 
  Please write documentation!

### Arguments:
  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - Modulatable  

  * :res
    - doc:  
    - default: 0.5 
    - Not Modulatable  

  * :res_slide
    - doc:  
    - default: 0 
    - Not Modulatable  



## FX Normalised Resonant Low Pass Filter

### Key: 
  :norm_rlpf 

### Doc: 
  Please write documentation!

### Arguments:
  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - Modulatable  

  * :res
    - doc:  
    - default: 0.5 
    - Not Modulatable  

  * :res_slide
    - doc:  
    - default: 0 
    - Not Modulatable  



## FX Resonant High Pass Filter

### Key: 
  :rhpf 

### Doc: 
  Please write documentation!

### Arguments:
  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - Modulatable  

  * :res
    - doc:  
    - default: 0.5 
    - Not Modulatable  

  * :res_slide
    - doc:  
    - default: 0 
    - Not Modulatable  



## FX Normalised Resonant High Pass Filter

### Key: 
  :norm_rhpf 

### Doc: 
  Please write documentation!

### Arguments:
  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - Modulatable  

  * :res
    - doc:  
    - default: 0.5 
    - Not Modulatable  

  * :res_slide
    - doc:  
    - default: 0 
    - Not Modulatable  



## FX High Pass Filter

### Key: 
  :hpf 

### Doc: 
  Please write documentation!

### Arguments:
  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - Modulatable  



## FX Normalised High Pass Filter

### Key: 
  :norm_hpf 

### Doc: 
  Please write documentation!

### Arguments:
  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - Modulatable  

  * :res
    - doc:  
    - default: 0.5 
    - Not Modulatable  

  * :res_slide
    - doc:  
    - default: 0 
    - Not Modulatable  



## FX Low Pass Filter

### Key: 
  :lpf 

### Doc: 
  Please write documentation!

### Arguments:
  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - Modulatable  



## FX Normalised Low Pass Filter

### Key: 
  :norm_lpf 

### Doc: 
  Please write documentation!

### Arguments:
  * :cutoff
    - doc: MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy. 
    - default: 100 
    - Modulatable  

  * :cutoff_slide
    - doc: Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value. 
    - default: 0 
    - Modulatable  

  * :res
    - doc:  
    - default: 0.5 
    - Not Modulatable  

  * :res_slide
    - doc:  
    - default: 0 
    - Not Modulatable  



## FX Normaliser

### Key: 
  :normaliser 

### Doc: 
  Please write documentation!

### Arguments:
  * :amp
    - doc: The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played. 
    - default: 1 
    - Modulatable  

  * :amp_slide
    - doc: Amount of time (in seconds) for the amp to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude. 
    - default: 0 
    - Modulatable  



## FX Distortion

### Key: 
  :distortion 

### Doc: 
  Please write documentation!

### Arguments:
  * :distort
    - doc:  
    - default: 0.5 
    - Not Modulatable  

  * :distort_slide
    - doc:  
    - default: 0 
    - Not Modulatable  



