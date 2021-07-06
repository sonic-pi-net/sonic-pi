# Trigger sample

```
sample 
 <!--- #tr -->name_or_path (symbol_or_string)<!--- #end tr -->
```


Play back a recorded sound file (sample). Sonic Pi comes with lots of great samples included (see the section under help) but you can also load and play `.wav`, `.wave`, `.aif`, `.aiff`, `.ogg`, `.oga` or `.flac` files from anywhere on your computer too. To play a built-in sample use the corresponding keyword such as `sample :bd_haus`. To play any file on your computer use a full path such as `sample "/path/to/sample.wav"`.

There are many opts for manipulating the playback. For example, the `rate:` opt affects both the speed and the pitch of the playback. To control the rate of the sample in a pitch-meaningful way take a look at the `rpitch:` opt.

The sampler synth has three separate envelopes - one for amplitude, one for a low pass filter and another for a high pass filter. These work very similar to the standard synth envelopes except for two major differences. Firstly, the envelope times do not stretch or shrink to match the BPM. Secondly, the sustain time by default stretches to make the envelope fit the length of the sample. This is explained in detail in the tutorial.

Samples are loaded on-the-fly when first requested (and subsequently remembered). If the sample loading process takes longer than the schedule ahead time, the sample trigger will be skipped rather than be played late and out of time. To avoid this you may preload any samples you wish to work with using `load_sample` or `load_samples`.

It is possible to set the `start:` and `finish:` positions within the sample to play only a sub-section of it. These values can be automatically chosen based on an onset detection algorithm which will essentially isolate each individual drum or synth hit in the sample and let you access each one by an integer index (floats will be rounded to the nearest integer value). See the `onset:` docstring and examples for more information.

Finally, the sampler supports a powerful filtering system to make it easier to work with large folders of samples. The filter commands must be used before the first standard opt. There are six kinds of filter parameters you may use:

1. Folder strings - `"/foo/bar"` - which will add all samples within the folder to the set of candidates.
2. Recursive folder strings - `"/foo/bar/**"` - Folder strings ending with `**` will add all samples contained within all subfolders (searched recursively).
3. Sample strings - `"/path/to/sample.wav"` - which will add the specific sample to the set of candidates.
4. Other strings - `"foobar"` - which will filter the candidates based on whether the filename contains the string.
5. Regular expressions - `/b[aA]z.*/` - which will filter the candidates based on whether the regular expression matches the filename.
6. Keywords - `:quux` - will filter the candidates based on whether the keyword is a direct match of the filename (without extension).
7. Numbers - `0` - will select the candidate with that index (wrapping round like a ring if necessary).
8. Lists of the above - `["/foo/bar", "baz", /0-9.*/]` - will recurse down and work through the internal filter parameters as if they were in the top level.
9. Lambdas - `lambda {|s| [s.choose] }` - the ultimate power tool for filters. Allows you to create a custom fn which receives a list of candidates as an arg and which should return a new list of candidates (this may be smaller, larger, re-ordered it's up to you).

By combining commands which add to the candidates and then filtering those candidates it is possible to work with folders full of samples in very powerful ways. Note that the specific ordering of filter parameters is irrelevant with the exception of the numbers - in which case the last number is the index. All the candidates will be gathered first before the filters are applied.


Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

sample :loop_amen



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Play a built-in sample
# Plays the Amen break



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 2 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby


sample :loop_amen
sample :ambi_lunar_land
                       



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Play two samples at the same time
# with incredible timing accuracy
 
# Note, for timing guarantees select the pref:
#   Studio -> Synths and FX -> Enforce timing guarantees



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 3 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

live_loop :bass do
  sample :bd_haus
  sleep 0.5
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Create a simple repeating bass drum
 
 
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 4 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

live_loop :rhythm do
  sample :tabla_ghe3 if (spread 5, 7).tick
  sleep 0.125
end
live_loop :bd, sync: :rhythm do
  sample :bd_haus, lpf: 90, amp: 2
  sleep 0.5
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Create a more complex rhythm with multiple live loops:
 
 
 
 
 
 
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 5 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

sample :loop_amen, rate: 0.5
                            



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Change the playback speed of the sample using rate:
# Play the Amen break at half speed
# for old school hip-hop



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 6 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

sample :loop_amen, rate: 1.5
                            



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Speed things up
# Play the Amen break at 1.5x speed
# for a jungle/gabba sound



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 7 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

sample :loop_amen, rate: -1



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Go backwards
# Negative rates play the sample backwards



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 8 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

sample :loop_amen, rate: -3



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Fast rewind
# Play backwards at 3x speed for a fast rewind effect



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 9 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

sample :loop_amen, start: 0.5



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Start mid sample
# Start playback half way through



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 10 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

sample :loop_amen, finish: 0.5



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Finish mid sample
# Finish playback half way through



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 11 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

sample :loop_amen, start: 0.125, finish: 0.25



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Play part of a sample
# Play the second eighth of the sample



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 12 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

sample :loop_amen, start: 0.25, finish: 0.125



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Finishing before the start plays backwards
# Play the second eighth of the sample backwards



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 13 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

sample :loop_amen, start: 0.125, finish: 0.25, rate: -0.25
                                                          
                                                          



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Play a section of a sample at quarter speed backwards
# Play the second eighth of the
# amen break backwards at a
# quarter speed



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 14 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

s = sample :loop_amen, lpf: 70
sleep 0.5
control s, lpf: 130
sleep 0.5
synth :dsaw, note: :e3



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Control a sample synchronously
 
 
 
 
# This is triggered 1s from start



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 15 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

sample :loop_amen, lpf: 70 do |s|
  sleep 1                               
  control s, lpf: 130                   
end
sleep 0.5
synth :dsaw, note: :e3



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Controlling a sample asynchronously
 
# This block is run in an implicit in_thread
# and therefore is asynchronous
 
 
# This is triggered 0.5s from start



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 16 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

sample :loop_garzul, slice: 0     
sleep 0.5
4.times do
  sample :loop_garzul, slice: 1   
  sleep 0.125
end
sample :loop_garzul, slice: 4, num_slices: 4, rate: -1     



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Play with slices
# => play the first 16th of the sample
 
 
# => play the second 16th of the sample 4 times
 
 
# => play the final quarter backwards



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 17 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

use_sample_bpm :loop_amen                   
live_loop :beat_slicer do
  n = 8                                     
                                            
  s = rand_i n                              
  sample :loop_amen, slice: s, num_slices: n
  sleep 1.0/n                               
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Build a simple beat slicer
# Set the BPM to match the amen break sample
 
# Specify number of slices
# (try changing to 2, 4, 6, 16 or 32)
# Choose a random slice within range
# Play the specific part of the sample
# Sleep for the duration of the slice
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 18 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

sample :loop_amen, lpf: 80, hpf: 70, compress: 1, pre_amp: 10



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Play with the built-in low pass filter, high pass filter and compressor
# Make the amen break sound punchy.



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 19 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

sample :loop_garzul, lpf_attack: 8
sleep 8
sample :loop_garzul, hpf_attack: 8



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Use the cutoff filter envelopes
# Sweep the low pass filter up over 8 beats
 
# Sweep the high pass filter down over 8 beats



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 20 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

puts sample_duration :loop_industrial                  
puts sample_duration :loop_industrial, beat_stretch: 1 
live_loop :industrial do
  sample :loop_industrial, beat_stretch: 1             
  sleep 1                                              
                                                       
                                                       
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Sample stretching
# => 0.88347
# => 1
 
# Stretch the sample to make it 1 beat long
# This now loops perfectly.
# However, note that stretching/shrinking
# also modifies the pitch.
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 21 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

puts sample_duration :loop_garzul                      
puts sample_duration :loop_garzul, beat_stretch: 6     
live_loop :garzul do
  sample :loop_garzul, beat_stretch: 6                 
                                                       
                                                       
  sleep 6
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Sample shrinking
# => 8
# => 6
 
# As :loop_garzul is longer than 6 beats
# it is shrunk to fit. This increases the
# pitch.
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 22 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

use_bpm 30                                             
puts sample_duration :loop_garzul                      
puts sample_duration :loop_garzul, beat_stretch: 6     
live_loop :garzul do
  sample :loop_garzul, beat_stretch: 6                 
  sleep 6
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Sample stretching matches the BPM
# Set the BPM to 30
# => 4.0 (at 30 BPM the sample lasts for 4 beats)
# => 6.0
 
# The sample is stretched to match 6 beats at 30 BPM
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 23 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

sample "/path/to/sample.wav"                         
                                                       
                                                       



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# External samples
# Play any Wav, Aif, Ogg, Oga, or FLAC sample on your computer
# by simply passing a string representing the full
# path



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 24 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

dir = "/path/to/dir/of/samples"                      
sample dir                                             
                                                       
sample dir, 1                                          
sample dir, 99                                         
                                                       
                                                       
                                                       
                                                       
sample dir, "120"                                    
                                                       
                                                       
sample dir, "120", 1                                 
                                                       
                                                       
sample dir, /beat[0-9]/                                
                                                       
                                                       
                                                       
                                                       
sample dir, /beat[0-9]0/, "100"                      
                                                       
                                                       
                                                       



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Sample pack filtering
# You can easily work with a directory of samples
# Play the first sample in the directory
# (it is sorted alphabetically)
# Play the second sample in the directory
# Play the 100th sample in the directory, or if there
# are fewer, treat the directory like a ring and keep
# wrapping the index round until a sample is found.
# For example, if there are 90 samples, the 10th sample
# is played (index 9).
# Play the first sample in the directory that contains
# the substring "120".
# For example, this may be "beat1_120_rave.wav"
# Play the second sample in the directory that contains
# the substring "120".
# For example, this may be "beat2_120_rave.wav"
# Play the first sample in the directory that matches
# the regular expression /beat[0-9]/.
# For example, this may be "beat0_100_trance.wav"
# You may use the full power of Ruby's regular expression
# system here: http://ruby-doc.org/core-2.1.1/Regexp.html
# Play the first sample in the directory that both matches
# the regular expression /beat[0-9]0/ and contains the
# the substring "100".
# For example, this may be "beat10_100_rave.wav"



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 25 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

                                                       
                                                       
sample "tabla_"                                      
                                                       
sample "tabla_", 2                                   
                                                       



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Filtering built-in samples
# If you don't pass a directory source, you can filter over
# the built-in samples.
# Play the first built-in sample that contains the substring
# "tabla"
# Play the third built-in sample that contains the substring
# "tabla"



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 26 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

load_samples "tabla_"                                
                                                       
                                                       
live_loop :tabla do
  sample "tabla_", tick                              
  sleep 0.125
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Play with whole directories of samples
# You may pass any of the source/filter options to load_samples
# to load all matching samples. This will load all the built-in
# samples containing the substring "tabla_"
 
# Treat the matching samples as a ring and tick through them
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 27 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

dir1 = "/path/to/sample/directory"
dir2 = "/path/to/other/sample/directory"
sample dir1, dir2, "foo"                             
                                                       
                                                       



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Specify multiple sources
 
 
# Match the first sample that contains the string "foo" out of
# all the samples in dir1 and dir2 combined.
# Note that the sources must be listed before any filters.



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 28 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

dir = "/path/to/sample/directory"                    
                                                       
dir_recursive = "/path/to/sample/directory/**"       
                                                       
                                                       
sample dir, 0                                          
sample dir_recursive, 0                                
                                                       
                                                       
                                                       
                                                       



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# List contents recursively
# By default the list of all top-level samples within the directory
# is considered.
# However, if you finish your directory string with ** then if that
# directory contains other directories then the samples within the
# subdirectories and their subsubdirectories in turn are considered.
# Play the first top-level sample in the directory
# Play the first sample found after combining all samples found in
# the directory and all directories within it recursively.
# Note that if there are many sub directories this may take some time
# to execute. However, the result is cached so subsequent calls will
# be fast.



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 29 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

filter = lambda do |candidates|                        
  [candidates.choose]                                  
end                                                    
                                                       
8.times do
  sample "drum_", filter                             
  sleep 0.25                                           
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Bespoke filters
# If the built-in String, Regexp and index filters are not sufficient
# you may write your own. They need to be a function which takes a list
# of paths to samples and return a list of samples. This one returns a
# list of a single randomly selected sample.
 
# Play 8 randomly selected samples from the built-in sample set that also
# contain the substring "drum_"
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 30 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby


sample :loop_tabla, start: 0, finish: 0.00763          
                                                       
                                                       
sleep 1
                                                       
                                                       
sample :loop_tabla, onset: 0                           
                                                       
                                                       
sleep 1

sample :loop_tabla, onset: 1                           
                                                       



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Basic Onset Detection
 
# If you know the right start: and finish: values, you can extract a
# single drum hit from a longer sample. However, finding these values
# can be very time consuming.
 
# Instead of specifying the start: and finish: values manually you can
# use the onset: option to find them for you using an integer index.
# onset: 0 will set the start: and finish: values so that the first
# percussive sound (something that shifts from quiet to loud quickly)
# is picked out.
 
 
# We can easily find the second percussive sound in the sample with
# onset: 1



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 31 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby


                                                       
                                                       
                                                       
                                                       
                                                       


live_loop :tabla do
  use_bpm 50                                           
  sample :loop_tabla, onset: tick                      
  sleep [0.125, 0.25].choose                           
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Ticking through onsets
 
# The onsets are actually a ring so the index will wrap around. This
# means that if there are only 8 onsets in a sample, specifying an
# onset of 100 will still return one of the 8 onsets. This means we
# can use tick to work through each onset in sequence. This allows us
# to redefine the rhythm and tempo of a sample
 
 
 
# We can choose our own BPM here - it doesn't need to match the sample
# tick through each onset in sequence
# randomly choose a delay between onset triggers
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 32 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

                                                       
use_bpm 50
live_loop :tabla do
  sample :loop_tabla, onset: pick                      
  sleep [0.125, 0.25].choose                           
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Random Onset Triggering
# We can easily pick a random onset using the pick fn
 
 
# Each time round the live loop we now trigger a random onset
# creating an infinite stream of randomly selected drums
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 33 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

                                                       
                                                       
live_loop :tabla do
  use_random_seed 30000                                
  8.times do
    sample :loop_tabla, onset: pick
    sleep [0.125, 0.25].choose
  end
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Repeatable Random Onsets
# Instead of an infinite stream of choices, we can combine iteration
# and use_random_seed to create repeatable riffs:
 
# every 8 times, reset the random seed, this resets the riff
 
 
 
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 34 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

                                                           
                                                           
                                                           
                                                           
                                                           
live_loop :tabla do
  sample :loop_tabla, onset: pick, sustain: 0, release: 0.1
                                                           
                                                           
                                                           
  sleep [0.125, 0.25].choose
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#  Random Onset Duration
# Each onset has a variable length (determined by the sample contents).
# Therefore, if you wish to ensure each onset has a specific length it
# is necessary to use the sample's amplitude envelope.
# As the sample's envelope automatically changes the sustain: value to
# match the duration - you also need to override this with a value of 0.
 
# Each drum onset will now be no longer than 0.1. Note that the envelope
# for a sample only determines the maximum duration of a sample trigger.
# If the actual audible duration of the onset is smaller than 0.1 then
# it will *not* be extended.
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 35 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby


                                                       
                                                       
                                                       

l = lambda {|c| puts c ; c[0]}                         
                                                       
                                                       
                                                       

sample :loop_tabla, onset: l                           

                                                       
                                                       
                                                       
                                                       



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Onset lambdas
 
# The onset index can be a lambda as well as an integer. If a lambda is
# given, it will be passed a ring of all of the onsets as an argument.
# This will be a ring of maps:
 
# define a lambda which accepts a single argument, prints it and
# returns the first value. This particular example is essentially
# the same as using onset: 0 with the side effect of also printing out
# the full ring of onsets:
 
# (ring {:start=>0.0, :finish=>0.0076}, {:start=>0.0076, :finish 0.015}...)
 
# We are therefore free to define this lambda to do anything we want.
# This gives us very powerful control over the choice of onset. It is
# unlikely you will use this frequently, but it is a powerful tool
# that's there when you need it.



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 36 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
sample :loop_tabla, onset: 1                                        

                                                                    
                                                                    
                                                                    



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Plays the 2nd onset (the first onset would have index 0)
 
# Will override opts with: {start: 0.0151, finish: 0.0304}
# (these values are specific to the :loop_tabla sample and
# will vary for different samples)



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 37 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
sample :loop_tabla, onset: 1, slice: 0, num_slices: 1               
                                                                    

                                                                    
                                                                    
                                                                    



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Plays the 2nd onset. This behaves the same as not specifying
# a slice as we select the first of one slices.
 
# Will override opts with: {start: 0.0151, finish: 0.0304}
# (these values are specific to the :loop_tabla sample and
# will vary for different samples)



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 38 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
sample :loop_tabla, onset: 1, slice: 0, num_slices: 2               
                                                                    
                                                                    

                                                                    
                                                                    
                                                                    



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# This plays the first half of the 2nd onset.
# This is because  we split that onset into two slices and
# play just the first slice (with index 0).
 
# Will override opts with: {start: 0.0151, finish: 0.0227}
# (these values are specific to the :loop_tabla sample and
# will vary for different samples)



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 39 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
sample :loop_tabla, onset: 1, slice: 0, num_slices: 4               
                                                                    
                                                                    

                                                                    
                                                                    
                                                                    

sample :loop_tabla, onset: 1, slice: 0, num_slices: 4, finish: 0.5  
                                                                    
                                                                    

                                                                    
                                                                    
                                                                    

sample :loop_tabla, onset: 1, slice: 0, num_slices: 4, finish: 0.0, start: 0.5  
                                                                                
                                                                                
                                                                                

                                                                                
                                                                                
                                                                                



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# This plays the first quarter of the 2nd onset.
# This is because we split that onset into four slices and
# play just the first slice (with index 0).
 
# Will override opts with: {start: 0.0151, finish: 0.0189}
# (these values are specific to the :loop_tabla sample and
# will vary for different samples)
 
# Will play the first 1/8th of the 2nd onset.
# This is because we split that specific onset into 4 slices
# and then only play the first half of the first slice.
 
# Will override opts with: {start: 0.0151, finish: 0.017}
# (these values are specific to the :loop_tabla sample and
# will vary for different samples)
 
# Will play the first 1/8th of the 2nd onset backwards..
# This is because we split that specific onset into 4 slices
# and then only play from the first half of the first slice
# back to the beginning.
 
# Will override opts with: {start: 0.017, finish: 0.0151}
# (these values are specific to the :loop_tabla sample and
# will vary for different samples)



```
<!--- #end tr -->

</td>
</tr>
</table>

