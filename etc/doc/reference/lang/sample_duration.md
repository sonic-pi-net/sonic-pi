# Get duration of sample in beats

```
sample_duration 
 <!--- #tr -->path (string)<!--- #end tr -->
```


Given the name of a loaded sample, or a path to a `.wav`, `.wave`, `.aif`, `.aiff`, `.ogg`, `.oga` or `.flac` file returns the length of time in beats that the sample would play for. `sample_duration` understands and accounts for all the opts you can pass to `sample` which have an effect on the playback duration such as `rate:`. The time returned is scaled to the current BPM.

*Note:* avoid using `sample_duration` to set the sleep time in `live_loop`s, prefer stretching the sample with the `beat_stretch:` opt or changing the BPM instead. See the examples below for details.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

puts sample_duration(:loop_garzul)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Simple use
# returns 8.0 because this sample is 8 seconds long



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

use_bpm 120
puts sample_duration(:loop_garzul)
use_bpm 90
puts sample_duration(:loop_garzul)
use_bpm 21
puts sample_duration(:loop_garzul)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# The result is scaled to the current BPM
 
# => 16.0
 
# => 12.0
 
# => 2.8



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


live_loop :avoid_this do              
  with_fx :slicer do                  
    sample :loop_amen                 
    sleep sample_duration(:loop_amen) 
  end                                 
end

live_loop :prefer_this do             
  use_sample_bpm :loop_amen           
  with_fx :slicer do                  
    sample :loop_amen
    sleep 1
  end
end

live_loop :or_this do                 
  with_fx :slicer do                  
    sample :loop_amen, beat_stretch: 1
    sleep 1
  end
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Avoid using sample_duration to set the sleep time in live_loops
 
# It is possible to use sample_duration to drive the frequency of a live loop.
# However, if you're using a rhythmical sample such as a drum beat and it isn't
# in the same BPM as the current BPM, then the FX such as this slicer will be
# badly out of sync. This is because the slicer slices at the current BPM and
# this live_loop is looping at a different BPM (that of the sample)
 
 
# Instead prefer to set the BPM of the live_loop to match the sample. It has
# two benefits. Now our sleep is a nice and simple 1 (as it's one beat).
# Also, our slicer now works with the beat and sounds much better.
 
 
 
 
 
# Alternatively we can beat_stretch the sample to match the current BPM. This has the
# side effect of changing the rate of the sample (and hence the pitch). However, the
# FX works nicely in time and the sleep time is also a simple 1.
 
 
 



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


                                                                 
sample_duration :loop_garzul, rate: 1                            

                                                                 
sample_duration :loop_garzul, rate: 0.5                          

                                                                 
sample_duration :loop_garzul, rate: 2                            

                                                                 
sample_duration :loop_garzul, rate: -2                           

                                                                 
sample_duration :loop_garzul, attack: 1                          
sample_duration :loop_garzul, attack: 100                        
sample_duration :loop_garzul, attack: 0                          

                                                                 
sample_duration :loop_garzul, release: 1                         
sample_duration :loop_garzul, release: 100                       
sample_duration :loop_garzul, release: 0                         

                                                                 
sample_duration :loop_garzul, decay: 1                           
sample_duration :loop_garzul, decay: 100                         
sample_duration :loop_garzul, decay: 0                           

                                                                 
                                                                 
                                                                 
sample_duration :loop_garzul, sustain: 0, attack: 0.5            
sample_duration :loop_garzul, sustain: 0, decay: 0.1             
sample_duration :loop_garzul, sustain: 0, release: 1             
sample_duration :loop_garzul, sustain: 2, attack: 0.5, release: 1

                                                                 
                                                                 
sample_duration :loop_garzul, sustain: 0, attack: 8, release: 3  


                                                                 
sample_duration :loop_garzul, rate: 10                           
sample_duration :loop_garzul, sustain: 0, attack: 0.9, rate: 10  


                                                                 
                                                                 
sample_duration :loop_garzul, rpitch: 12                         
sample_duration :loop_garzul, rpitch: -12                        

                                                                 
sample_duration :loop_garzul, rpitch: 12, rate: 2                

                                                                 
                                                                 
sample_duration :loop_garzul, beat_stretch: 3                    
sample_duration :loop_garzul, beat_stretch: 3, rate: 0.5         

                                                                 
                                                                 
sample_duration :loop_garzul, pitch_stretch: 3                   
sample_duration :loop_garzul, pitch_stretch: 3, rate: 0.5        

                                                                 
                                                                 
sample_duration :loop_garzul, start: 0.5                         
sample_duration :loop_garzul, start: 0.5, finish: 0.75           
sample_duration :loop_garzul, finish: 0.5, start: 0.75           
sample_duration :loop_garzul, rate: 2, finish: 0.5, start: 0.75



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# The standard sample opts are also honoured
 
# Playing a sample at standard speed will return standard length
# => 8.0
 
# Playing a sample at half speed will double duration
# => 16.0
 
# Playing a sample at double speed will halve duration
# => 4.0
 
# Playing a sample backwards at double speed will halve duration
# => 4.0
 
# Without an explicit sustain: opt attack: just affects amplitude not duration
# => 8.0
# => 8.0
# => 8.0
 
# Without an explicit sustain: opt release: just affects amplitude not duration
# => 8.0
# => 8.0
# => 8.0
 
# Without an explicit sustain: opt decay: just affects amplitude not duration
# => 8.0
# => 8.0
# => 8.0
 
# With an explicit sustain: opt, if the attack + decay + sustain + release envelope
# duration is less than the sample duration time, the envelope will shorten the
# sample time.
# => 0.5
# => 0.1
# => 1.0
# => 3.5
 
# If the envelope duration is longer than the sample it will not affect the
# sample duration
# => 8
 
 
# All other opts are taken into account before the comparison with the envelope opts.
# => 0.8
# => 0.8 (The duration of the sample is less than the envelope length so wins)
 
 
# The rpitch: opt will modify the rate to shift the pitch of the sample up and down
# and therefore affects duration.
# => 4.0
# => 16
 
# The rpitch: and rate: opts combine together.
# => 2.0
 
# The beat_stretch: opt stretches the sample so that its duration matches the value.
# It also combines with rate:
# => 3.0
# => 6.0
 
# The pitch_stretch: opt acts identically to beat_stretch when just considering sample
# duration.
# => 3.0
# => 6.0
 
# The start: and finish: opts can also shorten the sample duration and also combine
# with other opts such as rate:
# => 4.0
# => 2.0
# => 2.0
# => 1.0



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


sample :loop_amen                   
sleep sample_duration(:loop_amen)   
sample :loop_amen                   



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Triggering samples one after another
 
# start the :loop_amen sample
# wait for the duration of :loop_amen before
# starting it again



```
<!--- #end tr -->

</td>
</tr>
</table>

