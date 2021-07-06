# Control running synth

```
control 
 <!--- #tr -->node (synth_node)<!--- #end tr -->
```


Control a running synth node by passing new parameters to it. A synth node represents a running synth and can be obtained by assigning the return value of a call to play or sample or by specifying a parameter to the do/end block of an FX. You may modify any of the parameters you can set when triggering the synth, sample or FX. See documentation for opt details. If the synth to control is a chord, then control will change all the notes of that chord group at once to a new target set of notes - see example. Also, you may use the on: opt to conditionally trigger the control - see the docs for the `synth` and `sample` fns for more information.

If no synth to control is specified, then the last synth triggered by the current (or parent) thread will be controlled - see example below.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby


my_node = play 50, release: 5, cutoff: 60
sleep 1
control my_node, cutoff: 70
sleep 1
control my_node, cutoff: 90



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Basic control
 
# play note 50 with release of 5 and cutoff of 60. Assign return value to variable my_node
# Sleep for a second
# Now modify cutoff from 60 to 70, sound is still playing
# Sleep for another second
# Now modify cutoff from 70 to 90, sound is still playing



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


s = synth :prophet, note: :e1, cutoff: 70, cutoff_slide: 8, release: 8
control s, cutoff: 130
                      



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Combining control with slide opts allows you to create nice transitions.
 
# start synth and specify slide time for cutoff opt
# Change the cutoff value with a control.
# Cutoff will now slide over 8 beats from 70 to 130



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


notes = (scale :e3, :minor_pentatonic, num_octaves: 2).shuffle

s = synth :beep, note: :e3, sustain: 8, note_slide: 0.05
64.times do
  control s, note: notes.tick                           
  sleep 0.125
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Use a short slide time and many controls to create a sliding melody
 
# get a random ordering of a scale
 
# Start our synth running with a long sustain and short note slide time
 
# Keep quickly changing the note by ticking through notes repeatedly
 
 



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


with_fx :bitcrusher, sample_rate: 1000, sample_rate_slide: 8 do |bc|
                                                                    
                                                                    
  sample :loop_garzul, rate: 1
  control bc, sample_rate: 5000                                     
                                                                    
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Controlling FX
 
# Start FX but also use the handy || goalposts
# to grab a handle on the running FX. We can call
# our handle anything we want. Here we've called it bc
 
# We can use our handle bc now just like we used s in the
# previous example to modify the FX as it runs.
 



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


cg = play (chord :e4, :minor), sustain: 2 
sleep 1
control cg, notes: (chord :c3, :major)    
                                          
                                          



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Controlling chords
 
# start a chord
 
# transition to new chord.
# Each note in the original chord is mapped onto
# the equivalent in the new chord.



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


cg = play (chord :e4, :minor), sustain: 4, note_slide: 3 
sleep 1
control cg, notes: (chord :c3, :major)                   
                                                         
                                                         



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Sliding between chords
 
# start a chord
 
# slide to new chord.
# Each note in the original chord is mapped onto
# the equivalent in the new chord.



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


cg = play (chord :e3, :m13), sustain: 4, note_slide: 3 
sleep 1
control cg, notes: (chord :c3, :major)                   
                                                         
                                                         
                                                         
                                                         



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Sliding from a larger to smaller chord
 
# start a chord with 7 notes
 
# slide to new chord with fewer notes (3)
# Each note in the original chord is mapped onto
# the equivalent in the new chord using ring-like indexing.
# This means that the 4th note in the original chord will
# be mapped onto the 1st note in the second chord and so-on.



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

cg = play (chord :c3, :major), sustain: 4, note_slide: 3 
sleep 1
control cg, notes: (chord :e3, :m13)                    
                                                         
                                                         
                                                         
                                                         
                                                         



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Sliding from a smaller to larger chord
# start a chord with 3 notes
 
# slide to new chord with more notes (7)
# Each note in the original chord is mapped onto
# the equivalent in the new chord.
# This means that the 4th note in the new chord
# will not sound as there is no 4th note in the
# original chord.



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


s = synth :prophet, note: :e1, release: 8, cutoff: 70, cutoff_slide: 8
sleep 1                                                               
control s, cutoff: 130                                                
sleep 3                                                               
control s, cutoff_slide: 1                                            
                                                                      
                                                                      



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Changing the slide rate
 
# Start a synth playing with a long cutoff slide
# wait a beat
# change the cutoff so it starts sliding slowly
# wait for 3 beats
# Change the cutoff_slide - the cutoff now slides more quickly to 130
# it will now take 1 beat to slide from its *current* value
# (somewhere between 70 and 130) to 130



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


synth :prophet, note: :e1, release: 8                                 
sleep 1
16.times do
  control note: (octs :e1, 3).tick                                    
  sleep 0.125                                                         
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Controlling the last triggered synth
 
# Every time a synth is triggered, Sonic Pi automatically remembers the node
 
 
# This means we don't need to use an explicit variable to control the synth
# we last triggered.
 



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


synth :beep, release: 4                 
sleep 0.1
control note: :e5                       
sleep 0.5
synth :dsaw, release: 4                 
sleep 0.1
control note: :e4                       



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Controlling multiple synths without variables
 
# Trigger a beep synth
 
# Control last triggered synth (:beep)
 
# Next, trigger a dsaw synth
 
# Control last triggered synth (:dsaw)



```
<!--- #end tr -->

</td>
</tr>
</table>

