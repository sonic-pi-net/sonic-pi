# A named audio stream live from your soundcard

```
live_audio 
 <!--- #tr -->name (symbol)<!--- #end tr -->
```


Create a named synthesiser which works similar to `play`, `sample` or `synth`. Rather than synthesising the sound mathematically or playing back recorded audio, it streams audio live from your sound card.

However, unlike `play`, `sample` and `synth`, which allow multiple similar synths to play at the same time (i.e. a chord) only one `live_audio` synth of a given name may exist in the system at any one time. This is similar to `live_loop` where only one live loop of each name may exist at any one time. See examples for further information.

An additional difference is that `live_audio` will create an infinitely long synth rather than be timed to an envelope like the standard `synth` and `sample` synths. This is particularly suitable for working with continuous incoming audio streams where the source of the audio is unknown (for example, it may be a guitar, an analog synth or an electronic violin). If the source is continuous, then it may not be suited to being stitched together by successive enveloped calls to something like: `synth :sound_in, attack: 0, sustain: 4, release: 0`. If we were to `live_loop` this with a `sleep 4` to match the sustain duration, we would get something that emulated a continuous stream, but for certain inputs you'll hear clicking at the seams between each successive call to `synth` where the final part of the audio signal from the previous synth doesn't precisely match up with the start of the signal in the next synth due to very minor timing differences.

Another important feature of `live_audio` is that it will automatically move an existing `live_audio` synth into the current FX context. This means you can live code the FX chain around the live stream and it will update automatically. See examples.

To stop a `live_audio` synth, use the `:stop` arg: `live_audio :foo, :stop`.
.


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

live_audio :foo 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Basic usage
# Play whatever audio is coming into the sound card on input 1



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

live_audio :foo, input: 3 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Specify an input
# Play whatever audio is coming into the sound card on input 3



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

live_audio :foo, input: 3, stereo: true 
                                        



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Work with stereo input
# Play whatever audio is coming into the sound card on inputs 3 and 4
# as a stereo stream



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

live_audio :guitar    

sleep 2               

with_fx :reverb do
  live_audio :guitar  
end

sleep 2               

live_audio :guitar    



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Switching audio contexts (i.e. changing FX)
# Play whatever audio is coming into the sound card on input 1
 
# Wait for 2 seconds then...
 
 
# Add reverb to the audio from input 1
 
 
# Wait for another 2 seconds then...
 
# Remove the reverb from input 1



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


live_loop :foo do
  with_fx [:reverb, :distortion, :echo].choose do  
    live_audio :voice                              
  end                                              
  sleep 8
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Working with live_loops
 
 
# chooses a new FX each time round the live loop
# the audio stream from input 1 will be moved to the
# new FX and the old FX will complete and finish as normal.
 
 



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


live_audio :foo           
live_audio :bar, input: 2 

sleep 3                   

live_audio :foo, :stop    
                          



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Stopping
 
#=> start playing audio from input 1
#=> start playing audio from input 2
 
#=> wait for 3s...
 
#=> stop playing audio from input 1
#=> (live_audio :bar is still playing)



```
<!--- #end tr -->

</td>
</tr>
</table>

