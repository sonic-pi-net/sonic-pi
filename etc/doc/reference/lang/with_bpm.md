# Set the tempo for the code block

```
with_bpm 
 <!--- #tr -->bpm (number)<!--- #end tr -->
```


Sets the tempo in bpm (beats per minute) for everything in the given block. Affects all containing calls to `sleep` and all temporal synth arguments which will be scaled to match the new bpm. See also `use_bpm`

  For dance music here's a rough guide for which BPM to aim for depending on your genre:

  * Dub: 60-90 bpm
  * Hip-hop: 60-100 bpm
  * Downtempo: 90-120 bpm
  * House: 115-130 bpm
  * Techno/trance: 120-140 bpm
  * Dubstep: 135-145 bpm
  * Drum and bass: 160-180 bpm
  

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

  4.times do
    sample :drum_bass_hard
    sleep 1
  end

  sleep 5

 
 
  with_bpm 120 do 
    4.times do
      sample :drum_bass_hard
      sleep 1
    end
  end

  sleep 5

 
  4.times do
    sample :drum_bass_hard
    sleep 1
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# default tempo is 60 bpm
 
 
# sleeps for 1 second
 
 
# sleeps for 5 seconds
 
# with_bpm sets a tempo for everything between do ... end (a block)
# Hear how it gets faster?
# set bpm to be twice as fast
 
 
# now sleeps for 0.5 seconds
 
 
 
 
 
# bpm goes back to normal
 
 
# sleeps for 1 second
 



```
<!--- #end tr -->

</td>
</tr>
</table>

