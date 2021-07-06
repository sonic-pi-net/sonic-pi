# Set the tempo

```
use_bpm 
 <!--- #tr -->bpm (number)<!--- #end tr -->
```


Sets the tempo in bpm (beats per minute) for everything afterwards. Affects all subsequent calls to `sleep` and all temporal synth arguments which will be scaled to match the new bpm. If you wish to bypass scaling in calls to sleep, see the fn `rt`. Also, if you wish to bypass time scaling in synth args see `use_arg_bpm_scaling`. See also `with_bpm` for a block scoped version of `use_bpm`.

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
    play 50, attack: 0.5, release: 0.25
    sleep 1
  end

  sleep 2 

 
  use_bpm 120 
  4.times do
    play 62, attack: 0.5, release: 0.25
    sleep 1
  end

  sleep 2

 
  use_bpm 240 
  8.times do
    play 62, attack: 0.5, release: 0.25
    sleep 1
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# default tempo is 60 bpm
 
# attack is 0.5s and release is 0.25s
# sleep for 1 second
 
 
# sleep for 2 seconds
 
# Let's make it go faster...
# double the bpm
 
# attack is scaled to 0.25s and release is now 0.125s
# actually sleeps for 0.5 seconds
 
 
# sleep for 1 second
 
# Let's make it go even faster...
#  bpm is 4x original speed!
 
# attack is scaled to 0.125s and release is now 0.0625s
# actually sleeps for 0.25 seconds
 



```
<!--- #end tr -->

</td>
</tr>
</table>

