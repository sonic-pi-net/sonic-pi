# Play current synth

```
play 
 <!--- #tr -->note (symbol_or_number)<!--- #end tr -->
```


Play note with current synth. Accepts a set of standard options which include control of an amplitude envelope with `attack:`, `decay:`, `sustain:` and `release:` phases. These phases are triggered in order, so the duration of the sound is attack + decay + sustain + release times. The duration of the sound does not affect any other notes. Code continues executing whilst the sound is playing through its envelope phases.

If `duration:` is supplied and `sustain:` isn't, it causes `sustain:` to be set so that all four phases add up to the duration.

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See `use_synth` and `with_synth` for changing the current synth.

If note is `nil`, `:r` or `:rest`, play is ignored and treated as a rest. Also, if the `on:` opt is specified and returns `false`, or `nil` then play is similarly ignored and treated as a rest.

Note that the default opts listed are only a guide to the most common opts across all the synths. Not all synths support all the default opts and each synth typically supports many more opts specific to that synth. For example, the `:tb303` synth supports 45 unique opts. For a full list of a synth's opts see its documentation in the Help system.
    

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play 50



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Plays note 50 on the current synth



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
play 50, attack: 1



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Plays note 50 with a fade-in time of 1s



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
play 62, pan: -1, release: 3



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Play note 62 in the left ear with a fade-out time of 3s.



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

s = play :e3, release: 4
sleep 1
control s, note: :e5
sleep 0.5
use_synth :dsaw
play :e3  



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# controlling a synth synchronously
 
 
 
 
 
# This is triggered after 1.5s from start



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

play :e3, release: 4 do |s|
  sleep 1                                              
  control s, note: :e5                                 
end

sleep 0.5
use_synth :dsaw
play :e3



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Controlling a synth asynchronously
 
# This block is run in an implicit in_thread
# and therefore is asynchronous
 
 
 
 
# This is triggered after 0.5s from start



```
<!--- #end tr -->

</td>
</tr>
</table>

