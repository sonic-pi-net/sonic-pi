# Trigger and release an external synth via MIDI

```
midi 
 <!--- #tr -->note (number)<!--- #end tr -->
```


Sends a MIDI note on event to *all* connected MIDI devices and *all* channels and then after sustain beats sends a MIDI note off event. Ensures MIDI trigger is synchronised with standard calls to play and sample. Co-operates completely with Sonic Pi's timing system including `time_warp`.

If `note` is specified as `:off` then all notes will be turned off (same as `midi_all_notes_off`).


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi :e1, sustain: 0.3, vel_f: 0.5, channel: 3



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Play E, octave 1 for 0.3 beats at half velocity on channel 3 on all connected MIDI ports.



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
midi :off, channel: 3



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Turn off all notes on channel 3 on all connected MIDI ports



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
midi :e1, channel: 3, port: "foo"



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Play note :E1 for 1 beats on channel 3 on MIDI port named "foo" only



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
live_loop :arp do
  midi (octs :e1, 3).tick, sustain: 0.1
  sleep 0.125
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
# repeatedly play a ring of octaves
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

