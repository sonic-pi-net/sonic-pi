# Send a quarter-note's worth of MIDI clock ticks

```
midi_clock_beat 
 <!--- #tr -->duration (beats)<!--- #end tr -->
```


Sends enough MIDI clock ticks for one beat to *all* connected MIDI devices. Use the `port:` opt to restrict which MIDI ports are used.

The MIDI specification requires 24 clock tick events to be sent per beat. These can either be sent manually using `midi_clock_tick` or all 24 can be scheduled in one go using this fn. `midi_clock_beat` will therefore schedule for 24 clock ticks to be sent linearly spread over duration beats. This fn will automatically take into account the current BPM and any `time_warp`s.


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_clock_beat



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Send 24 clock ticks over a period of 1 beat



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
midi_clock_beat 0.5



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Send 24 clock ticks over a period of 0.5 beats



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
live_loop :clock do 
  midi_clock_beat   
  sleep 1
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Create a live loop which continually sends out MIDI clock
# events at the current BPM
 
 



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

live_loop :clock do
  midi_start if tick == 0
  midi_clock_beat        
  sleep 1                
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Ensuring Clock Phase is Correct
 
# Send a midi_start event the first time round the live loop only
# this will not just send a steady clock beat, but also ensure
# the clock phase of the MIDI device matches Sonic Pi.
 



```
<!--- #end tr -->

</td>
</tr>
</table>

