# Send an individual MIDI clock tick

```
midi_clock_tick 
 <!--- #tr --><!--- #end tr -->
```


Sends a MIDI clock tick message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Typical MIDI devices expect the clock to send 24 ticks per quarter note (typically a beat). See `midi_clock_beat` for a simple way of sending all the ticks for a given beat.

[MIDI 1.0 Specification - System Real-Time Messages - Timing Clock](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)


Introduced in v3.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_clock_tick



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Send an individual clock tick to all connected MIDI devices on all ports.



```
<!--- #end tr -->

</td>
</tr>
</table>

