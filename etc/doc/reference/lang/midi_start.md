# Send MIDI system message - start

```
midi_start 
 <!--- #tr --><!--- #end tr -->
```


Sends the MIDI start system message to *all* connected MIDI devices on *all* ports.  Use the `port:` opt to restrict which MIDI ports are used.

Start the current sequence playing. (This message should be followed with calls to `midi_clock_tick` or `midi_clock_beat`).

[MIDI 1.0 Specification - System Real-Time Messages - Start](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)


Introduced in v3.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_start



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Send start message to all connected MIDI devices



```
<!--- #end tr -->

</td>
</tr>
</table>

