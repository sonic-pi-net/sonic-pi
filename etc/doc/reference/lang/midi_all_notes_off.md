# Turn off all notes on MIDI devices

```
midi_all_notes_off 
 <!--- #tr --><!--- #end tr -->
```


Sends a MIDI all notes off message to *all* connected MIDI devices. on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

When an All Notes Off event is received, all oscillators will turn off.

[MIDI 1.0 Specification - Channel Mode Messages - All Notes Off](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_all_notes_off



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Turn off all notes on MIDI devices on all channels (and ports)



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
midi_all_notes_off channel: 2



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Turn off all notes on MIDI devices on channel 2



```
<!--- #end tr -->

</td>
</tr>
</table>

