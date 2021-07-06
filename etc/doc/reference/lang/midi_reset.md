# Reset MIDI devices

```
midi_reset 
 <!--- #tr -->value (number)<!--- #end tr -->
```


Sends a MIDI reset all controllers message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All controller values are reset to their defaults.

[MIDI 1.0 Specification - Channel Mode Messages - Reset All Controllers](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_reset



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Reset MIDI devices on all channels (and ports)



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
midi_reset channel: 2



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Reset MIDI devices on channel 2



```
<!--- #end tr -->

</td>
</tr>
</table>

