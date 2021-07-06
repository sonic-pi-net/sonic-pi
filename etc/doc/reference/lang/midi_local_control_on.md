# Enable local control on MIDI devices

```
midi_local_control_on 
 <!--- #tr --><!--- #end tr -->
```


Sends a MIDI local control on message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All devices on a given channel will respond both to data received over MIDI and played data, etc. See `midi_local_control_off` to disable local control.

[MIDI 1.0 Specification - Channel Mode Messages - Local Control On](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_local_control_on



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Enable local control on MIDI devices on all channels (and ports)



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
midi_local_control_on channel: 2



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Enable local control on MIDI devices on channel 2



```
<!--- #end tr -->

</td>
</tr>
</table>

