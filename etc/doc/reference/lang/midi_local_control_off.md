# Disable local control on MIDI devices

```
midi_local_control_off 
 <!--- #tr --><!--- #end tr -->
```


Sends a MIDI local control off message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All devices on a given channel will respond only to data received over MIDI. Played data, etc. will be ignored. See `midi_local_control_on` to enable local control.

[MIDI 1.0 Specification - Channel Mode Messages - Local Control Off](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_local_control_off



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Disable local control on MIDI devices on all channels (and ports)



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
midi_local_control_off channel: 2



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Disable local control on MIDI devices on channel 2



```
<!--- #end tr -->

</td>
</tr>
</table>

