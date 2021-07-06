# Silence all MIDI devices

```
midi_sound_off 
 <!--- #tr --><!--- #end tr -->
```


Sends a MIDI sound off message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All oscillators will turn off, and their volume envelopes are set to zero as soon as possible.

[MIDI 1.0 Specification - Channel Mode Messages - All Sound Off](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_sound_off



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Silence MIDI devices on all ports and channels



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
midi_sound_off channel: 2



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Silence MIDI devices on channel 2



```
<!--- #end tr -->

</td>
</tr>
</table>

