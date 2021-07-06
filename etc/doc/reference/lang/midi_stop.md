# Send MIDI system message - stop

```
midi_stop 
 <!--- #tr --><!--- #end tr -->
```


Sends the MIDI stop system message to *all* connected MIDI devices on *all* ports.  Use the `port:` opt to restrict which MIDI ports are used.

Stops the current sequence.

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
midi_stop



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Send stop message to all connected MIDI devices



```
<!--- #end tr -->

</td>
</tr>
</table>

