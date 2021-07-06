# Send MIDI system message - continue

```
midi_continue 
 <!--- #tr --><!--- #end tr -->
```


Sends the MIDI continue system message to *all* connected MIDI devices on *all* ports.  Use the `port:` opt to restrict which MIDI ports are used.

Upon receiving the MIDI continue event, the MIDI device(s) will continue at the point the sequence was stopped.

[MIDI 1.0 Specification - System Real-Time Messages - Continue](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)


Introduced in v3.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_continue



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Send continue message to all connected MIDI devices



```
<!--- #end tr -->

</td>
</tr>
</table>

