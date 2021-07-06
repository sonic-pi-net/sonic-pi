# Get current MIDI defaults

```
current_midi_defaults 
 <!--- #tr --><!--- #end tr -->
```


Returns the current MIDI defaults. This is a map of opt names to values

This can be set via the fns `use_midi_defaults`, `with_midi_defaults`, `use_merged_midi_defaults` and `with_merged_midi_defaults`.

Introduced in v3.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_midi_defaults channel: 1, port: "foo"
midi_note_on :e1
current_midi_defaults



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# Sends MIDI :e1 note on to channel 1 on port "foo"
#=> Prints {channel: 1, port: "foo"}



```
<!--- #end tr -->

</td>
</tr>
</table>

