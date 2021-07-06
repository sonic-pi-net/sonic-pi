# Block-level use new MIDI defaults

```
with_midi_defaults 
 <!--- #tr --><!--- #end tr -->
```


Specify new default values to be used by all calls to `midi_*` fns within the `do`/`end` block. After the `do`/`end` block has completed the previous MIDI defaults (if any) are restored.

Introduced in v3.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_note_on :e1

with_midi_defaults channel: 3, port: "foo" do
  midi_note_on :e3
end

use_midi_defaults channel: 1  

with_midi_defaults channel: 5 do
  midi_note_on :e2
                  
end

  midi_note_on :e4
                  



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Sends MIDI :e1 note on with default opts
 
 
# Sends MIDI :e3 note on to channel 3 on port "foo"
 
 
# this will be overridden by the following
 
 
# Sends MIDI :e2 note on to channel 5.
# Note that the port is back to the default
 
 
# Sends MIDI :e4 note on to channel 1
# Note that the call to use_midi_defaults is now honoured.



```
<!--- #end tr -->

</td>
</tr>
</table>

