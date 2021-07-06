# Block-level merge midi defaults

```
with_merged_midi_defaults 
 <!--- #tr --><!--- #end tr -->
```


Specify opt values to be used by any following call to the `midi_*` fns within the specified `do`/`end` block. Merges the specified values with any previous midi defaults, rather than replacing them. After the `do`/`end` block has completed, previous defaults (if any) are restored.

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

use_midi_defaults channel: 3, port: "foo"

midi_note_on :e3

with_merged_midi_defaults channel: 1 do

  midi_note_on :e2
                  
                  
end

midi_note_on :e2
                
                



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Sends MIDI :e1 note_on with default opts
 
 
 
# Sends MIDI :e3 note_on to channel 3 on port "foo"
 
 
 
# Sends MIDI :e2 note_on to channel 1 on port "foo".
# This is because the call to use_merged_midi_defaults overrode the
# channel but not the port which got merged in.
 
 
# Sends MIDI :e2 note_on to channel 3 on port "foo".
# This is because the previous defaults were restored after
# the call to with_merged_midi_defaults.



```
<!--- #end tr -->

</td>
</tr>
</table>

