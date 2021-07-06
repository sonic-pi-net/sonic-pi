# Use new MIDI defaults

```
use_midi_defaults 
 <!--- #tr --><!--- #end tr -->
```


Specify new default values to be used by all subsequent calls to `midi_*` fns. Will remove and override any previous defaults.

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

use_midi_defaults channel: 1

midi_note_on :e2



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Sends MIDI :e1 note_on with default opts
 
 
 
# Sends MIDI :e3 note_on to channel 3 on port "foo"
 
 
 
# Sends MIDI :e2 note_on to channel 1. Note that the port is back to the default and no longer "foo".



```
<!--- #end tr -->

</td>
</tr>
</table>

