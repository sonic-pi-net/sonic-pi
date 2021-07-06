# Enable and disable MIDI logging

```
use_midi_logging 
 <!--- #tr -->true_or_false (boolean)<!--- #end tr -->
```


Enable or disable log messages created on MIDI functions. This does not disable the MIDI functions themselves, it just stops them from being printed to the log

Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_midi_logging true



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Turn on MIDI logging



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
use_midi_logging false



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Disable MIDI logging



```
<!--- #end tr -->

</td>
</tr>
</table>

