# Block-level enable and disable MIDI logging

```
with_midi_logging 
 <!--- #tr -->true_or_false (boolean)<!--- #end tr -->
```


Similar to use_midi_logging except only applies to code within supplied `do`/`end` block. Previous MIDI log value is restored after block.

Introduced in v3.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

  use_midi_logging true

  midi :e1

  with_midi_logging false do
   
    midi :f2
  end
  sleep 1
 
  midi :G3



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Turn on MIDI logging:
 
 
#  message is printed to log
 
 
#MIDI logging is now disabled
# MIDI message *is* sent but not displayed in log
 
 
# Debug is re-enabled
# message is displayed in log



```
<!--- #end tr -->

</td>
</tr>
</table>

