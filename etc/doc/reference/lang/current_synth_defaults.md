# Get current synth defaults

```
current_synth_defaults 
 <!--- #tr --><!--- #end tr -->
```


Returns the current synth defaults. This is a map of synth arg names to values.

This can be set via the fns `use_synth_defaults`, `with_synth_defaults`, `use_merged_synth_defaults` and `with_merged_synth_defaults`.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_synth_defaults amp: 0.5, cutoff: 80
play 50
puts current_synth_defaults



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# Plays note 50 with amp 0.5 and cutoff 80
#=> Prints {amp: 0.5, cutoff: 80}



```
<!--- #end tr -->

</td>
</tr>
</table>

