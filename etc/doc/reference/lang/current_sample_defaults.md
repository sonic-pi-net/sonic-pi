# Get current sample defaults

```
current_sample_defaults 
 <!--- #tr --><!--- #end tr -->
```


Returns the current sample defaults. This is a map of synth arg names to either values or functions.

This can be set via the fns `use_sample_defaults`, `with_sample_defaults`, `use_merged_sample_defaults` and `with_merged_sample_defaults`.

Introduced in v2.5

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_sample_defaults amp: 0.5, cutoff: 80
sample :loop_amen
puts current_sample_defaults



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# Plays amen break with amp 0.5 and cutoff 80
#=> Prints {amp: 0.5, cutoff: 80}



```
<!--- #end tr -->

</td>
</tr>
</table>

