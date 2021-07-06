# Block-level use merged sample defaults

```
with_merged_sample_defaults 
 <!--- #tr --><!--- #end tr -->
```


Specify new default values to be used by all subsequent calls to `sample` within the `do`/`end` block.  Merges the specified values with any previous sample defaults, rather than replacing them. After the `do`/`end` block has completed, the previous sampled defaults (if any) are restored.

Introduced in v2.9

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
sample :loop_amen

use_merged_sample_defaults amp: 0.5, cutoff: 70

sample :loop_amen

with_merged_sample_defaults cutoff: 90 do
  sample :loop_amen 
end

sample :loop_amen 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# plays amen break with default arguments
 
 
 
# plays amen break with an amp of 0.5, cutoff of 70 and defaults for rest of args
 
 
# plays amen break with a cutoff of 90 and amp of 0.5
 
 
# plays amen break with a cutoff of 70 and amp is 0.5 again as the previous defaults are restored.



```
<!--- #end tr -->

</td>
</tr>
</table>

