# Merge new sample defaults

```
use_merged_sample_defaults 
 <!--- #tr --><!--- #end tr -->
```


Specify new default values to be used by all subsequent calls to `sample`. Merges the specified values with any previous defaults, rather than replacing them.

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

use_merged_sample_defaults cutoff: 90

sample :loop_amen 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# plays amen break with default arguments
 
 
 
# plays amen break with an amp of 0.5, cutoff of 70 and defaults for rest of args
 
 
 
# plays amen break with a cutoff of 90 and and an amp of 0.5 with defaults for rest of args



```
<!--- #end tr -->

</td>
</tr>
</table>

