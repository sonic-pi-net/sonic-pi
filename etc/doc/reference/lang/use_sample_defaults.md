# Use new sample defaults

```
use_sample_defaults 
 <!--- #tr --><!--- #end tr -->
```


Specify new default values to be used by all subsequent calls to `sample`. Will remove and override any previous defaults.

Introduced in v2.5

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
sample :loop_amen

use_sample_defaults amp: 0.5, cutoff: 70

sample :loop_amen

use_sample_defaults cutoff: 90

sample :loop_amen 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# plays amen break with default arguments
 
 
 
# plays amen break with an amp of 0.5, cutoff of 70 and defaults for rest of args
 
 
 
# plays amen break with a cutoff of 90 and defaults for rest of args - note that amp is no longer 0.5



```
<!--- #end tr -->

</td>
</tr>
</table>

