# Block-level use new sample defaults

```
with_sample_defaults 
 <!--- #tr --><!--- #end tr -->
```


Specify new default values to be used by all subsequent calls to `sample` within the `do`/`end` block. After the `do`/`end` block has completed, the previous sampled defaults (if any) are restored. For the contents of the block, will remove and override any previous defaults.

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

with_sample_defaults cutoff: 90 do
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
 
 
# plays amen break with a cutoff of 90 and defaults for rest of args - note that amp is no longer 0.5
 
 
# plays amen break with a cutoff of 70 and amp is 0.5 again as the previous defaults are restored.



```
<!--- #end tr -->

</td>
</tr>
</table>

