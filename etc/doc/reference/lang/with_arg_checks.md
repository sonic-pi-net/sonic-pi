# Block-level enable and disable arg checks

```
with_arg_checks 
 <!--- #tr -->true_or_false (boolean)<!--- #end tr -->
```


Similar to `use_arg_checks` except only applies to code within supplied `do`/`end` block. Previous arg check value is restored after block.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

use_arg_checks true

play 80, cutoff: 100

with_arg_checks false do
 
  play 50, release: 3
  sleep 1
  play 72            
end


play 90



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Turn on arg checking:
 
 
# Args are checked
 
 
#Arg checking is now disabled
# Args are not checked
 
# Arg is not checked
 
 
# Arg checking is re-enabled
# Args are checked



```
<!--- #end tr -->

</td>
</tr>
</table>

