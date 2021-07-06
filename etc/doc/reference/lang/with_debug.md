# Block-level enable and disable debug

```
with_debug 
 <!--- #tr -->true_or_false (boolean)<!--- #end tr -->
```


Similar to use_debug except only applies to code within supplied `do`/`end` block. Previous debug value is restored after block.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

use_debug true

play 80

with_debug false do
 
  play 50
  sleep 1
  play 72
end


play 90



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Turn on debugging:
 
 
# Debug message is sent
 
 
#Debug is now disabled
# Debug message is not sent
 
# Debug message is not sent
 
 
# Debug is re-enabled
# Debug message is sent



```
<!--- #end tr -->

</td>
</tr>
</table>

