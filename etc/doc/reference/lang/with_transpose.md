# Block-level note transposition

```
with_transpose 
 <!--- #tr -->note_shift (number)<!--- #end tr -->
```


Similar to use_transpose except only applies to code within supplied `do`/`end` block. Previous transpose value is restored after block. To transpose entire octaves see `with_octave`.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_transpose 3
play 62

with_transpose 12 do
  play 50
  sleep 1
  play 72
end


play 80



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# Plays note 65
 
 
# Plays note 62
 
# Plays note 84
 
 
# Original transpose value is restored
# Plays note 83



```
<!--- #end tr -->

</td>
</tr>
</table>

