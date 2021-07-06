# Block-level cent tuning

```
with_cent_tuning 
 <!--- #tr -->cent_shift (number)<!--- #end tr -->
```


Similar to `use_cent_tuning` except only applies cent shift to code within supplied `do`/`end` block. Previous cent tuning value is restored after block. One semitone consists of 100 cents. To transpose entire semitones see `with_transpose`.

Introduced in v2.9

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_cent_tuning 1
play 50

with_cent_tuning 2 do
  play 50
end


play 50



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# Plays note 50.01
 
 
# Plays note 50.02
 
 
# Original cent tuning value is restored
# Plays note 50.01



```
<!--- #end tr -->

</td>
</tr>
</table>

