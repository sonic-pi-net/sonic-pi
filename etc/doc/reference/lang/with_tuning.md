# Block-level tuning modification

```
with_tuning 
 <!--- #tr -->tuning (symbol), fundamental_note (symbol_or_number)<!--- #end tr -->
```


Similar to use_tuning except only applies to code within supplied `do`/`end` block. Previous tuning value is restored after block.

Introduced in v2.6

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_tuning :equal, :c
play :e4
with_tuning :just, :c do
  play :e4
  sleep 1
  play :c4
end

play :e4



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# Plays note 64
 
# Plays note 63.8631
 
# Plays note 60
 
# Original tuning value is restored
# Plays note 64



```
<!--- #end tr -->

</td>
</tr>
</table>

