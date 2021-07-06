# Minecraft Pi - set block at specific coord

```
mc_set_block 
 <!--- #tr -->x (number), y (number), z (number), block_name (symbol_or_number)<!--- #end tr -->
```


Change the block type of the block at coords `x`, `y`, `z` to `block_type`. The block type may be specified either as a symbol such as `:air` or a number. See `mc_block_ids` and `mc_block_types` for lists of valid symbols and numbers.

Introduced in v2.5

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
mc_set_block :glass, 40, 50, 60



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> set block at coords 40, 50, 60 to type glass



```
<!--- #end tr -->

</td>
</tr>
</table>

