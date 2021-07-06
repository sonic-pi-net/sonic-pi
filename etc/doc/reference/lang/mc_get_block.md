# Minecraft Pi - get type of block at coords

```
mc_get_block 
 <!--- #tr -->x (number), y (number), z (number)<!--- #end tr -->
```


Returns the type of the block at the coords `x`, `y`, `z` as a symbol.

Introduced in v2.5

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts mc_get_block 40, 50, 60



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> :air



```
<!--- #end tr -->

</td>
</tr>
</table>

