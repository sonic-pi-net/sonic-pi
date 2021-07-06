# Minecraft Pi - get ground height at x, z coords

```
mc_ground_height 
 <!--- #tr -->x (number), z (number)<!--- #end tr -->
```


Returns the height of the ground at the specified `x` and `z` coords.

Introduced in v2.5

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts mc_ground_height 40, 50



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> 43 (height of world at x=40, z=50)



```
<!--- #end tr -->

</td>
</tr>
</table>

