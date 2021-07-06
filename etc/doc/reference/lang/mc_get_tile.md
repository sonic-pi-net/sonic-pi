# Minecraft Pi - get location of current tile/block

```
mc_get_tile 
 <!--- #tr --><!--- #end tr -->
```


Returns the coordinates of the nearest block that the player is next to. This is more course grained than `mc_location` as it only returns whole number coordinates.

Introduced in v2.5

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts mc_get_tile



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> [10, 20, 101]



```
<!--- #end tr -->

</td>
</tr>
</table>

