# Minecraft Pi - get current location

```
mc_location 
 <!--- #tr --><!--- #end tr -->
```


Returns a list of floats `[x, y, z]` coords of the current location for Steve. The coordinates are finer grained than raw block coordinates but may be used anywhere you might use block coords.

Introduced in v2.5

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts mc_location   



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> [10.1, 20.67, 101.34]



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 2 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
x, y, z = mc_location      



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Find the current location and store in x, y and z variables.



```
<!--- #end tr -->

</td>
</tr>
</table>

