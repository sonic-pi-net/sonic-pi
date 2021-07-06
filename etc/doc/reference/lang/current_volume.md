# Get current volume

```
current_volume 
 <!--- #tr --><!--- #end tr -->
```


Returns the current volume.

This can be set via the fn `set_volume!`.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts current_volume



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Print out the current volume



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
set_volume! 2
puts current_volume



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
#=> 2



```
<!--- #end tr -->

</td>
</tr>
</table>

