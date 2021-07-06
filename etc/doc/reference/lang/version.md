# Get current version information

```
version 
 <!--- #tr --><!--- #end tr -->
```


Return information representing the current version of Sonic Pi. This information may be further inspected with `version.major`, `version.minor`, `version.patch` and `version.dev`

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts version



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# => Prints out the current version such as v2.0.1



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
puts version.major



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# => Prints out the major version number such as 2



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 3 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts version.minor



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# => Prints out the minor version number such as 0



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 4 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
puts version.patch



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# => Prints out the patch level for this version such as 0



```
<!--- #end tr -->

</td>
</tr>
</table>

