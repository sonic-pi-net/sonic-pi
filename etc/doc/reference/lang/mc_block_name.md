# Minecraft Pi - normalise block name

```
mc_block_name 
 <!--- #tr -->id (number_or_symbol)<!--- #end tr -->
```


Given a block id or a block name will return a symbol representing the block name or throw an exception if the id or name isn't valid.

Introduced in v2.5

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts mc_block_name :air



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
<tr>
<th colspan="2" class="odd head"># Example 2 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
puts mc_block_name 0  



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> :air



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
puts mc_block_name 19



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Throws an invalid block id exception



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
puts mc_block_name :foo



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Throws an invalid block name exception



```
<!--- #end tr -->

</td>
</tr>
</table>

