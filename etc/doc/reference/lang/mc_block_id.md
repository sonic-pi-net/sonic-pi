# Minecraft Pi - normalise block code

```
mc_block_id 
 <!--- #tr -->name (symbol_or_number)<!--- #end tr -->
```


Given a block name or id will return a number representing the id of the block or throw an exception if the name or id isn't valid

Introduced in v2.5

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts mc_block_id :air



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> 0



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
puts mc_block_id 0 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> 0



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
puts mc_block_id 19



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
puts mc_block_id :foo



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

