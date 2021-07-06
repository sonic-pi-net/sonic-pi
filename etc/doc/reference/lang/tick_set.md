# Set tick to a specific value

```
tick_set 
 <!--- #tr -->value (number)<!--- #end tr -->
```


Set the default tick to the specified `value`. If a `key` is referenced, set that tick to `value` instead. Next call to `look` will return `value`.

Introduced in v2.6

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
tick_set 40
  puts look  



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# set default tick to 40
#=> 40



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
tick_set :foo, 40
  puts look(:foo)  
  puts look        



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# set tick :foo to 40
#=> 40 (tick :foo is now 40)
#=> 0 (default tick is unaffected)



```
<!--- #end tr -->

</td>
</tr>
</table>

