# Reset tick to 0

```
tick_reset 
 <!--- #tr --><!--- #end tr -->
```


Reset default tick to 0. If a `key` is referenced, set that tick to 0 instead. Same as calling tick_set(0)

Introduced in v2.6

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

  tick
  tick
  tick
  puts look
  tick_set 0
  puts look



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# increment default tick a few times
 
 
 
#=> 2 (default tick is now 2)
# default tick is now 0
#=> 0 (default tick is now 0



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

  tick :foo
  tick :foo
  tick :foo
  puts look(:foo)
  tick_set 0
  puts look(:foo)
  tick_set :foo, 0
  puts look(:foo)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# increment tick :foo a few times
 
 
 
#=> 2 (tick :foo is now 2)
# default tick is now 0
#=> 2 (tick :foo is still 2)
#  reset tick :foo
#=> 0 (tick :foo is now 0)



```
<!--- #end tr -->

</td>
</tr>
</table>

