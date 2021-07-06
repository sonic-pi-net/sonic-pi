# Reset all ticks

```
tick_reset_all 
 <!--- #tr --><!--- #end tr -->
```


Reset all ticks - default and keyed

Introduced in v2.6

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
tick     
  tick
  tick :foo
  tick :foo
  tick :foo
  puts look
  puts look(:foo)
  tick_reset_all
  puts look
  puts look(:foo)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# increment default tick and tick :foo
 
 
 
 
#=> 1
#=> 2
 
#=> 0
#=> 0



```
<!--- #end tr -->

</td>
</tr>
</table>

