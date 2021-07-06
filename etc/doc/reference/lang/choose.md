# Random list selection

```
choose 
 <!--- #tr -->list (array)<!--- #end tr -->
```


Choose an element at random from a list (array).

If no arguments are given, will return a lambda function which when called takes an argument which will be a list to be chosen from. This is useful for choosing random `onset:` vals for samples

Always returns a single element (or nil)

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
loop do
    play choose([60, 64, 67])
    sleep 1
    play chord(:c, :major).choose
    sleep 1
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
#=> plays one of 60, 64 or 67 at random
 
#=> You can also call .choose on the list
 
 



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

live_loop :foo do
  sample :loop_amen, onset: choose  
  sleep 0.125
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Using choose for random sample onsets
 
# choose a random onset value each time
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

