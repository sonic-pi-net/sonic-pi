# Randomly pick from list (with duplicates)

```
pick 
 <!--- #tr -->list (array), n (number_or_nil)<!--- #end tr -->
```


Pick n elements from list or ring. Unlike shuffle, after each element has been picked, it is 'returned' to the list so it may be picked again. This means there may be duplicates in the result. If n is greater than the size of the ring/list then duplicates are guaranteed to be in the result.

If `n` isn't supplied it defaults to a size of 1.

If no arguments are given, will return a lambda function which when called takes an argument which will be a list to be picked from. This is useful for choosing random `onset:` vals for samples.

Always returns a list-like thing (either an array or ring)

Introduced in v2.10

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts [1, 2, 3, 4, 5].pick(3)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> [4, 4, 3]



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
puts (ring 1, 2, 3, 4, 5).pick(3)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring 4, 4, 3)



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
puts (ring 1, 2).pick(5)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 2, 2, 1, 1, 1)



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
puts (ring 1, 2, 3).pick



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring 3)



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 5 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

live_loop :foo do
  sample :loop_amen, onset: pick  
  sleep 0.125
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Using pick for random sample onsets
 
# pick a random onset value each time
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

