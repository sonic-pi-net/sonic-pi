# Random true value with specified probability

```
one_in 
 <!--- #tr -->num (number)<!--- #end tr -->
```


Returns `true` or `false` with a specified probability - it will return true every one in num times where num is the param you specify

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
one_in 2



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# will return true with a probability of 1/2, false with probability 1/2



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
one_in 3



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# will return true with a probability of 1/3, false with a probability of 2/3



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
one_in 100



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# will return true with a probability of 1/100, false with a probability of 99/100



```
<!--- #end tr -->

</td>
</tr>
</table>

