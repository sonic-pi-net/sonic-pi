# Ensure args are equal

```
assert_equal 
 <!--- #tr -->arg1 (anything), arg2 (anything)<!--- #end tr -->
```


Raises an exception if both arguments aren't equal. 

Introduced in v2.8

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

assert_equal 1, 1


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Simple assertions
 



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

assert_equal 1 + 1, 2
assert_equal [:a, :b, :c].size,  3



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# More interesting assertions
# Ensure that arithmetic is sane!
# ensure lists can be correctly counted



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

assert_equal 3, 5, "something is seriously wrong!"


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Add messages to the exceptions
 



```
<!--- #end tr -->

</td>
</tr>
</table>

