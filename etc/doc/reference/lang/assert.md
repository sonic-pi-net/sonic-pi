# Ensure arg is valid

```
assert 
 <!--- #tr -->arg (anything)<!--- #end tr -->
```


Raises an exception if the argument is either nil or false.

Introduced in v2.8

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

assert true  
assert 1     
assert "foo"
assert false 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Simple assertions
# As true is neither nil or false, this assertion passes
# Similarly, 1 passes
# As do string
# This will raise an exception



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

assert false, "oops"



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Communicating error messages
# This will raise an exception containing the message "oops"



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

assert (1 + 1) == 2
assert [:a, :b, :c].size == 3



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# More interesting assertions
# Ensure that arithmetic is sane!
# ensure lists can be correctly counted



```
<!--- #end tr -->

</td>
</tr>
</table>

