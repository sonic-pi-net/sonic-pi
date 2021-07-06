# Ensure args are not equal

```
assert_not_equal 
 <!--- #tr -->arg1 (anything), arg2 (anything)<!--- #end tr -->
```


Raises an exception if both arguments are qual. 

Introduced in v3.3

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

assert_not_equal 1, 3
assert_not_equal 1, -1
assert_not_equal 1, :foo


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

assert_not_equal 3, 3, "something is seriously wrong!"


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Add messages to the exceptions
 



```
<!--- #end tr -->

</td>
</tr>
</table>

