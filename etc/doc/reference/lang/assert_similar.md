# Ensure args are similar

```
assert_similar 
 <!--- #tr -->arg1 (anything), arg2 (anything)<!--- #end tr -->
```


Raises an exception if both arguments aren't similar.

Currently similarity is only defined for numbers - all other types are compared for equality with assert_equal.

Useful for testing in cases where floating point imprecision stops you from being able to use `assert_equal`. 

Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

assert_similar 1, 1



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Simple assertions
#=> True



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

assert_similar(4.9999999999, 5.0)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Handles floating point imprecision
#=> True



```
<!--- #end tr -->

</td>
</tr>
</table>

