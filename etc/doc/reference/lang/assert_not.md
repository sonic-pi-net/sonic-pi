# Ensure arg is not valid

```
assert_not 
 <!--- #tr -->arg (anything)<!--- #end tr -->
```


Raises an exception if the argument is not either nil or false.

Introduced in v3.3

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

assert_not false  
assert_not nil    
assert_not 1 == 5 
assert true 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Simple assertions
# As false is either nil or false, this assertion passes
# As nil is either nil or false, this assertion passes
# These numbers are not equal
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

assert_not true , "oops"



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
</table>

