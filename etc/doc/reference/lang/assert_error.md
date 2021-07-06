# Ensure block throws an error

```
assert_error 
 <!--- #tr -->class (Exception)<!--- #end tr -->
```


Runs the block and ensures that it raises the correct Exception. Useful for asserting that an Exception will be raised. You may specify the particular Exception class, which defaults to `Exception`.

Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
assert_error do
  play 70
end                        
                           



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
# Will throw an exception: "Assert error failed!" as the block
# contains no errors.



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
assert_error do
  1 / 0
end                        



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
# Will not throw an exception as the block contains an error.



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
assert_error ZeroDivisionError do
  1 / 0
end                        



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
# Will not throw an exception as the block contains a ZeroDivisionError.



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
assert_error ThreadError do
  1 / 0
end                        
                           



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
# Will throw an exception as the block contains a ZeroDivisionError rather than
# a ThreadError.



```
<!--- #end tr -->

</td>
</tr>
</table>

