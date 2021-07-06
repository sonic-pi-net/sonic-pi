# Generate a random whole number below a value (exclusive)

```
rand_i 
 <!--- #tr -->max (number_or_range)<!--- #end tr -->
```


Given a max number, produces a whole number between `0` and the supplied max value exclusively. If max is a range produces an int within the range. With no args returns either `0` or `1`

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
print rand_i(5)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> will print either 0, 1, 2, 3, or 4 to the output pane



```
<!--- #end tr -->

</td>
</tr>
</table>

