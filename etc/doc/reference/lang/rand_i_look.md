# Generate a random whole number without consuming a rand

```
rand_i_look 
 <!--- #tr -->max (number_or_range)<!--- #end tr -->
```


Given a max number, produces a whole number between `0` and the supplied max value exclusively. If max is a range produces an int within the range. With no args returns either `0` or `1`.

Does not consume a random value from the stream. Therefore, multiple sequential calls to `rand_i_look` will all return the same value.

Introduced in v2.11

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
print rand_i_look(5)



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
<tr>
<th colspan="2" class="odd head"># Example 2 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
print rand_i_look(5)
print rand_i_look(5)
print rand_i_look(5)
print rand_i(5)
print rand_i_look(5)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> will print either 0, 1, 2, 3, or 4 to the output pane
#=> will print the same number again
#=> will print the same number again
#=> will print either 0, 1, 2, 3, or 4 to the output pane
#=> will print the same number as the previous statement



```
<!--- #end tr -->

</td>
</tr>
</table>

