# Generate a random number without consuming a rand

```
rand_look 
 <!--- #tr -->max (number_or_range)<!--- #end tr -->
```


Given a max number, produces a number between `0` and the supplied max value exclusively. If max is a range produces an int within the range. With no args returns a value between `0` and `1`.

Does not consume a random value from the stream. Therefore, multiple sequential calls to `rand_look` will all return the same value.

Introduced in v2.11

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
print rand_look(0.5)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> will print a number like 0.375030517578125 to the output pane



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
print rand_look(0.5)
  print rand_look(0.5)
  print rand_look(0.5)
  print rand(0.5)
  print rand_look(0.5)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> will print a number like 0.375030517578125 to the output pane
#=> will print the same number again
#=> will print the same number again
#=> will print a different random number
#=> will print the same number as the previous line again.



```
<!--- #end tr -->

</td>
</tr>
</table>

