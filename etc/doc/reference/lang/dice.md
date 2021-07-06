# Random dice throw

```
dice 
 <!--- #tr -->num_sides (number)<!--- #end tr -->
```


Throws a dice with the specified num_sides (defaults to `6`) and returns the score as a number between `1` and `num_sides`.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
dice
      



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# will return a number between 1 and 6 inclusively
# (with an even probability distribution).



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
dice 3



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# will return a number between 1 and 3 inclusively



```
<!--- #end tr -->

</td>
</tr>
</table>

