# Print a string representing a list of numeric values as a spark graph/bar chart

```
spark 
 <!--- #tr --><!--- #end tr -->
```


Given a list of numeric values, this method turns them into a string of bar heights and prints them out. Useful for quickly graphing the shape of an array.

Introduced in v2.5

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
spark (range 1, 5)   



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> ▁▃▅█



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
spark (range 1, 5).shuffle



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> ▃█▅▁



```
<!--- #end tr -->

</td>
</tr>
</table>

