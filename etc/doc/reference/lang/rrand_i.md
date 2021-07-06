# Generate a random whole number between two points inclusively

```
rrand_i 
 <!--- #tr -->min (number), max (number)<!--- #end tr -->
```


Given two numbers, this produces a whole number between the min and max you supplied inclusively. Both min and max need to be supplied. For random floats, see `rrand`

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
print rrand_i(0, 10)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> will print a random number between 0 and 10 (e.g. 4, 0 or 10) to the output pane



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
loop do
    play rrand_i(60, 72)
    sleep 0.125
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
#=> Will play a random midi note between C4 (60) and C5 (72)
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

