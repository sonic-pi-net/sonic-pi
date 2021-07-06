# Generate a random float between two numbers

```
rrand 
 <!--- #tr -->min (number), max (number)<!--- #end tr -->
```


Given two numbers, this produces a float between the supplied min and max values exclusively. Both min and max need to be supplied. For random integers, see `rrand_i`. If optional arg `step:` is used, the result is quantised by step.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
print rrand(0, 10)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> will print a number like 8.917730007820797 to the output pane



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
    play rrand(60, 72)
    sleep 0.125
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
#=> Will play a random non-integer midi note between C4 (60) and C5 (72) such as 67.3453 or 71.2393
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

