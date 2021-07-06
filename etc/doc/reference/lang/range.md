# Create a ring buffer with the specified start, finish and step size

```
range 
 <!--- #tr -->start (number), finish (number), step_size (number)<!--- #end tr -->
```


Create a new ring buffer from the range arguments (start, finish and step size). Step size defaults to `1`. Indexes wrap around positively and negatively

Introduced in v2.2

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(range 1, 5)   



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 1, 2, 3, 4)



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
(range 1, 5, inclusive: true)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring 1, 2, 3, 4, 5)



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
(range 1, 5, step: 2)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 1, 3)



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
(range 1, -5, step: 2)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring 1, -1, -3)



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 5 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(range 1, -5, step: 2)[-1]



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> -3



```
<!--- #end tr -->

</td>
</tr>
</table>

