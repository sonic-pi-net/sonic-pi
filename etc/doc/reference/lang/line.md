# Create a ring buffer representing a straight line

```
line 
 <!--- #tr -->start (number), finish (number)<!--- #end tr -->
```


Create a ring buffer representing a straight line between start and finish of steps elements. Steps defaults to `4`. Indexes wrap around positively and negatively. Similar to `range`.

Introduced in v2.5

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(line 0, 4, steps: 4)   



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 0.0, 1.0, 2.0, 3.0)



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
(line 5, 0, steps: 5)   



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring 5.0, 4.0, 3.0, 2.0, 1.0)



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
(line 0, 3, inclusive: true)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 0.0, 1.0, 2.0, 3.0)



```
<!--- #end tr -->

</td>
</tr>
</table>

