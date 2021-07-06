# Stretch a sequence of values

```
stretch 
 <!--- #tr -->list (anything), count (number)<!--- #end tr -->
```


Stretches a list of values each value repeated count times. Always returns a ring regardless of the type of the list that is stretched. To preserve type, consider using `.stretch` i.e. `(ramp 1, 2, 3).stretch(2) #=> (ramp 1, 1, 2, 2, 3, 3)`

Introduced in v2.6

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(stretch [1,2], 3)   



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 1, 1, 1, 2, 2, 2)



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
(stretch [:e2, :c3], 1, [:c2, :d3], 2)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring :e2, :c3, :c2, :c2, :d3, :d3)



```
<!--- #end tr -->

</td>
</tr>
</table>

