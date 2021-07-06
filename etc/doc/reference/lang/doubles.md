# Create a ring of successive doubles

```
doubles 
 <!--- #tr -->start (number), num_doubles (int)<!--- #end tr -->
```


Create a ring containing the results of successive doubling of the `start` value. If `num_doubles` is negative, will return a ring of `halves`.

Introduced in v2.10

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(doubles 60, 2) 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 60, 120)



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
(doubles 1.5, 3)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring 1.5, 3, 6)



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
(doubles 1.5, 5)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 1.5, 3, 6, 12, 24)



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
(doubles 100, -4)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring 100, 50, 25, 12.5)



```
<!--- #end tr -->

</td>
</tr>
</table>

