# Create a ring of successive halves

```
halves 
 <!--- #tr -->start (number), num_halves (int)<!--- #end tr -->
```


Create a ring containing the results of successive halving of the `start` value. If `num_halves` is negative, will return a ring of `doubles`.

Introduced in v2.10

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(halves 60, 2) 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 60, 30)



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
(halves 120, 3)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring 120, 60, 30)



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
(halves 120, 5)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 120, 60, 30, 15, 7.5)



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
(halves 30, -5)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring 30, 60, 120, 240, 480)



```
<!--- #end tr -->

</td>
</tr>
</table>

