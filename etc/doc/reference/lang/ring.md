# Create a ring buffer

```
ring 
 <!--- #tr -->list (array)<!--- #end tr -->
```


Create a new immutable ring buffer from args. Indexes wrap around positively and negatively

Introduced in v2.2

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(ring 1, 2, 3)[0]



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> 1



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
(ring 1, 2, 3)[1]



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> 2



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
(ring 1, 2, 3)[3]



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> 1



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
(ring 1, 2, 3)[-1]



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> 3



```
<!--- #end tr -->

</td>
</tr>
</table>

