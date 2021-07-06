# Create a ring of boolean values

```
bools 
 <!--- #tr -->list (array)<!--- #end tr -->
```


Create a new ring of booleans values from 1s and 0s, which can be easier to write and manipulate in a live setting.

Introduced in v2.2

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(bools 1, 0)   



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring true, false)



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
(bools 1, 0, true, false, nil)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring true, false, true, false, false)



```
<!--- #end tr -->

</td>
</tr>
</table>

