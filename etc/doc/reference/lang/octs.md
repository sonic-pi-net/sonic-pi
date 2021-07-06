# Create a ring of octaves

```
octs 
 <!--- #tr -->start (note), num_octaves (pos_int)<!--- #end tr -->
```


Create a ring of successive octaves starting at `start` for `num_octaves`. 

Introduced in v2.8

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(octs 60, 2) 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 60, 72)



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
(octs :e3, 3)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring 52, 64, 76)



```
<!--- #end tr -->

</td>
</tr>
</table>

