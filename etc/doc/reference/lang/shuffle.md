# Randomise order of a list

```
shuffle 
 <!--- #tr -->list (array)<!--- #end tr -->
```


Returns a new list with the same elements as the original but with their order shuffled. Also works for strings

Introduced in v2.1

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
shuffle [1, 2, 3, 4]



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Would return something like: [3, 4, 2, 1]



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
shuffle "foobar" 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Would return something like: "roobfa"



```
<!--- #end tr -->

</td>
</tr>
</table>

