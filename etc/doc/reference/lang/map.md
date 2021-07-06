# Create an immutable map

```
map 
 <!--- #tr -->list (array)<!--- #end tr -->
```


Create a new immutable key/value map from args. 

Introduced in v2.11

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(map foo: 1, bar: 2)[:foo]



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
(map foo: 1, bar: 2)[:bar]



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
(map foo: 1, bar: 2)[:quux]



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> nil



```
<!--- #end tr -->

</td>
</tr>
</table>

