# Quantise a value to resolution

```
quantise 
 <!--- #tr -->n (number), step (positive_number)<!--- #end tr -->
```


Round value to the nearest multiple of step resolution.

Introduced in v2.1

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
quantise(10, 1)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# 10 is already a multiple of 1, so returns 10



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
quantise(10, 1.1)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Returns 9.9 which is 1.1 * 9



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
quantise(13.3212, 0.1)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# 13.3



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
quantise(13.3212, 0.2)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# 13.4



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
quantise(13.3212, 0.3)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# 13.2



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 6 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
quantise(13.3212, 0.5)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# 13.5



```
<!--- #end tr -->

</td>
</tr>
</table>

