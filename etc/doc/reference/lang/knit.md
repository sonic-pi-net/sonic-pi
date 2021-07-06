# Knit a sequence of repeated values

```
knit 
 <!--- #tr -->value (anything), count (number)<!--- #end tr -->
```


Knits a series of value, count pairs to create a ring buffer where each value is repeated count times.

Introduced in v2.2

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(knit 1, 5)   



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 1, 1, 1, 1, 1)



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
(knit :e2, 2, :c2, 3)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring :e2, :e2, :c2, :c2, :c2)



```
<!--- #end tr -->

</td>
</tr>
</table>

