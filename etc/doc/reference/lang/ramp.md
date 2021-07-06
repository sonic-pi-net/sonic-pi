# Create a ramp vector

```
ramp 
 <!--- #tr -->list (array)<!--- #end tr -->
```


Create a new immutable ramp vector from args. Indexes always return first or last value if out of bounds.

Introduced in v2.6

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(ramp 1, 2, 3)[0]



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
(ramp 1, 2, 3)[1]



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
(ramp 1, 2, 3)[2]



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> 3



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
(ramp 1, 2, 3)[3]



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
<tr>
<th colspan="2" class="even head"># Example 5 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(ramp 1, 2, 3)[1000]



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> 3



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
(ramp 1, 2, 3)[-1]



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> 1



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 7 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(ramp 1, 2, 3)[-1000]



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
</table>

