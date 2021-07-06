# Random number in centred distribution

```
rdist 
 <!--- #tr -->width (number), centre (number)<!--- #end tr -->
```


Returns a random number within the range with width around centre. If optional arg `step:` is used, the result is quantised by step.

Introduced in v2.3

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
print rdist(1, 0)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> will print a number between -1 and 1



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
print rdist(1)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> centre defaults to 0 so this is the same as rdist(1, 0)



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
loop do
    play :c3, pan: rdist(1)
    sleep 0.125
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
#=> Will play :c3 with random L/R panning
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

