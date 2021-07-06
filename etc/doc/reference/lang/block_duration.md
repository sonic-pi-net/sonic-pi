# Return block duration

```
block_duration 
 <!--- #tr --><!--- #end tr -->
```


Given a block, runs it and returns the amount of time that has passed. This time is in seconds and is not scaled to the current BPM. Any threads spawned in the block are not accounted for.

Introduced in v2.9

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
dur = block_duration do
  play 50
  sleep 1
  play 62
  sleep 2
end

puts dur



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
 
 
 
 
 
#=> Returns 3 as 3 seconds have passed within the block



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
use_bpm 120
dur = block_duration do
  play 50
  sleep 1
  play 62
  sleep 2
end

puts dur
        



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
 
 
 
 
 
 
#=> Returns 1.5 as 1.5 seconds have passed within the block
#   (due to the BPM being 120)



```
<!--- #end tr -->

</td>
</tr>
</table>

