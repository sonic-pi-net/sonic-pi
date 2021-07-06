# Beat time conversion

```
bt 
 <!--- #tr -->seconds (number)<!--- #end tr -->
```


Beat time representation. Scales the time to the current BPM. Useful for adding bpm scaling

Introduced in v2.8

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_bpm 120 
  puts bt(1)
  use_bpm 60  
  puts bt(1)
  use_bpm 30  
  puts bt(1)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Set the BPM to be double the default
# 0.5
# BPM is now default
# 1
# BPM is now half the default
# 2



```
<!--- #end tr -->

</td>
</tr>
</table>

