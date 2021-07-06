# Block-level enable and disable BPM scaling

```
with_arg_bpm_scaling 
 <!--- #tr --><!--- #end tr -->
```


Turn synth argument bpm scaling on or off for the supplied block. Note, using `rt` for args will result in incorrect times when used within this block.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_bpm 120
play 50, release: 2
with_arg_bpm_scaling false do
  play 50, release: 2
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# release is actually 1 due to bpm scaling
 
# release is now 2
 



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
play 50, release: rt(2)  
sleep rt(2)              
with_arg_bpm_scaling false do
  play 50, release: rt(2)
  sleep rt(2)            
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Interaction with rt
 
# release is 2 seconds
# sleeps for 2 seconds
 
# ** Warning: release is NOT 2 seconds! **
# still sleeps for 2 seconds
 



```
<!--- #end tr -->

</td>
</tr>
</table>

