# Enable and disable BPM scaling

```
use_arg_bpm_scaling 
 <!--- #tr -->bool (boolean)<!--- #end tr -->
```


Turn synth argument bpm scaling on or off for the current thread. This is on by default. Note, using `rt` for args will result in incorrect times when used after turning arg bpm scaling off.

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
sleep 2            
use_arg_bpm_scaling false
play 50, release: 2
sleep 2            



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# release is actually 1 due to bpm scaling
# actually sleeps for 1 second
 
# release is now 2
# still sleeps for 1 second



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
use_arg_bpm_scaling false
play 50, release: rt(2)
sleep rt(2)            



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

