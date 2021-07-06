# Set new tempo as a multiple of current tempo for block

```
with_bpm_mul 
 <!--- #tr -->mul (number)<!--- #end tr -->
```


Sets the tempo in bpm (beats per minute) for everything in the given block as a multiplication of the current tempo. Affects all containing calls to `sleep` and all temporal synth arguments which will be scaled to match the new bpm. See also `with_bpm`

Introduced in v2.3

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_bpm 60  
  play 50
  sleep 1     
  play 62
  sleep 2     
  with_bpm_mul 0.5 do
    play 50
    sleep 1          
    play 62
  end
  sleep 1           



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Set the BPM to 60
 
# Sleeps for 1 second
 
# Sleeps for 2 seconds
# BPM is now (60 * 0.5) == 30
 
# Sleeps for 2 seconds
 
 
# BPM is now back to 60, therefore sleep is 1 second



```
<!--- #end tr -->

</td>
</tr>
</table>

