# Get current beat

```
beat 
 <!--- #tr --><!--- #end tr -->
```


Returns the beat value for the current thread/live_loop. Beats are advanced only by calls to `sleep` and `sync`. Beats are distinct from virtual time (the value obtained by calling `vt`) in that it has no notion of rate. It is just essentially a counter for sleeps. After a `sync`, the beat is overridden with the beat value from the thread which called `cue`. 

Introduced in v2.10

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_bpm 120 
  puts beat   
  sleep 1
  puts beat   
  use_bpm 2000
  sleep 2
  puts beat   



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# The current BPM makes no difference
#=> 0
 
#=> 1
 
 
#=> 3



```
<!--- #end tr -->

</td>
</tr>
</table>

