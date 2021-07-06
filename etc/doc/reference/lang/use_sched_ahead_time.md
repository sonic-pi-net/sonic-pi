# Set sched ahead time for the current thread

```
use_sched_ahead_time 
 <!--- #tr -->time (number)<!--- #end tr -->
```


Specify how many seconds ahead of time the synths should be triggered. This represents the amount of time between pressing 'Run' and hearing audio. A larger time gives the system more room to work with and can reduce performance issues in playing fast sections on slower platforms. However, a larger time also increases latency between modifying code and hearing the result whilst live coding.

See `set_sched_ahead_time!` for a global version of this function. Note, `use_sched_ahead_time` will override any value set with `set_sched_ahead_time!` for the current thread.

See `use_real_time` for a simple way of setting the schedule ahead time to 0.

Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_sched_ahead_time 1



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Code will now run approximately 1 second ahead of audio.



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

live_loop :foo do
  use_sched_ahead_time 1
  play 70                
  sleep 1
end

live_loop :foo do
  use_sched_ahead_time 0.5
  play 82
  sleep 1
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Each thread can have its own sched ahead time
 
 
# Note 70 will be played with 1 second latency
 
 
 
 
# Note 70 will be played with 0.5 second latency
 
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

