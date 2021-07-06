# Get current (logically quantized) time

```
current_time 
 <!--- #tr --><!--- #end tr -->
```


Returns the current logical time. This is a 'wall-clock' time which should typically be pretty similar to Time.now but quantised to a nearby sleep point in the thread. May be quite different to Time.now within a time_warp!

Unlike `Time.now`, Multiple calls to `current_time` with no interleaved calls to `sleep` or `sync` will return the same value.

Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts current_time



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# 2017-03-19 23:37:57 +0000



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




puts "A", Time.now.to_f
puts "B", __system_thread_locals.get(:sonic_pi_spider_time).to_f
puts "C", Time.now.to_f
puts "D", __system_thread_locals.get(:sonic_pi_spider_time).to_f
puts "E", __system_thread_locals.get(:sonic_pi_spider_time).to_f



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# The difference between current_time and Time.now
# See that Time.now is continuous and current_time is discrete
#
# {run: 19, time: 0.0}
# ├─ "A" 1489966042.761211
# ├─ "B" 1489966042.760181
# ├─ "C" 1489966042.761235
# ├─ "D" 1489966042.760181
# └─ "E" 1489966042.760181



```
<!--- #end tr -->

</td>
</tr>
</table>

