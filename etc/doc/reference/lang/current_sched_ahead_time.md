# Get current sched ahead time

```
current_sched_ahead_time 
 <!--- #tr --><!--- #end tr -->
```


Returns the current schedule ahead time.

This can be set via the fn `set_sched_ahead_time!`.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
set_sched_ahead_time! 0.5
puts current_sched_ahead_time



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# Prints 0.5



```
<!--- #end tr -->

</td>
</tr>
</table>

