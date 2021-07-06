# Set sched ahead time globally

```
set_sched_ahead_time! 
 <!--- #tr -->time (number)<!--- #end tr -->
```


Specify how many seconds ahead of time the synths should be triggered. This represents the amount of time between pressing 'Run' and hearing audio. A larger time gives the system more room to work with and can reduce performance issues in playing fast sections on slower platforms. However, a larger time also increases latency between modifying code and hearing the result whilst live coding.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
set_sched_ahead_time! 1



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
</table>

