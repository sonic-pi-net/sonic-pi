# Duration of current beat

```
current_beat_duration 
 <!--- #tr --><!--- #end tr -->
```


Get the duration of the current beat in seconds. This is the actual length of time which will elapse with `sleep 1`.

Affected by calls to `use_bpm`, `with_bpm`, `use_sample_bpm` and `with_sample_bpm`.

Introduced in v2.6

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_bpm 60
  puts current_beat_duration

  use_bpm 120
  puts current_beat_duration



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
#=> 1
 
 
#=> 0.5



```
<!--- #end tr -->

</td>
</tr>
</table>

