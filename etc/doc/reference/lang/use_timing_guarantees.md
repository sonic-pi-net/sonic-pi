# Inhibit synth triggers if too late

```
use_timing_guarantees 
 <!--- #tr -->bool (true_or_false)<!--- #end tr -->
```


If set to true, synths will not trigger if it is too late. If false, some synth triggers may be late.

Introduced in v2.10

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_timing_guarantees true

sample :loop_amen 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
#=> if time is behind by any margin, this will not trigger



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
use_timing_guarantees false

sample :loop_amen 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
#=> unless time is too far behind, this will trigger even when late.



```
<!--- #end tr -->

</td>
</tr>
</table>

