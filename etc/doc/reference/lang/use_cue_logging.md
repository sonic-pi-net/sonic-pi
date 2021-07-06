# Enable and disable cue logging

```
use_cue_logging 
 <!--- #tr -->true_or_false (boolean)<!--- #end tr -->
```


Enable or disable log messages created on cues. This does not disable the cues themselves, it just stops them from being printed to the log

Introduced in v2.6

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_cue_logging true



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Turn on cue messages



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
use_cue_logging false



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Disable cue messages



```
<!--- #end tr -->

</td>
</tr>
</table>

