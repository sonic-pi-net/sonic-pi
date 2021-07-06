# Enable and disable OSC logging

```
use_osc_logging 
 <!--- #tr -->true_or_false (boolean)<!--- #end tr -->
```


Enable or disable log messages created on OSC functions. This does not disable the OSC functions themselves, it just stops them from being printed to the log

Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_osc_logging true



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Turn on OSC logging



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
use_osc_logging false



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Disable OSC logging



```
<!--- #end tr -->

</td>
</tr>
</table>

