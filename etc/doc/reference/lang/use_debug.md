# Enable and disable debug

```
use_debug 
 <!--- #tr -->true_or_false (boolean)<!--- #end tr -->
```


Enable or disable messages created on synth triggers. If this is set to false, the synths will be silent until debug is turned back on. Silencing debug messages can reduce output noise and also increase performance on slower platforms. See `with_debug` for setting the debug value only for a specific `do`/`end` block.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_debug true



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Turn on debug messages



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
use_debug false



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Disable debug messages



```
<!--- #end tr -->

</td>
</tr>
</table>

