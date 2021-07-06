# Switch current synth

```
use_synth 
 <!--- #tr -->synth_name (symbol)<!--- #end tr -->
```


Switch the current synth to `synth_name`. Affects all further calls to `play`. See `with_synth` for changing the current synth only for a specific `do`/`end` block.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play 50
use_synth :mod_sine
play 50



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Plays with default synth
 
# Plays with mod_sine synth



```
<!--- #end tr -->

</td>
</tr>
</table>

