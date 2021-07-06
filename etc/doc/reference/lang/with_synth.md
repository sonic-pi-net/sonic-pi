# Block-level synth switching

```
with_synth 
 <!--- #tr -->synth_name (symbol)<!--- #end tr -->
```


Switch the current synth to `synth_name` but only for the duration of the `do`/`end` block. After the `do`/`end` block has completed, the previous synth is restored.

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
sleep 2
use_synth :supersaw
play 50
sleep 2
with_synth :saw_beep do
  play 50
end
sleep 2

play 50



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Plays with default synth
 
 
# Plays with supersaw synth
 
 
# Plays with saw_beep synth
 
 
# Previous synth is restored
# Plays with supersaw synth



```
<!--- #end tr -->

</td>
</tr>
</table>

