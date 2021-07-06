# Block-level use new synth defaults

```
with_synth_defaults 
 <!--- #tr --><!--- #end tr -->
```


Specify new default values to be used by all calls to `play` within the `do`/`end` block. After the `do`/`end` block has completed the previous synth defaults (if any) are restored.

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

use_synth_defaults amp: 0.5, pan: -1

play 50

with_synth_defaults amp: 0.6, cutoff: 80 do
  play 50
end

play 60



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# plays note 50 with default arguments
 
 
 
# plays note 50 with an amp of 0.5, pan of -1 and defaults for rest of args
 
 
# plays note 50 with an amp of 0.6, cutoff of 80 and defaults for rest of args (including pan)
 
 
# plays note 60 with an amp of 0.5, pan of -1 and defaults for rest of args



```
<!--- #end tr -->

</td>
</tr>
</table>

