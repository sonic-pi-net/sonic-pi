# Use new synth defaults

```
use_synth_defaults 
 <!--- #tr --><!--- #end tr -->
```


Specify new default values to be used by all subsequent calls to `play`. Will remove and override any previous defaults.

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

use_synth_defaults amp: 0.5, cutoff: 70

play 50

use_synth_defaults cutoff: 90

play 50



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# plays note 50 with default arguments
 
 
 
# plays note 50 with an amp of 0.5, cutoff of 70 and defaults for rest of args
 
 
 
# plays note 50 with a cutoff of 90 and defaults for rest of args - note that amp is no longer 0.5



```
<!--- #end tr -->

</td>
</tr>
</table>

