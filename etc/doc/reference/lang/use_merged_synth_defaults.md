# Merge synth defaults

```
use_merged_synth_defaults 
 <!--- #tr --><!--- #end tr -->
```


Specify synth arg values to be used by any following call to play. Merges the specified values with any previous defaults, rather than replacing them.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play 50

use_merged_synth_defaults amp: 0.5
play 50

use_merged_synth_defaults cutoff: 80
play 50

use_merged_synth_defaults amp: 0.7
play 50



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Plays note 50
 
 
#=> Plays note 50 with amp 0.5
 
 
#=> Plays note 50 with amp 0.5 and cutoff 80
 
 
#=> Plays note 50 with amp 0.7 and cutoff 80



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
use_synth_defaults amp: 0.5, cutoff: 80, pan: -1
use_merged_synth_defaults amp: 0.7
play 50



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
#=> Plays note 50 with amp 0.7, cutoff 80 and pan -1



```
<!--- #end tr -->

</td>
</tr>
</table>

