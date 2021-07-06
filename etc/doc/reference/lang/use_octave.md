# Note octave transposition

```
use_octave 
 <!--- #tr -->octave_shift (number)<!--- #end tr -->
```


Transposes your music by shifting all notes played by the specified number of octaves. To shift up by an octave use a transpose of 1. To shift down use negative numbers. See `with_octave` for setting the octave shift only for a specific `do`/`end` block. For transposing the notes within the octave range see `use_transpose`.

Introduced in v2.9

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play 50
use_octave 1
play 50



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Plays note 50
 
# Plays note 62



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

play 62
use_octave -1
play 62
use_octave 2
play 62



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# You may change the transposition multiple times:
# Plays note 62
 
# Plays note 50
 
# Plays note 86



```
<!--- #end tr -->

</td>
</tr>
</table>

