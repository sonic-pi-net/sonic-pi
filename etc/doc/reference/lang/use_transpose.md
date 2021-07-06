# Note transposition

```
use_transpose 
 <!--- #tr -->note_shift (number)<!--- #end tr -->
```


Transposes your music by shifting all notes played by the specified amount. To shift up by a semitone use a transpose of 1. To shift down use negative numbers. See `with_transpose` for setting the transpose value only for a specific `do`/`end` block. To transpose entire octaves see `use_octave`.

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
use_transpose 1
play 50



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Plays note 50
 
# Plays note 51



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
use_transpose -12
play 62
use_transpose 3
play 62



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# You may change the transposition multiple times:
# Plays note 62
 
# Plays note 50
 
# Plays note 65



```
<!--- #end tr -->

</td>
</tr>
</table>

