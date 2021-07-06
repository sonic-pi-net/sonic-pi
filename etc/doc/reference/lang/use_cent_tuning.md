# Cent tuning

```
use_cent_tuning 
 <!--- #tr -->cent_shift (number)<!--- #end tr -->
```


Uniformly tunes your music by shifting all notes played by the specified number of cents. To shift up by a cent use a cent tuning of 1. To shift down use negative numbers. One semitone consists of 100 cents.

See `with_cent_tuning` for setting the cent tuning value only for a specific `do`/`end` block. To transpose entire semitones see `use_transpose`.

Introduced in v2.9

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play 50
use_cent_tuning 1
play 50



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Plays note 50
 
# Plays note 50.01



```
<!--- #end tr -->

</td>
</tr>
</table>

