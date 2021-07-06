# Use alternative tuning systems

```
use_tuning 
 <!--- #tr -->tuning (symbol), fundamental_note (symbol_or_number)<!--- #end tr -->
```


In most music we make semitones by dividing the octave into 12 equal parts, which is known as equal temperament. However there are lots of other ways to tune the 12 notes. This method adjusts each midi note into the specified tuning system. Because the ratios between notes aren't always equal, be careful to pick a centre note that is in the key of the music you're making for the best sound. Currently available tunings are `:just`, `:pythagorean`, `:meantone` and the default of `:equal`

Introduced in v2.6

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play :e4
use_tuning :just, :c
play :e4

play 64



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Plays note 64
 
# Plays note 63.8631
# transparently changes midi notes too
# Plays note 63.8631



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

play 64
use_tuning :just
play 64
use_tuning :equal
play 64



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# You may change the tuning multiple times:
# Plays note 64
 
# Plays note 63.8631
 
# Plays note 64



```
<!--- #end tr -->

</td>
</tr>
</table>

