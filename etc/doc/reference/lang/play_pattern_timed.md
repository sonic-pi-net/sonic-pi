# Play pattern of notes with specific times

```
play_pattern_timed 
 <!--- #tr -->notes (list), times (list_or_number)<!--- #end tr -->
```


Play each note in a list of notes one after another with specified times between them. The notes should be a list of MIDI numbers, symbols such as :E4 or chords such as chord(:A3, :major) - identical to the first parameter of the play function. The times should be a list of times between the notes in beats.

If the list of times is smaller than the number of gaps between notes, the list is repeated again. If the list of times is longer than the number of gaps between notes, then some of the times are ignored. See examples for more detail.

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See `use_synth` and `with_synth` for changing the current synth.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play_pattern_timed [40, 42, 44, 46], [1, 2, 3]



play 40
sleep 1
play 42
sleep 2
play 44
sleep 3
play 46


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
# same as:
 
 
 
 
 
 
 
 



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
play_pattern_timed [40, 42, 44, 46, 49], [1, 0.5]



play 40
sleep 1
play 42
sleep 0.5
play 44
sleep 1
play 46
sleep 0.5
play 49


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
# same as:
 
 
 
 
 
 
 
 
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 3 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play_pattern_timed [40, 42, 44, 46], [0.5]



play 40
sleep 0.5
play 42
sleep 0.5
play 44
sleep 0.5
play 46


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
# same as:
 
 
 
 
 
 
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 4 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
play_pattern_timed [40, 42, 44], [1, 2, 3, 4, 5]



play 40
sleep 1
play 42
sleep 2
play 44


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
#same as:
 
 
 
 
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

