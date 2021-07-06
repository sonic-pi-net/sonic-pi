# Chord inversion

```
chord_invert 
 <!--- #tr -->notes (list), shift (number)<!--- #end tr -->
```


Given a set of notes, apply a number of inversions indicated by the `shift` parameter. Inversions being an increase to notes if `shift` is positive or decreasing the notes if `shift` is negative.

An inversion is simply rotating the chord and shifting the wrapped notes up or down an octave. For example, consider the chord :e3, :minor - `(ring 52, 55, 59)`. When we invert it once, we rotate the notes around to `(ring 55, 59, 52)`. However, because note 52 is wrapped round, it's shifted up an octave (12 semitones) so the actual first inversion of the chord :e3, :minor is `(ring 55, 59, 52 + 12)` or `(ring 55, 59, 64)`.

Note that it's also possible to directly invert chords on creation with the `invert:` opt - `(chord :e3, :minor, invert: 2)`

Introduced in v2.6

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play (chord_invert (chord :A3, "M"), 0)
sleep 1
play (chord_invert (chord :A3, "M"), 1)
sleep 1
play (chord_invert (chord :A3, "M"), 2)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#No inversion     - (ring 57, 61, 64)
 
#First inversion  - (ring 61, 64, 69)
 
#Second inversion - (ring 64, 69, 73)



```
<!--- #end tr -->

</td>
</tr>
</table>

