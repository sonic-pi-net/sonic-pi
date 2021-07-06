# Construct chords of stacked thirds, based on scale degrees

```
chord_degree 
 <!--- #tr -->degree (symbol_or_number), tonic (symbol), scale (symbol), number_of_notes (number)<!--- #end tr -->
```


In music we build chords from scales. For example, a C major chord is made by taking the 1st, 3rd and 5th notes of the C major scale (C, E and G). If you do this on a piano you might notice that you play one, skip one, play one, skip one etc. If we use the same spacing and start from the second note in C major (which is a D), we get a D minor chord which is the 2nd, 4th and 6th notes in C major (D, F and A). We can move this pattern all the way up or down the scale to get different types of chords. `chord_degree` is a helper method that returns a ring of midi note numbers when given a degree (starting point in a scale) which is a symbol `:i`, `:ii`, `:iii`, `:iv`, `:v`, `:vi`, `:vii` or a number `1`-`7`. The second argument is the tonic note of the scale, the third argument is the scale type and finally the fourth argument is number of notes to stack up in the chord. If we choose 4 notes from degree `:i` of the C major scale, we take the 1st, 3rd, 5th and 7th notes of the scale to get a C major 7 chord.

Introduced in v2.1

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts (chord_degree :i, :A3, :major)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# returns a ring of midi notes - (ring 57, 61, 64, 68) - an A major 7 chord



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
play (chord_degree :i, :A3, :major, 3)


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 



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
play (chord_degree :ii, :A3, :major, 3)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Chord ii in A major is a B minor chord



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
play (chord_degree :iii, :A3, :major, 3)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Chord iii in A major is a C# minor chord



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 5 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play (chord_degree :iv, :A3, :major, 3)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Chord iv in A major is a D major chord



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 6 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
play (chord_degree :i, :C4, :major, 4)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Taking four notes is the default. This gives us 7th chords - here it plays a C major 7



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 7 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play (chord_degree :i, :C4, :major, 5)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Taking five notes gives us 9th chords - here it plays a C major 9 chord



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 8 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
play (chord_degree :i, :C4, :major, 3, invert: 1)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Play the first inversion of chord i in C major - (ring 64, 67, 72)



```
<!--- #end tr -->

</td>
</tr>
</table>

