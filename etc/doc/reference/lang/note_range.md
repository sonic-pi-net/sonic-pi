# Get a range of notes

```
note_range 
 <!--- #tr -->low_note (note), high_note (note)<!--- #end tr -->
```


Produces a ring of all the notes between a low note and a high note. By default this is chromatic (all the notes) but can be filtered with a pitches: argument. This opens the door to arpeggiator style sequences and other useful patterns. If you try to specify only pitches which aren't in the range it will raise an error - you have been warned!

Introduced in v2.6

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(note_range :c4, :c5)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# => (ring 60,61,62,63,64,65,66,67,68,69,70,71,72)



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
(note_range :c4, :c5, pitches: (chord :c, :major))



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# => (ring 60,64,67,72)



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
(note_range :c4, :c6, pitches: (chord :c, :major))



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# => (ring 60,64,67,72,76,79,84)



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
(note_range :c4, :c5, pitches: (scale :c, :major))



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# => (ring 60,62,64,65,67,69,71,72)



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
(note_range :c4, :c5, pitches: [:c4, :g2])



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# => (ring 60,67,72)



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
live_loop :arpeggiator do
 
  play (note_range :c4, :c5, pitches: (chord :c, :major)).tick
  sleep 0.125
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
# try changing the chord
 
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

