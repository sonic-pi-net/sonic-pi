# Describe note

```
note 
 <!--- #tr -->note (symbol_or_number)<!--- #end tr -->
```


Takes a midi note, a symbol (e.g. `:C`) or a string (e.g. `"C"`) and resolves it to a midi note. You can also pass an optional `octave:` parameter to get the midi note for a given octave. Please note - `octave:` param overrides any octave specified in a symbol i.e. `:c3`. If the note is `nil`, `:r` or `:rest`, then `nil` is returned (`nil` represents a rest)

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

puts note(60)
puts note(:C)
puts note(:C4)
puts note('C')


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# These all return 60 which is the midi number for middle C (octave 4)
 
 
 
 



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

puts note(60, octave: 2)


puts note(:C, octave: 2)
puts note(:C4, octave: 2)
puts note('C', octave: 2)


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# returns 60 - octave param has no effect if we pass in a number
 
 
# These all return 36 which is the midi number for C2 (two octaves below middle C)
 
# note the octave param overrides any octaves specified in a symbol
 



```
<!--- #end tr -->

</td>
</tr>
</table>

