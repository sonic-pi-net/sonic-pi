# Create a ring buffer of midi note numbers

```
midi_notes 
 <!--- #tr -->list (array)<!--- #end tr -->
```


Create a new immutable ring buffer of notes from args. Indexes wrap around positively and negatively. Final ring consists only of MIDI numbers and nil.

Introduced in v2.7

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(midi_notes :d3, :d4, :d5)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring 50, 62, 74)



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
(midi_notes :d3, 62,  nil)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring 50, 62, nil)



```
<!--- #end tr -->

</td>
</tr>
</table>

