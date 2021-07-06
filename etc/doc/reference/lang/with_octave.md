# Block level octave transposition

```
with_octave 
 <!--- #tr -->octave_shift (number)<!--- #end tr -->
```


Transposes your music by shifting all notes played by the specified number of octaves within the specified block. To shift up by an octave use a transpose of 1. To shift down use negative numbers. For transposing the notes within the octave range see `with_transpose`.

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
sleep 1
with_octave 1 do
 play 50
end
sleep 1
play 50



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Plays note 50
 
 
# Plays note 62
 
 
# Plays note 50



```
<!--- #end tr -->

</td>
</tr>
</table>

