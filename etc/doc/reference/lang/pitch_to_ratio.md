# relative MIDI pitch to frequency ratio

```
pitch_to_ratio 
 <!--- #tr -->pitch (midi_number)<!--- #end tr -->
```


Convert a midi note to a ratio which when applied to a frequency will scale the frequency by the number of semitones. Useful for changing the pitch of a sample by using it as a way of generating the rate.

Introduced in v2.5

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
pitch_to_ratio 12



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> 2.0



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
pitch_to_ratio 1



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> 1.05946



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
pitch_to_ratio -12



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> 0.5



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
sample :ambi_choir, rate: pitch_to_ratio(3)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Plays :ambi_choir 3 semitones above default.



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

(range 0, 16).each do |n|                 
  sample :ambi_choir, rate: pitch_to_ratio(n)
  sleep 0.5                               
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Play a chromatic scale of semitones
# For each note in the range 0->16
# play :ambi_choir at the relative pitch
# and wait between notes
 



```
<!--- #end tr -->

</td>
</tr>
</table>

