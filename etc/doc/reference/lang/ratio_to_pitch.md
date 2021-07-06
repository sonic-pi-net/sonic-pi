# relative frequency ratio to MIDI pitch

```
ratio_to_pitch 
 <!--- #tr -->ratio (number)<!--- #end tr -->
```


Convert a frequency ratio to a midi note which when added to a note will transpose the note to match the frequency ratio.

Introduced in v2.7

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
ratio_to_pitch 2



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> 12.0



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
ratio_to_pitch 0.5



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> -12.0



```
<!--- #end tr -->

</td>
</tr>
</table>

