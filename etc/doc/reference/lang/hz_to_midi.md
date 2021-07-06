# Hz to MIDI conversion

```
hz_to_midi 
 <!--- #tr -->freq (number)<!--- #end tr -->
```


Convert a frequency in hz to a midi note. Note that the result isn't an integer and there is a potential for some very minor rounding errors.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
hz_to_midi(261.63)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> 60.0003



```
<!--- #end tr -->

</td>
</tr>
</table>

