# Get note info

```
note_info 
 <!--- #tr -->note (symbol_or_number)<!--- #end tr -->
```


Returns an instance of `SonicPi::Note`. Please note - `octave:` param overrides any octave specified in a symbol i.e. `:c3`

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts note_info(:C, octave: 2)




```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# returns #<SonicPi::Note :C2>



```
<!--- #end tr -->

</td>
</tr>
</table>

