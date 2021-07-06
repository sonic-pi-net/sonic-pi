# Convert a degree into a note

```
degree 
 <!--- #tr -->degree (symbol_or_number), tonic (symbol), scale (symbol)<!--- #end tr -->
```


For a given scale and tonic it takes a symbol/string/number and resolves it to a midi note. The degree can be either a decimal number or a roman numeral (if it's a string or symbol), and may optionally be prefixed an augmentation (`a`/`d` for an augmented/diminished interval, `aa`/`dd` for double augmented/diminished or `p` for a perfect (unchanged) interval).

Introduced in v2.1

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play degree(:iii, :D3, :major)
play degree(3, :C3, :minor)
play degree('d5', :B3, :major)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# major third up from :D3
# minor third up from :C3
# diminished fifth up from :B3



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
chrd = []
[:i, :iii, :v, :dvii, :dix, :Axi, :xiii].each do |d| 
  chrd.append (degree d, :Fs, :major) 
end
play chrd 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
# for each degree in the chord
# add the corresponding note
 
# play an F# 13+11-9 chord, using roman numeral symbols



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
chrd = []
['1', '3', '5', 'd7', 'd9', 'A11', '13'].each do |d|
  chrd.append (degree d, :Fs, :major)
end
play chrd 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
 
 
# the same chord as above, but using decimal number strings



```
<!--- #end tr -->

</td>
</tr>
</table>

