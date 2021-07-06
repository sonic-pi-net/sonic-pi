# Play notes simultaneously

```
play_chord 
 <!--- #tr -->notes (list)<!--- #end tr -->
```


Play a list of notes at the same time.

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See `use_synth` and `with_synth` for changing the current synth.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play_chord [40, 45, 47]



play 40
play 45
play 47


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
# same as:
 
 
 
 



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
play_chord [40, 45, 47], amp: 0.5



play 40, amp: 0.5
play 45, amp: 0.5
play 47, amp: 0.5


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
# same as:
 
 
 
 



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
play_chord chord(:e3, :minor)


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 



```
<!--- #end tr -->

</td>
</tr>
</table>

