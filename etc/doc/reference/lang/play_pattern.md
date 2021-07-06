# Play pattern of notes

```
play_pattern 
 <!--- #tr -->notes (list)<!--- #end tr -->
```


Play list of notes with the current synth one after another with a sleep of 1

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See use_synth and with_synth for changing the current synth.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play_pattern [40, 41, 42]
                         
                         
                         
                         
                         



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Same as:
#   play 40
#   sleep 1
#   play 41
#   sleep 1
#   play 42



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
play_pattern [:d3, :c1, :Eb5]



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# You can use keyword notes



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
play_pattern [:d3, :c1, :Eb5], amp: 0.5, cutoff: 90



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Supports the same arguments as play:



```
<!--- #end tr -->

</td>
</tr>
</table>

