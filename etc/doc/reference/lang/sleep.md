# Wait for beat duration

```
sleep 
 <!--- #tr -->beats (number)<!--- #end tr -->
```


Wait for a number of beats before triggering the next command. Beats are converted to seconds by scaling to the current bpm setting.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby


  play 50 
  play 55
  play 62

  sleep 1 

  play 50 
  sleep 0.5
  play 55
  sleep 0.5
  play 62


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Without calls to sleep, all sounds would happen at once:
 
# This is actually a chord with all notes played simultaneously
 
 
 
# Create a gap, to allow a moment's pause for reflection...
 
# Let's try the chord again, but this time with sleeps:
# With the sleeps, we turn a chord into an arpeggio
 
 
 



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


  use_bpm 120
  play 50
  sleep 1
  play 55
  sleep 1
  play 62

 

  use_bpm 30
  play 50
  sleep 1
  play 55
  sleep 1
  play 62


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# The amount of time sleep pauses for is scaled to match the current bpm. The default bpm is 60. Let's double it:
 
 
 
# This actually sleeps for 0.5 seconds as we're now at double speed
 
 
 
 
# Let's go down to half speed:
 
 
 
# This now sleeps for 2 seconds as we're now at half speed.
 
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

