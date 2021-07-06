# Real time conversion

```
rt 
 <!--- #tr -->seconds (number)<!--- #end tr -->
```


Real time representation. Returns the amount of beats for the value in real-time seconds. Useful for bypassing any bpm scaling

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_bpm 120 
  play 50
  sleep 1     
  play 62
  sleep rt(1) 
  play 72


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# modifies all time to be half
 
# actually sleeps for half of a second
 
# bypasses bpm scaling and sleeps for a second
 



```
<!--- #end tr -->

</td>
</tr>
</table>

