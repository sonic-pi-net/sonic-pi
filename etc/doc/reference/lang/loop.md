# Repeat do/end block forever

```
loop 
 <!--- #tr --><!--- #end tr -->
```


Given a do/end block, repeats it forever. Note that once the program enters the loop - it will not move on but will instead stay within the loop. Plain loops like this are like black holes - instead of sucking in the light they suck in the program.

The loop must either `sleep` or `sync` each time round otherwise it will stop and throw an error. This is to stop the loop from spinning out of control and locking the system.

For a more powerful, flexible loop built for live coding see `live_loop`.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play 70      

loop do
  play 50    
  sleep 1
  play 62
  sleep 2
end

play 80     



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# note 70 is played
 
 
# This loop will repeat notes 50 and 62 forever
 
 
 
 
 
# This is *never* played as the program is trapped in the loop above



```
<!--- #end tr -->

</td>
</tr>
</table>

