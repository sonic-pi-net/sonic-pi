# Reset main mixer

```
reset_mixer! 
 <!--- #tr --><!--- #end tr -->
```


The main mixer is the final mixer that all sound passes through. This fn resets it to its default set - undoing any changes made via set_mixer_control!

Introduced in v2.9

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
set_mixer_control! lpf: 70
sample :loop_amen         
sleep 3
reset_mixer!              
sample :loop_amen         



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# LPF cutoff value of main mixer is now 70
# :loop_amen sample is played with low cutoff
 
# mixer is now reset to default values
# :loop_amen sample is played with normal cutoff



```
<!--- #end tr -->

</td>
</tr>
</table>

