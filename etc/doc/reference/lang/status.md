# Get server status

```
status 
 <!--- #tr --><!--- #end tr -->
```


This returns a Hash of information about the synthesis environment. Mostly used for debugging purposes.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts status
           
           
           
           
           
           
           
           
           
           
           
           



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Returns something similar to:
# {
#   :ugens=>10,
#   :synths=>1,
#   :groups=>7,
#   :sdefs=>61,
#   :avg_cpu=>0.20156468451023102,
#   :peak_cpu=>0.36655542254447937,
#   :nom_samp_rate=>44100.0,
#   :act_samp_rate=>44099.9998411752,
#   :audio_busses=>2,
#   :control_busses=>0
# }



```
<!--- #end tr -->

</td>
</tr>
</table>

