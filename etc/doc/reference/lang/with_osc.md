# Block-level setting for the default hostname and port number of outgoing OSC messages.

```
with_osc 
 <!--- #tr -->hostname (string), port (number)<!--- #end tr -->
```


Sets the destination host and port that `osc` will send messages to for the given do/end block.

Introduced in v3.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_osc "localhost", 7000 
osc "/foo/baz"            

with_osc "localhost", 7010 do
                               
   osc "/foo/baz"            
end

osc "/foo/baz"            
                            
                            



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Specify port 7010
# Send an OSC message to port 7000
 
# set hostname and port for the duration
# of this do/end block
# Send an OSC message to port 7010
 
 
# Send an OSC message to port 7000
# as old setting is restored outside
# do/end block



```
<!--- #end tr -->

</td>
</tr>
</table>

