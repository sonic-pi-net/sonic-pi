# Send an OSC message to a specific host and port

```
osc_send 
 <!--- #tr -->hostname (string), port (number), path (osc_path), args (list)<!--- #end tr -->
```


Similar to `osc` except ignores any `use_osc` settings and sends the OSC message directly to the specified `hostname` and `port`.

See `osc` for more information.

Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
osc_send "localhost", 7000, "/foo/baz" 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Send an OSC message to port 7000 on the same machine



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
use_osc "localhost", 7010                
osc "/foo/baz"                           

osc_send "localhost", 7000, "/foo/baz" 
                                           



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# set hostname and port
# Send an OSC message to port 7010
 
# Send an OSC message to port 7000
# (ignores use_osc settings)



```
<!--- #end tr -->

</td>
</tr>
</table>

