# Send an OSC message (Open Sound Control)

```
osc 
 <!--- #tr -->path (arguments)<!--- #end tr -->
```


Sends an OSC message to the current host and port specified by `use_osc` or `with_osc`.

OSC (Open Sound Control) is a simple way of passing messages between two separate programs on the same computer or even on different computers via a local network or even the internet. `osc` enables you to send well-timed OSC messages from within Sonic Pi. `osc` will ensure that the OSC message is sent at the correct time using the same timing system shared with the synthesis functionality via `sample`, `synth` and friends. `osc` even works seamlessly within `time_warp` - see examples.

A typical OSC message has two parts: a descriptive `path` which looks simalar to a URL (website address), and an optional list of `arguments` that are either numbers or strings.

For example, a hypothetical synth program might accept this OSC message:

`/set/filter lowpass 80 0.5`

where `/set/filter` is the path, and `lowpass`, `80`, and `0.5` are three
arguments. This can be sent from within Sonic Pi by writing:

`osc "/set/filter", "lowpass", 80, 0.5`

However, in order to send the OSC message you must first specify where to send it to. This is achieved by specifying both the host (the machine's internet address) and the port that the remote OSC server is listening on. This is configured using `use_osc` or `with_osc`. So, if our synth program was running on a machine on the local network with IP address `10.0.1.5` on port `5100` we could send our OSC message to it with the following:


`use_osc "10.0.1.5", 5100`

`osc "/set/filter", "lowpass", 80, 0.5`


Note, by default, Sonic Pi listens for OSC messages on port `4560`, so you may send messages to an external machine running Sonic Pi if you know the IP address of that external machine. Any OSC messages received on port `4559` are automatically converted to standard cue events and displayed in the GUI's cue log. This also means that you can use `sync` to wait for the next incoming OSC message with a given path (see example).

Finally, it is also very useful to send OSC messages to aother programs on the same computer. This can be achieved by specifying "localhost" as the hostname and the port as normal (depending on which port the other program is listening on).

See `osc_send` for a version which allows you to specify the hostname and port directly (ignoring any values set via `use_osc` or `with_osc`).

For further information see the OSC spec: [http://opensoundcontrol.org/spec-1_0](http://opensoundcontrol.org/spec-1_0)


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby


use_osc "localhost", 7000 
osc "/foo/bar"            
                            



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Send a simple OSC message to another program on the same machine
 
# Specify port 7000 on this machine
# Send an OSC message with path "/foo/bar"
# and no arguments



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


use_osc "localhost", 7000       
osc "/foo/bar", 1, 3.89, "baz"
                                  
                                  
                                  
                                  



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Send an OSC messages with arguments to another program on the same machine
 
# Specify port 7000 on this machine
# Send an OSC message with path "/foo/bar"
# and three arguments:
# 1) The whole number (integer) 1
# 2) The fractional number (float) 3.89
# 3) The string "baz"



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


use_osc "10.0.1.5", 7000        
osc "/foo/bar", 1, 3.89, "baz"
                                  
                                  
                                  
                                  



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Send an OSC messages with arguments to another program on a different machine
 
# Specify port 7000 on the machine with address 10.0.1.5
# Send an OSC message with path "/foo/bar"
# and three arguments:
# 1) The whole number (integer) 1
# 2) The fractional number (float) 3.89
# 3) The string "baz"



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 4 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby


osc "/foo/bar"      
play 60               

sleep 1               

osc "/baz/quux"      
play 72                



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# OSC messages honour the timing system
 
# Send an OSC message with path /foo/bar at *exactly* the
# same time as note 60 is played
 
# Wait for 1 beat
 
# Send an OSC message with path /baz/quux at *exactly* the
# same time as note 72 is played



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 5 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby


live_loop :foo do            
  osc "/counter", tick     
                             
                             
  sleep 1                    
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Send a incrementing OSC counter
 
# Start a live loop called :foo
# Send an OSC message with the path /counter
# with successive whole numbers (0, 1, 2, 3.. etc.)
# each time round the live loop
# Repeat the live loop every 1 beat
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 6 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby


time_warp 0.5 do
  osc "/foo/bar"      
end

sleep 1                 

time_warp -0.1 do
  osc "/baz/quux"     
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# OSC messages can be sent from within time_warp
 
 
# Send an OSC message with path /foo/bar at 0.5 beats
 
 
# Wait for 1 beat
 
 
# Send an OSC message with path /baz/quux at 0.9 beats
 



```
<!--- #end tr -->

</td>
</tr>
</table>

