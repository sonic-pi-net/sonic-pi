# Set the default hostname and port number for outgoing OSC messages.

```
use_osc 
 <!--- #tr -->hostname (string), port (number)<!--- #end tr -->
```


Sets the destination host and port that `osc` will send messages to. If no port number is specified - will default to port 4560 (Sonic Pi's default OSC listening port).

OSC (Open Sound Control) is a simple way of passing messages between two separate programs on the same computer or even on different computers via a local network or even the internet. `use_osc` allows you to specify which computer (`hostname`) and program (`port`) to send messages to.

It is possible to send messages to the same computer by using the host name `"localhost"`

This is a thread-local setting - therefore each thread (or live loop) can have their own separate `use_osc` values.

Note that calls to `osc_send` will ignore these values.



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
osc "/foo/bar" 1, 3.89, "baz" 
                                  
                                  
                                  
                                  



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
# 2) The fractional number (float) 3,89
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
osc "/foo/bar" 1, 3.89, "baz" 
                                  
                                  
                                  
                                  



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
# 2) The fractional number (float) 3,89
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


use_osc "localhost", 7000 
osc "/foo/bar"            
osc "/foo/baz"            

use_osc "localhost", 7005 
osc "/foo/bar"            
osc "/foo/baz"            



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# use_osc only affects calls to osc until the next call to use_osc
 
# Specify port 7000 on this machine
# Send an OSC message to port 7000
# Send another OSC message to port 7000
 
# Specify port 7000 on this machine
# Send an OSC message to port 7005
# Send another OSC message to port 7005



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


use_osc "localhost", 7000 

live_loop :foo do
  osc "/foo/bar"            
  sleep 1                     
end

live_loop :bar do
  use_osc "localhost", 7005 
                              
                              

  osc "/foo/bar"            
  sleep 1
end

use_osc "localhost", 7010 
osc "/foo/baz"            
                            
                            
                            



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# threads may have their own use_osc value
 
# Specify port 7000 on this machine
 
 
# Thread inherits outside use_osc values
# and therefore sends OSC messages to port 7000
 
 
 
# Override OSC hostname and port for just this
# thread (live loop :bar). Live loop :foo is
# unaffected.
 
# Send OSC messages to port 7005
 
 
 
# Specify port 7010
# Send another OSC message to port 7010
# Note that neither live loops :foo or :bar
# are affected (their use_osc values are
# independent and isolated.



```
<!--- #end tr -->

</td>
</tr>
</table>

