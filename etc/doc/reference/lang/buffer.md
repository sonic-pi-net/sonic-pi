# Initialise or return named buffer

```
buffer 
 <!--- #tr -->symbol (name), number (duration)<!--- #end tr -->
```


Initialise or return a named buffer with a specific duration (defaults to 8 beats). Useful for working with the `:record` FX. If the buffer is requested with a different duration, then a new buffer will be initialised and the old one recycled.

Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
buffer(:foo)
b = buffer(:foo)
puts b.duration 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# load a 8s buffer and name it :foo
# return cached buffer and bind it to b
#=> 8.0



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
buffer(:foo, 16)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# load a 16s buffer and name it :foo



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
use_bpm 120
buffer(:foo, 16)
                
                



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# load a 8s buffer and name it :foo
# (this isn't 16s as the BPM has been
# doubled from the default of 60)



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
buffer(:foo)    
buffer(:foo, 8) 
buffer(:foo, 10)
buffer(:foo, 10)
buffer(:foo)    
buffer(:foo)    



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# init a 8s buffer and name it :foo
# return cached 8s buffer (has the same duration)
# init a new 10s buffer and name it :foo
# return cached 10s buffer
# init a 8s buffer and name it :foo
# return cached 8s buffer (has the same duration)



```
<!--- #end tr -->

</td>
</tr>
</table>

