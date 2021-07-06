# Obtain value of a tick

```
look 
 <!--- #tr --><!--- #end tr -->
```


Read and return value of default tick. If a `key` is specified, read the value of that specific tick. Ticks are `in_thread` and `live_loop` local, so the tick read will be the tick of the current thread calling `look`.

Introduced in v2.6

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts look
  puts look
  puts look



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> 0
#=> 0
#=> 0 # look doesn't advance the tick, it just returns the current value



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
puts look
  tick
  puts look
  tick
  puts look
  puts look
  tick
  puts look



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> 0 # A look is always 0 before the first tick
# advance the tick
#=> 0 # Note: a look is still 0 after the first tick.
 
#=> 1
#=> 1 # making multiple calls to look doesn't affect tick value
 
#=> 2



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
tick(:foo)
  tick(:foo)
  puts look(:foo)
  puts look
  puts look(:bar)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
#=> 1 (keyed look :foo has been advanced)
#=> 0 (default look hasn't been advanced)
#=> 0 (other keyed looks haven't been advanced either)



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

  live_loop :foo do
    tick                                     
    use_synth :beep
    play (scale :e3, :minor_pentatonic).look 
    sleep 0.5
    use_synth :square
    play (ring :e1, :e2, :e3).look, release: 0.25
    sleep 0.25
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# You can call look on lists and rings
 
# advance the default tick
 
# look into the default tick to play all notes in sequence
 
 
# use the same look on another ring
 
 



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

puts look(0)    
puts look(4)    
puts look(-4)   
puts look(20.3) 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Returns numbers unchanged if single argument
#=> 0
#=> 4
#=> -4
#=> 20.3



```
<!--- #end tr -->

</td>
</tr>
</table>

