# Increment a tick and return value

```
tick 
 <!--- #tr -->key (symbol)<!--- #end tr -->
```


Increment the default tick by 1 and return value. Successive calls to `tick` will continue to increment the default tick. If a `key` is specified, increment that specific tick. If an increment `value` is specified, increment key by that value rather than 1. Ticks are `in_thread` and `live_loop` local, so incrementing a tick only affects the current thread's version of that tick. See `tick_reset` and `tick_set` for directly manipulating the tick vals.

Introduced in v2.6

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts tick
  puts tick
  puts tick
  puts tick



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> 0
#=> 1
#=> 2
#=> 3



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
puts tick(:foo)
  puts tick(:foo)
  puts tick(:foo)
  puts tick(:bar)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> 0 # named ticks have their own counts
#=> 1
#=> 2
#=> 0 # tick :bar is independent of tick :foo



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

 

  puts tick            
  puts tick            
  puts tick            
  puts tick(step: 2)   
  puts tick(step: 2)   
  puts tick(step: 10)  
  puts tick            



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# You can tick by more than increments of 1
# using the step: opt
 
#=> 0
#=> 1
#=> 2
#=> 4
#=> 6
#=> 16
#=> 17



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

  live_loop :fast_tick do
    puts tick  
    sleep 2    
  end

  live_loop :slow_tick do
    puts tick  
    sleep 4    
               
               
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Each_live loop has its own separate ticks
 
# the fast_tick live_loop's tick will
# be updated every 2 seconds
 
 
 
# the slow_tick live_loop's tick is
# totally independent from the fast_tick
# live loop and will be updated every 4
# seconds
 



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
live_loop :regular_tick do
    puts tick  
    sleep 1    
  end

  live_loop :random_reset_tick do
    if one_in 3
      tick_reset
      puts "reset tick!"
    end
    puts tick  
    sleep 1    
               
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# the regular_tick live_loop's tick will
# be updated every second
 
 
 
# randomly reset tick
 
 
 
# this live_loop's tick is totally
# independent and the reset only affects
# this tick.
 



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

 
  live_loop :scale do
    play [:c, :d, :e, :f, :g].tick  
    sleep 1
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Ticks work directly on lists, and will tick through each element
# However, once they get to the end, they'll return nil
 
# play all notes just once, then rests
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 7 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

  live_loop :odd_scale do
    tick 
    play [:c, :d, :e, :f, :g, :a].tick  
                                        
    sleep 1
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Normal ticks interact directly with list ticks
 
# Increment the default tick
# this now play every *other* note just once,
# then rests
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 8 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

 
 
 
  live_loop :looped_scale do
    play (ring :c, :d, :e, :f, :g).tick  
    sleep 1
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Ticks work wonderfully with rings
# as the ring ensures the tick wraps
# round internally always returning a
# value
 
# play all notes just once, then repeats
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 9 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

 
  live_loop :looped_scale do
    play (scale :e3, :minor_pentatonic).tick  
    sleep 0.25
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Ticks work wonderfully with scales
# which are also rings
 
# play all notes just once, then repeats
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

