# Shift time forwards or backwards for the given block

```
time_warp 
 <!--- #tr -->delta_time (number)<!--- #end tr -->
```


The code within the given block is executed with the specified delta time shift specified in beats. For example, if the delta value is 0.1 then all code within the block is executed with a 0.1 beat delay. Negative values are allowed which means you can move a block of code *backwards in time*. For example a delta value of -0.1 will execute the code in the block 0.1 beats ahead of time. The time before the block started is restored after the execution of the block.

Given a list of times, run the block once after waiting each given time. If passed an optional params list, will pass each param individually to each block call. If size of params list is smaller than the times list, the param values will act as rings (rotate through). If the block is given 1 arg, the times are fed through. If the block is given 2 args, both the times and the params are fed through. A third block arg will receive the index of the time.

Note that the code within the block is executed synchronously with the code before and after, so all thread locals will be modified inline - as is the case for `with_fx`. However, as time is always restored to the value before `time_warp` started, you can use it to schedule events for the future in a similar fashion to a thread (via `at` or `in_thread`) without having to use an entirely fresh and distinct set of thread locals - see examples.

Also, note that you cannot travel backwards in time beyond the `current_sched_ahead_time`.

If the `time_warp` block is within a `density` block, the delta time is not affected (although all the other times such as sleep and phase durations will be affected) - see example.

`time_warp` is ahead-of-time scheduling within the current thread. See `at` for just-in-time scheduling using multiple isolated threads.

Introduced in v2.11

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

play 70           
sleep 1
play 75           

time_warp 0.1 do
                  
  play 80         
  sleep 0.5
  play 80         

end               

                  
                  
                  

play 70           



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# shift forwards in time
#=> plays at time 0
 
#=> plays at time 1
 
 
# time shifts forward by 0.1 beats
#=> plays at 1.1
 
#=> plays at 1.6
 
# time shifts back by 0.6 beats
 
# we now honour the original sleep 1 and the
# sleep 0.5 within the time_warp block is
# ignored including the 0.1 shift offset
 
#=> plays at 1



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


play 70           
sleep 1
play 75           

time_warp -0.1 do
                  
  play 80         
  sleep 0.5
  play 80         
                  
end
                  
                  
                  
play 70           



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# shift backwards in time
 
#=> plays at time 0
 
#=> plays at time 1
 
 
# time shifts backwards by 0.1 beats
#=> plays at 0.9
 
#=> plays at 1.4
# time shifts forward by 0.1 beats
 
# we now honour the original sleep 1 and the
# sleep 0.5 within the time_warp block is
# ignored, including the -0.1 offset
#=> plays at 1



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

sleep 1

time_warp 2 do
  puts tick       
end

sleep 0.5

puts tick         



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Ticks count linearly through time_warp
 
#=> prints 0 (at time 0)
 
 
 
 
#=> prints 1 (at time 3)
 
 
 
 
#=> prints 2 (at time 1.5)



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


puts tick         
sleep 0.5
puts tick         

time_warp 2 do
  puts tick       
  sleep 0.5
  puts tick       
end

at 3 do           
  puts tick       
  sleep 0.5
  puts tick       
end

sleep 0.5

puts tick         



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Comparing time_warp with at
 
#=> prints 0 (at time 0)
 
#=> prints 1 (at time 0.5)
 
 
#=> prints 2 (at time 2.5)
 
#=> prints 3 (at time 3)
 
 
# the at will reset all thread locals
#=> prints 0 (At time 3.5)
 
#=> prints 1 (At time 4)
 
 
 
 
#=> prints 4 (at time 1)



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

density 2 do                       
                                   
  time_warp 0.5 do                 
    with_fx :slicer, phase: 0.5 do 
      play 60
      sleep 1                      
    end
  end

end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Time Warp within Density
# Typically this will double the BPM and affect all times
# in addition to looping the internal block twice
# However, this time is *not* affected and will remain 0.5
# This phase duration *is* affected and will be 0.25
 
# This time *will* be affected by the density and be 0.5
 
 
 
 



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


time_warp [0, 1, 2, 3] do
  puts "hello"               
end
                               
                               
                               
                               
                               
                               
                               



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Time Warp with lists of times
 
 
# Will print "hello" at 0, 1, 2, and 3 seconds
 
# Notice that the run completes before all the
# messages have been delivered. This is because it
# schedules all the messages at once so the program
# can complete immediately. This is unlike at which
# would appear to behave similarly, but would wait
# for all messages to be delivered (on time) before
# allowing the program to complete.



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
time_warp [1, 2, 4] do 
    play 75               
  end                     



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# plays a note after waiting 1 beat,
# then after 1 more beat,
# then after 2 more beats (4 beats total)



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
time_warp [1, 2, 3], [75, 76, 77] do |n| 
    play n
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# plays 3 different notes
 
 



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
time_warp [1, 2, 3],
      [{:amp=>0.5}, {:amp=> 0.8}] do |p|
    sample :drum_cymbal_open, p         
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# alternate soft and loud
# cymbal hits three times
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 10 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
time_warp [0, 1, 2] do |t|
    puts t
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# when no params are given to at, the times are fed through to the block
#=> prints 0, 1, then 2
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 11 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
time_warp [0, 1, 2], [:a, :b] do |t, b| 
    puts [t, b]
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# If you specify the block with 2 args, it will pass through both the time and the param
#=> prints out [0, :a], [1, :b], then [2, :a]
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 12 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
time_warp [0, 0.5, 2] do |t, idx| 
    puts [t, idx]
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# If you specify the block with 2 args, and no param list to at, it will pass through both the time and the index
#=> prints out [0, 0], [0.5, 1], then [2, 2]
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 13 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
time_warp [0, 0.5, 2], [:a, :b] do |t, b, idx| 
    puts [t, b, idx]
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# If you specify the block with 3 args, it will pass through the time, the param and the index
#=> prints out [0, :a, 0], [0.5, :b, 1], then [2, :a, 2]
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 14 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

puts "main: ", rand 
rand_back
time_warp 1 do        
                      
  puts "time_warp:", rand
  puts "time_warp:", rand
  rand_back          
end

sleep 2
puts "main: ", rand



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# time_warp consumes & interferes with the outer random stream
# 0.75006103515625
 
# the random stream inside the at block is the
# same as the one in the outer block
# 0.75006103515625
# 0.733917236328125
# undo last call to rand
 
 
 
# value is now 0.733917236328125 again



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 15 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

           
           
time_warp [0, 2] do
           
  puts tick
  puts tick
end
           
           
           



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Each block run inherits the same thread locals from the previous one.
# This means things like the thread local counters can flow through
# time warp iterations:
 
# first time round (after 1 beat) prints:
# 0
# 1
 
# second time round (after 2 beats) prints:
# 2
# 3



```
<!--- #end tr -->

</td>
</tr>
</table>

