# Run code block at the same time

```
in_thread 
 <!--- #tr --><!--- #end tr -->
```


Execute a given block (between `do` ... `end`) in a new thread. Use for playing multiple 'parts' at once. Each new thread created inherits all the use/with defaults of the parent thread such as the time, current synth, bpm, default synth args, etc. Despite inheriting defaults from the parent thread, any modifications of the defaults in the new thread will *not* affect the parent thread. Threads may be named with the `name:` optional arg. Named threads will print their name in the logging pane when they print their activity. If you attempt to create a new named thread with a name that is already in use by another executing thread, no new thread will be created.

It is possible to delay the initial trigger of the thread on creation with both the `delay:` and `sync:` opts. See their respective docstrings. If both `delay:` and `sync:` are specified, on initial thread creation first the delay will be honoured and then the sync.


Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
loop do     
    play 50   
    sleep 1   
  end

  loop do     
    play 55
    sleep 0.5
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# If you write two loops one after another like this,
# then only the first loop will execute as the loop acts
# like a trap not letting the flow of control out
 
 
# This code is never executed.
 
 
 



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

 
 

 
 
 
 

  in_thread do
   
    loop do
     
      play 50
      sleep 1
    end
  end

 

  loop do     
    play 55
    sleep 0.5
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# In order to play two loops at the same time, the first loops need to
# be in a thread (note that it's probably more idiomatic to use live_loop
# when performing):
 
# By wrapping our loop in an in_thread block, we split the
# control flow into two parts. One flows into the loop (a) and
# the other part flows immediately after the in_thread block (b).
# both parts of the control flow execute at exactly the same time.
 
 
# (a)
 
# (a)
 
 
 
 
 
# (b)
 
# This loop is executed thanks to the thread above
 
 
 



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
  use_synth :dsaw 

  in_thread do    
    play 50       
    use_synth :fm 
    sleep 1       
    play 38       
  end

  play 62         
  sleep 2         
  play 67         



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Set the bpm to be double rate
# Set the current synth to be :dsaw
 
# Create a new thread
# Play note 50 at time 0
# Switch to fm synth (only affects this thread)
# sleep for 0.5 seconds (as we're double rate)
# Play note 38 at time 0.5
 
 
# Play note 62 at time 0 (with dsaw synth)
# sleep 1s
# Play note 67 at time 1s (also with dsaw synth)



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
in_thread(name: :foo) do
    loop do
      sample :drum_bass_hard
      sleep 1
    end
  end

  in_thread(name: :foo) do
    loop do               
      sample :elec_chime  
      sleep 0.5
    end
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Here we've created a named thread
 
 
 
 
 
 
# This thread isn't created as the name is
# the same as the previous thread which is
# still executing.
 
 
 



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

  define :foo do 
    play 50      
    sleep 1      
  end

  in_thread(name: :main) do 
    loop do                 
      foo                   
    end
  end

 
 
 
 
 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Named threads work well with functions for live coding:
# Create a function foo
# which does something simple
# and sleeps for some time
 
 
# Create a named thread
# which loops forever
# calling our function
 
 
 
# We are now free to modify the contents of :foo and re-run the entire buffer.
# We'll hear the effect immediately without having to stop and re-start the code.
# This is because our fn has been redefined, (which our thread will pick up) and
# due to the thread being named, the second re-run will not create a new similarly
# named thread. This is a nice pattern for live coding and is the basis of live_loop.



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

  in_thread delay: 1 do
    sample :ambi_lunar_land
  end

  play 80                  



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#Delaying the start of a thread
 
# this sample is not triggered at time 0 but after 1 beat
 
 
# Note 80 is played at time 0



```
<!--- #end tr -->

</td>
</tr>
</table>

