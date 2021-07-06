# Sync with other threads

```
sync 
 <!--- #tr -->cue_id (symbol)<!--- #end tr -->
```


Pause/block the current thread until a `cue` heartbeat with a matching `cue_id` is received. When a matching `cue` message is received, unblock the current thread, and continue execution with the virtual time set to match the thread that sent the `cue` heartbeat. The current thread is therefore synced to the `cue` thread. If multiple cue ids are passed as arguments, it will `sync` on the first matching `cue_id`. The BPM of the cueing thread can optionally be inherited by using the bpm_sync: opt.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
in_thread do
    sync :foo
    sample :ambi_lunar_land
  end

  sleep 5

  cue :foo
           



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# this parks the current thread waiting for a foo sync message to be received.
 
 
 
 
 
# We send a sync message from the main thread.
# This then unblocks the thread above and we then hear the sample



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
      cue :tick
      sleep 0.5 
    end
  end

 
  loop do                   
    sync :tick              
    sample :drum_heavy_kick 
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Start a metronome thread
# Loop forever:
# sending tick heartbeat messages
# and sleeping for 0.5 beats between ticks
 
 
 
# We can now play sounds using the metronome.
# In the main thread, just loop
# waiting for :tick sync messages
# after which play the drum kick sample
 



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
sync :foo, :bar



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Wait for either a :foo or :bar cue



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
in_thread do  
    loop do     
      cue [:foo, :bar, :baz].choose
      sleep 0.5 
    end
  end

 

  in_thread do
    loop do                   
      sync :foo              
      sample :elec_beep 
    end
  end

  in_thread do
    loop do                   
      sync :bar              
      sample :elec_flip 
    end
  end

  in_thread do
    loop do                   
      sync :baz              
      sample :elec_blup 
    end
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Start a metronome thread
# Loop forever:
# sending one of three tick heartbeat messages randomly
# and sleeping for 0.5 beats between ticks
 
 
 
# We can now play sounds using the metronome:
 
 
# In the main thread, just loop
# waiting for :foo sync messages
# after which play the elec beep sample
 
 
 
 
# In the main thread, just loop
# waiting for :bar sync messages
# after which play the elec flip sample
 
 
 
 
# In the main thread, just loop
# waiting for :baz sync messages
# after which play the elec blup sample
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

