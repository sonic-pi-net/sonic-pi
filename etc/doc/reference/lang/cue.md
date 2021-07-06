# Cue other threads

```
cue 
 <!--- #tr -->cue_id (symbol)<!--- #end tr -->
```


Send a heartbeat synchronisation message containing the (virtual) timestamp of the current thread. Useful for syncing up external threads via the `sync` fn. Any opts which are passed are given to the thread which syncs on the `cue_id`. The values of the opts must be immutable. Currently numbers, symbols, booleans, nil and frozen strings, or vectors/rings/frozen arrays/maps of immutable values are supported.

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
 
# this parks the current thread waiting for a foo cue message to be received.
 
 
 
 
 
# We send a cue message from the main thread.
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
# waiting for :tick cue messages
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
<td class="even">

<!--- #tr -->
```ruby
# Start a metronome thread
# Loop forever:
# sending one of three tick heartbeat messages randomly
# and sleeping for 0.5 beats between ticks
 
 
 
# We can now play sounds using the metronome:
 
 
# In the main thread, just loop
# waiting for :foo cue messages
# after which play the elec beep sample
 
 
 
 
# In the main thread, just loop
# waiting for :bar cue messages
# after which play the elec flip sample
 
 
 
 
# In the main thread, just loop
# waiting for :baz cue messages
# after which play the elec blup sample
 
 



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
      cue :tick, foo: 64 
      sleep 0.5
    end
  end

 

  loop do
    values = sync :tick
    play values[:foo]   
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
# sending tick heartbeat messages with a value :foo
 
 
 
 
# The value for :foo can now be used in synced threads
 
 
 
# play the note value from :foo
 



```
<!--- #end tr -->

</td>
</tr>
</table>

