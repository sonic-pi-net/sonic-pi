# Add swing to successive calls to do/end block

```
with_swing 
 <!--- #tr -->shift (beats), pulse (number), tick (symbol)<!--- #end tr -->
```


Runs block within a `time_warp` except for once every `pulse` consecutive runs (defaulting to 4). When used for rhythmical purposes this results in one in every `pulse` calls of the block being 'on beat' and the rest shifted forward or backwards in time by `shift` beats.

Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
live_loop :foo do
  with_swing 0.1 do
    sample :elec_beep     
  end
  sleep 0.25
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
# plays the :elec_beep sample late except for every 4th time
 
 
 



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
live_loop :foo do
  with_swing -0.1 do
    sample :elec_beep     
  end                     
  sleep 0.25
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
# plays the :elec_beep sample slightly early
# except for every 4th time
 
 



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
live_loop :foo do
  with_swing -0.1, pulse: 8 do
    sample :elec_beep     
  end                     
  sleep 0.25
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
# plays the :elec_beep sample slightly early
# except for every 8th time
 
 



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
  with_swing 0.14, tick: :a do
    sample :elec_beep     
  end                     

  with_swing -0.1, tick: :b do
    sample :elec_beep, rate: 2 
  end                          
  sleep 0.25
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Use unique tick names if you plan on using with_swing
# more than once in any given live_loop or thread.
 
 
# plays the :elec_beep sample slightly late
# except for every 4th time
 
 
# plays the :elec_beep sample at double rate
#  slightly early except for every 4th time
 
 



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
  with_swing 0.1 do
    cue :tick             
  end
  sleep 0.25
end

live_loop :bar do
  sync :tick
  sample :elec_beep      
                         
                         
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
# send out cue messages with swing timing
 
 
 
 
 
 
# sync on the swing cue messages to bring the swing into
# another live loop (sync will match the timing and clock of
# the sending live loop)
 



```
<!--- #end tr -->

</td>
</tr>
</table>

