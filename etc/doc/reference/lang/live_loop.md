# A loop for live coding

```
live_loop 
 <!--- #tr -->name (symbol)<!--- #end tr -->
```


Loop the do/end block forever. However, unlike a basic loop, a live_loop has two special properties. Firstly it runs in a thread - so you can have any number of live loops running at the same time (concurrently). Secondly, you can change the behaviour of a live loop whilst it is still running without needing to stop it. Live loops are therefore the secret to live coding with Sonic Pi.

As live loops are excecuted within a named in_thread, they behave similarly. See the in_thread documentation for all the details. However, it's worth mentioning a few important points here. Firstly, only one live loop with a given name can run at any one time. Therefore, if you define two or more `live_loop`s called `:foo` only one will be running. Another important aspect of `live_loop`s is that they manage their own thread locals set with the `use_*` and `with_*` fns. This means that each `live_loop` can have its own separate default synth, BPM and sample defaults. When a `live_loop` is *first* created, it inherits the thread locals from the parent thread, but once it has started, the only way to change them is by re-defining the do/end body of the `live_loop`. See the examples below for details. Finally, as mentioned above, provided their names are different, you may have many `live_loop`s executing at once.

A typical way of live coding with live loops is to define a number of them in a buffer, hit Run to start them and then to modify their do/end blocks and then hit Run again. This will not create any more thread, but instead just modify the behaviour of the existing threads. The changes will *not* happen immediately. Instead, they will only happen the next time round the loop. This is because the behaviour of each live loop is implemented with a standard function. When a live loop is updated, the function definition is also updated. Each time round the live loop, the function is called, so the new behviour is only observed next time round the loop.

Also sends a `cue` with the same name each time the `live_loop` repeats. This may be used to `sync` with other threads and `live_loop`s.

If the `live_loop` block is given a parameter, this is given the result of the last run of the loop (with initial value either being `0` or an init arg). This allows you to 'thread' values across loops.

Finally, it is possible to delay the initial trigger of the live_loop on creation with both the `delay:` and `sync:` opts. See their respective docstrings. If both `delay:` and `sync:` are specified, on initial live_loop creation first the delay will be honoured and then the sync.


Introduced in v2.1

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby


live_loop :ping do 
  sample :elec_ping
  sleep 1          
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Define and start a simple live loop
 
# Create a live loop called :ping
# This live loops plays the :elec_ping sample
# Then sleeps for 1 beat before repeating
 



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


live_loop :ping do 
  sample :elec_ping
                   
                   
                   
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Every live loop must sleep or sync
 
# Create a live loop called :ping
# This live loops plays the :elec_ping sample
# However, because the do/end lock of the live loop does not
# contain any calls to sleep or sync, the live loop stops at
# the end of the first loop with a 'Did not sleep' error.
 



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
  play 70
  sleep 1
end

live_loop :bar do 
  sample :bd_haus 
  sleep 0.5       
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Multiple live loops will play at the same time
# Start a live loop called :foo
 
 
 
 
# Start another live loop called :bar
# Both :foo and :bar will be playing
# at the same time.
 



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

use_bpm 30
live_loop :foo do
  play 70          
  sleep 1          
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Live loops inherit external use_* thread locals
 
 
# live loop :foo now has a BPM of 30
# This sleep will be for 2 seconds
 



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
  use_bpm 30      
  play 70
  sleep 1         
end

live_loop :bar do
  use_bpm 120     
  play 82
  sleep 1         
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Live loops can have their own thread locals
 
# Set the BPM of live loop :foo to 30
 
# This sleep will be for 2 seconds
 
 
 
# Set the BPM of live loop :bar to 120
 
# This sleep will be for 0.5 seconds
 



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

live_loop :foo do |a| 
  puts a              
  sleep 1
  a += 1              
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Live loops can pass values between iterations
# pass a param (a) to the block (inits to 0)
# prints out all the integers
 
# increment a by 1 (last value is passed back into the loop)
 



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

live_loop :foo do 
  play 70
  sleep 1
end

live_loop :foo do 
  sample :bd_haus 
  sleep 0.5       
                  
end               
                  



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Live loop names must be unique
# Start a live loop called :foo
 
 
 
 
# Attempt to start another also called :foo
# With a different do/end block
# This will not start another live loop
# but instead replace the behaviour of the first.
# There will only be one live loop running playing
# The bass drum



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

live_loop :foo, sync: :bar do
 play 70                     
 sleep 1                     
end

sleep 4                      

live_loop :bar do            
  sample :bd_haus            
  sleep 0.5                  
end                          

                             
                             
                             
                             
                             
                             
                             



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# You can sync multiple live loops together
# Wait for a :bar cue event before starting :foo
# Live loop :foo is therefore blocked and does
# not make a sound initially
 
 
# Wait for 4 beats
 
# Start a live loop called :foo which will emit a :bar
# cue message therefore releasing the :foo live loop.
# Live loop :foo therefore starts and also inherits the
# logical time of live loop :bar.
 
# This pattern is also useful to re-sync live loops after
# errors are made. For example, when modifying live loop :foo
# it is possible to introduce a runtime error which will stop
# :foo but not :bar (as they are separate, isolated threads).
# Once the error has been fixed and the code is re-run, :foo
# will automatically wait for :bar to loop round and restart
# in sync with the correct virtual clock.



```
<!--- #end tr -->

</td>
</tr>
</table>

