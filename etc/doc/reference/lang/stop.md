# Stop current thread or run

```
stop 
 <!--- #tr --><!--- #end tr -->
```


Stops the current thread or if not in a thread, stops the current run. Does not stop any running synths triggered previously in the run/thread or kill any existing sub-threads.

Introduced in v2.5

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
sample :loop_amen
  sleep 0.5
  stop               
  sample :loop_garzul



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> this sample is played until completion
 
#=> signal to stop executing this run
#=> this never executes



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
    play 60     
    stop
    sleep 0.5   
    play 72     
  end

  play 80 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
#=> this note plays
 
#=> this sleep never happens
#=> this play never happens
 
 
#=> this plays as the stop only affected the above thread



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

  live_loop :foo
    sample :bd_haus
    sleep 1
    stop              
  end

  live_loop :bar      
    sample :elec_blip
    sleep 0.25
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Stopping live loops
 
 
 
# live loop :foo will now stop and no longer loop
 
 
# live loop :bar will continue looping
 
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

