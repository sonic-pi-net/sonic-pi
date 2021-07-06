# Block-scoped sample-duration-based bpm modification

```
with_sample_bpm 
 <!--- #tr -->string_or_number (sample_name_or_duration)<!--- #end tr -->
```


Block-scoped modification of bpm so that sleeping for 1 will sleep for the duration of the sample.

Introduced in v2.1

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
live_loop :dnb do
  with_sample_bpm :loop_amen do
    sample :bass_dnb_f
    sample :loop_amen
    sleep 1                    
  end
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
#Set bpm based on :loop_amen duration
 
 
#`sleep`ing for 1 sleeps for duration of :loop_amen
 
 



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
live_loop :dnb do
  with_sample_bpm :loop_amen, num_beats: 4 do
                                             
                                             
    sample :bass_dnb_f
    sample :loop_amen
    sleep 4                    
                               
                               
  end
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
# Set bpm based on :loop_amen duration
# but also specify that the sample duration
# is actually 4 beats long.
 
 
#`sleep`ing for 4 sleeps for duration of :loop_amen
# as we specified that the sample consisted of
# 4 beats
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

