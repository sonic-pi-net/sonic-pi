# Squash and repeat time

```
density 
 <!--- #tr -->d (density)<!--- #end tr -->
```


Runs the block `d` times with the bpm for the block also multiplied by `d`. Great for repeating sections a number of times faster yet keeping within a fixed time. If `d` is less than 1, then time will be stretched accordingly and the block will take longer to complete.

Introduced in v2.3

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_bpm 60  

  density 2 do      
                    
    sample :bd_haus
    sleep 0.5       
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Set the BPM to 60
 
# BPM for block is now 120
# block is called 2.times
# sample is played twice
# sleep is 0.25s
 



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
density 2 do |idx|
    puts idx        
    sleep 0.5       
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# You may also pass a param to the block similar to n.times
# prints out 0, 1
# sleep is 0.25s
 



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
density 0.5 do         
                         
                         
    play 80, release: 1  
    sleep 0.5            
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Specifying a density val of < 1 will stretch out time
# A density of 0.5 will double the length of the block's
# execution time.
# plays note 80 with 2s release
# sleep is 1s
 



```
<!--- #end tr -->

</td>
</tr>
</table>

