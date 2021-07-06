# Reset all thread locals

```
reset 
 <!--- #tr --><!--- #end tr -->
```


All settings such as the current synth, BPM, random stream and tick values will be reset to the values inherited from the parent thread. Consider using `clear` to reset all these values to their defaults.

Introduced in v2.11

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

use_synth :blade
use_octave 3

puts "before"        
puts current_synth     
puts current_octave    
puts rand              
puts tick              

reset

puts "after"         
puts current_synth     
puts current_octave    
puts rand              
puts tick              



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Basic Reset
 
 
 
#=> "before"
#=> :blade
#=> 3
#=> 0.75006103515625
#=> 0
 
 
 
#=> "after"
#=> :beep
#=> 0
#=> 0.75006103515625
#=> 0



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
Reset remembers defaults from when the thread was created:
use_synth :blade
use_octave 3

puts "before"        
puts current_synth     
puts current_octave    
puts rand              
puts tick              

at do
  use_synth :tb303
  puts rand              
  reset
  puts "thread"         


                         
                         
                         
                         
                         
  puts current_synth     
  puts current_octave    

                         
                         
                         
                         
  puts rand              
  puts tick              
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
 
 
#=> "before"
#=> :blade
#=> 3
#=> 0.75006103515625
#=> 0
 
 
 
#=> 0.9287109375
 
#=> "thread"
 
 
# The call to reset ensured that the current
# synth was returned to the the state at the
# time this thread was started. Thus any calls
# to use_synth between this line and the start
# of the thread are ignored
#=> :blade
#=> 3
 
# The call to reset ensured
# that the random stream was reset
# to the same state as it was when
# the current thread was started
#=> 0.9287109375
#=> 0
 



```
<!--- #end tr -->

</td>
</tr>
</table>

