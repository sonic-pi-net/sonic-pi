# Clear all thread locals to defaults

```
clear 
 <!--- #tr --><!--- #end tr -->
```


All settings such as the current synth, BPM, random stream and tick values will be reset to their defaults. Consider using `reset` to reset all these values to those inherited from the parent thread.

Introduced in v2.11

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
Clear wipes out the threads locals
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
  clear
  puts "thread"        


                         
                         
                         
                         
  puts current_synth     

                         
  puts current_octave    

                         
                         
  puts rand              
  puts tick              
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
 
 
#=> "before"
#=> :blade
#=> 3
#=> 0.75006103515625
#=> 0
 
 
 
#=> 0.9287109375
 
#=> "thread"
 
 
# The clear reset the current synth to the default
# of :beep. We are therefore ignoring any inherited
# synth settings. It is as if the thread was a completely
# new Run.
#=> :beep
 
# The current octave defaults back to 0
#=> 0
 
# The random stream defaults back to the standard
# stream used by every new Run.
#=> 0.75006103515625
#=> 0
 



```
<!--- #end tr -->

</td>
</tr>
</table>

