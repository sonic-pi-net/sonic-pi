# Set random seed generator to known seed

```
use_random_seed 
 <!--- #tr -->seed (number)<!--- #end tr -->
```


Resets the random number generator to the specified seed. All subsequently generated random numbers and randomisation functions such as `shuffle` and `choose` will use this new generator and the current generator is discarded. Use this to change the sequence of random numbers in your piece in a way that can be reproduced. Especially useful if combined with iteration. See examples.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby


  use_random_seed 1
  puts rand
  use_random_seed 1
  puts rand 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Basic usage
 
# reset random seed to 1
# => 0.417022004702574
# reset random seed back to 1
#=> 0.417022004702574



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

  notes = (scale :eb3, :minor_pentatonic) 
                                          

  with_fx :reverb do
    live_loop :repeating_melody do        

      use_random_seed 300                 
                                          
                                          
                                          
                                          

      8.times do                          
                                          
                                          

        play notes.choose, release: 0.1   
                                          
                                          
                                          
        sleep 0.125
      end
    end
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Generating melodies
# Create a set of notes to choose from.
# Scales work well for this
 
 
# Create a live loop
 
# Set the random seed to a known value every
# time around the loop. This seed is the key
# to our melody. Try changing the number to
# something else. Different numbers produce
# different melodies
 
# Now iterate a number of times. The size of
# the iteration will be the length of the
# repeating melody.
 
# 'Randomly' choose a note from our ring of
# notes. See how this isn't actually random
# but uses a reproducible method! These notes
# are therefore repeated over and over...
 
 
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

