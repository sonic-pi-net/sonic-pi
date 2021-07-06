# Specify random distribution for code block

```
with_random_source 
 <!--- #tr -->noise_type (symbol)<!--- #end tr -->
```


Resets the random number generator to the specified noise type for the specified code block. All generated random numbers and randomisation functions such as `shuffle` and `choose` within the code block will use this new generator. Once the code block has completed, the original generator is restored and the code block generator is discarded. Use this to change the sequence of random numbers in your piece in a way that can be reproduced. Especially useful if combined with iteration. See examples.

Introduced in v3.3

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
use_random_source :white
  rand_reset
  puts rand
  puts rand
  puts rand
  rand_reset
  use_random_source :pink
  puts rand
  puts rand
  rand_reset
  use_random_source :perlin
  puts rand
  puts rand

  with_random_source :white do
    puts rand
  end

  puts rand
           



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# use white noise as the distribution (default)
# reset random seed
# => 0.75006103515625
# => 0.733917236328125
# => 0.464202880859375
# reset it again
# use pink noise as the distribution
# => 0.47808837890625
# => 0.56011962890625
# reset it
# use perlin noise as the distribution
# => 0.546478271484375
# => 0.573150634765625
 
# use white noise just for this block
# => 0.464202880859375
 
 
# => 0.597015380859375
# notice how the last generator (perlin) is restored



```
<!--- #end tr -->

</td>
</tr>
</table>

