# Change how random numbers are chosen

```
use_random_source 
 <!--- #tr -->noise_type (symbol)<!--- #end tr -->
```


Sets the random number source to be one of `:white`, `:pink`, `:light_pink`, `:dark_pink` or `:perlin`.

`:white` is totally random - between 0 and 1, you can expect an even spread of values around 0.1, 0.2, 0.3 etc. This means that jumping around within the range (including large jumps) is expected.

`:pink` is more likely to produce values in the middle of the range and less likely to produce values at the extremes. Between 0 and 1 you expect to see a concentration of values around 0.5. This can make random melodies a little bit more smooth.

`:perlin` is a special kind of noise which produces gradients, a bit like a mountain landscape. Large jumps are much less likely and you will tend to see lots of smooth motion going either up or down

`:light_pink` is halfway between white noise and pink noise - more random and jumpy

`:dark_pink` is halfway between pink noise and brown noise - less jumpy with smoother slopes

You can see the 'buckets' that the numbers between 0 and 1 fall into with the following code:

        rand_type :white
        puts 10000.times.collect { rand.round(1) }.tally.sort
        rand_type :pink
        puts 10000.times.collect { rand.round(1) }.tally.sort
        rand_type :perlin
        puts 10000.times.collect { rand.round(1) }.tally.sort

      

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

