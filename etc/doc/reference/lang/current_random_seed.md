# Get current random seed

```
current_random_seed 
 <!--- #tr --><!--- #end tr -->
```


Returns the current random seed.

This can be set via the fns `use_random_seed` and `with_random_seed`. It is incremented every time you use the random number generator via fns such as `choose` and `rand`.

Introduced in v2.10

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts current_random_seed



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Print out the current random seed



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

puts rand              
puts rand              
a = current_random_seed
puts rand              
puts rand              
use_random_seed a      
                       
puts rand              
puts rand              



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Resetting the seed back to a known place
#=>  0.75006103515625
#=>  0.733917236328125
# Grab the current seed
#=> 0.464202880859375
#=> 0.24249267578125
# Restore the seed
# we'll now get the same random values:
#=> 0.464202880859375
#=> 0.24249267578125



```
<!--- #end tr -->

</td>
</tr>
</table>

