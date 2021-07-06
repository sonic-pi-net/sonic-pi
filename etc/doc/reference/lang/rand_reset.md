# Reset rand generator to last seed

```
rand_reset 
 <!--- #tr --><!--- #end tr -->
```


Resets the random stream to the last specified seed. See `use_random_seed` for changing the seed.

Introduced in v2.7

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts rand
  puts rand
  puts rand
  puts rand
  rand_reset 
  puts rand



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# prints 0.75006103515625
# prints 0.733917236328125
# prints 0.464202880859375
# prints 0.24249267578125
# reset the random stream
# prints 0.75006103515625



```
<!--- #end tr -->

</td>
</tr>
</table>

