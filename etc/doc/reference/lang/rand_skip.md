# Jump forward random generator

```
rand_skip 
 <!--- #tr -->amount (number)<!--- #end tr -->
```


Jump the random generator forward essentially skipping the next call to `rand`. You may specify an amount to jump allowing you to skip n calls to `rand`.

Introduced in v2.7

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby


  puts rand

  rand_skip
           

  puts rand



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Basic rand stream skip
 
# prints 0.75006103515625
 
# jump random stream forward one
# typically the next rand is 0.733917236328125
 
# prints 0.464202880859375



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
  puts rand
  puts rand

  rand_reset 

  puts rand

  rand_skip(2)
              
              
              

  puts rand 0.24249267578125


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Jumping forward multiple places in the rand stream
 
# prints 0.75006103515625
# prints 0.733917236328125
# prints 0.464202880859375
# prints 0.24249267578125
 
# reset the random stream
 
# prints 0.75006103515625
 
# jump random stream forward three places
# the result of the next call to rand will be
# exactly the same as if rand had been called
# three times
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

