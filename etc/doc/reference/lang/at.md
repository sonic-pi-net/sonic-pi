# Asynchronous Time. Run a block at the given time(s)

```
at 
 <!--- #tr -->times (list), params (list)<!--- #end tr -->
```


Given a list of times, run the block once after waiting each given time. If passed an optional params list, will pass each param individually to each block call. If size of params list is smaller than the times list, the param values will act as rings (rotate through). If the block is given 1 arg, the times are fed through. If the block is given 2 args, both the times and the params are fed through. A third block arg will receive the index of the time.

Note, all code within the block is executed in its own thread. Therefore despite inheriting all thread locals such as the random stream and ticks, modifications will be isolated to the block and will not affect external code.

`at` is just-in-time scheduling using multiple isolated threads. See `time_warp` for ahead-of-time scheduling within the current thread.

Introduced in v2.1

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
at 4 do
    sample :ambi_choir   
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# play sample after waiting for 4 beats
 



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
at [1, 2, 4] do 
    play 75          
  end                



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# plays a note after waiting 1 beat,
# then after 1 more beat,
# then after 2 more beats (4 beats total)



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
at [1, 2, 3], [75, 76, 77] do |n| 
    play n
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# plays 3 different notes
 
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 4 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
at [1, 2, 3],
      [{:amp=>0.5}, {:amp=> 0.8}] do |p|
    sample :drum_cymbal_open, p         
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
# alternate soft and loud
# cymbal hits three times
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 5 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
at [0, 1, 2] do |t|
    puts t
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# when no params are given to at, the times are fed through to the block
#=> prints 0, 1, then 2
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 6 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
at [0, 1, 2], [:a, :b] do |t, b| 
    puts [t, b]
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#If you specify the block with 2 args, it will pass through both the time and the param
#=> prints out [0, :a], [1, :b], then [2, :a]
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 7 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
at [0, 0.5, 2] do |t, idx| 
    puts [t, idx]
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
#If you specify the block with 2 args, and no param list to at, it will pass through both the time and the index
#=> prints out [0, 0], [0.5, 1], then [2, 2]
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 8 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
at [0, 0.5, 2], [:a, :b] do |t, b, idx| 
    puts [t, b, idx]
  end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#If you specify the block with 3 args, it will pass through the time, the param and the index
#=> prints out [0, :a, 0], [0.5, :b, 1], then [2, :a, 2]
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 9 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

puts "main: ", rand 
rand_back
at 1 do        
               
  puts "at:", rand
  puts "at:", rand
end

sleep 2
puts "main: ", rand



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# at does not consume & interfere with the outer random stream
# 0.75006103515625
 
# the random stream inside the at block is separate and
# isolated from the outer stream.
# 0.9287109375
# 0.1043701171875
 
 
 
# value is still 0.75006103515625



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 10 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby

at [1, 2] do
           
  puts rand
  puts rand
end
           
           
           



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Each block run within at has its own isolated random stream:
 
# first time round (after 1 beat) prints:
# 0.9287109375
# 0.1043701171875
 
# second time round (after 2 beats) prints:
# 0.1043701171875
# 0.764617919921875



```
<!--- #end tr -->

</td>
</tr>
</table>

