# Use Studio FX

```
with_fx 
 <!--- #tr -->fx_name (symbol)<!--- #end tr -->
```


This applies the named effect (FX) to everything within a given `do`/`end` block. Effects may take extra parameters to modify their behaviour. See FX help for parameter details.

For advanced control, it is also possible to modify the parameters of an effect within the body of the block. If you define the block with a single argument, the argument becomes a reference to the current effect and can be used to control its parameters (see examples).

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

with_fx :distortion do
  play 50
  sleep 1
  sample :loop_amen
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Basic usage
# Use the distortion effect with default parameters
# => plays note 50 with distortion
 
# => plays the loop_amen sample with distortion too
 



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

with_fx :level, amp: 0.3 do
  play 50
  sleep 1
  sample :loop_amen
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Specify effect parameters
# Use the level effect with the amp parameter set to 0.3
 
 
 
 



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

with_fx :reverb, mix: 0.1 do |fx|
 
 

  play 60
  sleep 2

  control fx, mix: 0.5
  play 60
  sleep 2

  control fx, mix: 1
  play 60
  sleep 2
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Controlling the effect parameters within the block
 
# here we set the reverb level quite low to start with (0.1)
# and we can change it later by using the 'fx' reference we've set up
 
# plays note 60 with a little bit of reverb
 
 
# change the parameters of the effect to add more reverb
# again note 60 but with more reverb
 
 
# change the parameters of the effect to add more reverb
# plays note 60 with loads of reverb
 
 



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

with_fx :reverb, reps: 16 do
  play (scale :e3, :minor_pentatonic), release: 0.1
  sleep 0.125
end


with_fx :reverb do
  16.times do
    play (scale :e3, :minor_pentatonic), release: 0.1
    sleep 0.125
  end
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Repeat the block 16 times internally
 
 
 
 
 
# The above is a shorthand for this:
 
 
 
 
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

