# Define a named value only once

```
defonce 
 <!--- #tr -->name (symbol)<!--- #end tr -->
```


Allows you to assign the result of some code to a name, with the property that the code will only execute once - therefore stopping re-definitions. This is useful for defining values that you use in your compositions but you don't want to reset every time you press run. You may force the block to execute again regardless of whether or not it has executed once already by using the override option (see examples).

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
defonce :foo do 
    sleep 1       
                  
                  
    puts "hello"
    10            
  end

 
  puts foo

 
  puts foo



  defonce :foo do
    puts "you can't redefine me"
    15
  end

  puts foo

 
 
  3.times do
    play foo 
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Define a new function called foo
# Sleep for a beat in the function definition. Note that this amount
# of time in seconds will depend on the current BPM of the live_loop
# or thread calling this function.
# Print hello
# Return a value of 10
 
 
# Call foo on its own
# The run sleeps for a beat and prints "hello" before returning 10
 
# Try it again:
# This time the run doesn't sleep or print anything out. However, 10 is still returned.
 
 
 
# Try redefining foo
 
 
 
 
# We still don't see any printing or sleeping, and the result is still 10
 
# You can use foo anywhere you would use normal code.
# For example, in a block:
 
# play 10
 



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
defonce :bar do
    50
  end

  play bar

  defonce :bar do
    70
  end

  play bar

  defonce :bar, override: true do 
    80
  end

  play bar



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
 
 
# plays 50
 
# This redefinition doesn't work due to the behaviour of defonce
 
 
 
# Still plays 50
 
# Force definition to take place with override option
 
 
 
# plays 80



```
<!--- #end tr -->

</td>
</tr>
</table>

