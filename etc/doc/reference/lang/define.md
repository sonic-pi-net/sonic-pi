# Define a new function

```
define 
 <!--- #tr -->name (symbol)<!--- #end tr -->
```


Allows you to group a bunch of code and give it your own name for future re-use. Functions are very useful for structuring your code. They are also the gateway into live coding as you may redefine a function whilst a thread is calling it, and the next time the thread calls your function, it will use the latest definition.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

  define :foo do
    play 50
    sleep 1
  end

 
  foo

 
 
  3.times do
    foo
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Define a new function called foo
 
 
 
 
 
# Call foo on its own
 
 
# You can use foo anywhere you would use normal code.
# For example, in a block:
 
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

