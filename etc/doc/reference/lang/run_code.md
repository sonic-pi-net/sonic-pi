# Evaluate the code passed as a String as a new Run

```
run_code 
 <!--- #tr -->code (string)<!--- #end tr -->
```


Executes the code passed as a string in a new Run. This works as if the code was in a buffer and Run button was pressed.

Introduced in v2.11

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
run_code "sample :ambi_lunar_land"



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> will play the :ambi_lunar_land sample



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

run_code "8.times do
play 60
sleep 1
end"



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Works with any amount of code:
 
 
 
# will play 60 8 times



```
<!--- #end tr -->

</td>
</tr>
</table>

