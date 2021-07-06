# Block-level enable and disable OSC logging

```
with_osc_logging 
 <!--- #tr -->true_or_false (boolean)<!--- #end tr -->
```


Similar to use_osc_logging except only applies to code within supplied `do`/`end` block. Previous OSC log value is restored after block.

Introduced in v3.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

  use_osc_logging true

  osc "/foo"

  with_osc_logging false do
   
    osc "/foo"
  end
  sleep 1
 
  osc "/foo"



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Turn on OSC logging:
 
 
#  message is printed to log
 
 
#OSC logging is now disabled
# OSC message *is* sent but not displayed in log
 
 
# Debug is re-enabled
# message is displayed in log



```
<!--- #end tr -->

</td>
</tr>
</table>

