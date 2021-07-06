# Block-level enable and disable cue logging

```
with_cue_logging 
 <!--- #tr -->true_or_false (boolean)<!--- #end tr -->
```


Similar to use_cue_logging except only applies to code within supplied `do`/`end` block. Previous cue log value is restored after block.

Introduced in v2.6

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

  use_cue_logging true

  cue :foo

  with_cue_logging false do
   
    cue :bar
  end
  sleep 1
 
  cue :quux



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Turn on debugging:
 
 
# cue message is printed to log
 
 
#Cue logging is now disabled
# cue *is* sent but not displayed in log
 
 
# Debug is re-enabled
# cue is displayed in log



```
<!--- #end tr -->

</td>
</tr>
</table>

