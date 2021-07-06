# Enable and disable arg checks

```
use_arg_checks 
 <!--- #tr -->true_or_false (boolean)<!--- #end tr -->
```


When triggering synths, each argument is checked to see if it is sensible. When argument checking is enabled and an argument isn't sensible, you'll see an error in the debug pane. This setting allows you to explicitly enable and disable the checking mechanism. See with_arg_checks for enabling/disabling argument checking only for a specific `do`/`end` block.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play 50, release: 5
use_arg_checks false
play 50, release: 5



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Args are checked
 
# Args are not checked



```
<!--- #end tr -->

</td>
</tr>
</table>

