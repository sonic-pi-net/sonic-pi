# Minecraft Pi - post a chat message

```
mc_message 
 <!--- #tr -->msg (string)<!--- #end tr -->
```


Post contents of `msg` on the Minecraft chat display. You may pass multiple arguments and all will be joined to form a single message (with spaces).

Introduced in v2.5

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
mc_message "Hello from Sonic Pi"



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Displays "Hello from Sonic Pi" on Minecraft's chat display



```
<!--- #end tr -->

</td>
</tr>
</table>

