# Set Volume globally

```
set_volume! 
 <!--- #tr -->vol (number)<!--- #end tr -->
```


Set the main system volume to `vol`. Accepts a value between `0` and `5` inclusive. Vols greater or smaller than the allowed values are trimmed to keep them within range. Default is `1`.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
set_volume! 2



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Set the main system volume to 2



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
set_volume! -1



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Out of range, so sets main system volume to 0



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
set_volume! 7



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Out of range, so sets main system volume to 5



```
<!--- #end tr -->

</td>
</tr>
</table>

