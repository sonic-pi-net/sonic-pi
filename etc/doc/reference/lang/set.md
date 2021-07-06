# Store information in the Time State

```
set 
 <!--- #tr -->time_state_key (default), value (anything)<!--- #end tr -->
```


Store information in the Time State for the current time for either the current or any other thread. If called multiple times without an intervening call to `sleep`, `sync`, `set` or `cue`, the last value set will prevail. The value will remain in the Time State until overwritten by another call to `set`, or until Sonic Pi quits.

May be used within a `time_warp` to set past/future events. Does not affect time.

Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
set :foo, 1



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Stores the value 1 with key :foo



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
set :foo, 3 
get[:foo]



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Set :foo to 3
#=> returns 3



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
in_thread do
  set :foo, 3 
end

in_thread do
  puts get[:foo] 
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# Set :foo to 3
 
 
 
#=> always returns 3 (no race conditions here!)
 



```
<!--- #end tr -->

</td>
</tr>
</table>

