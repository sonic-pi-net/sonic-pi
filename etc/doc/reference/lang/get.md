# Get information from the Time State

```
get 
 <!--- #tr -->time_state_key (default)<!--- #end tr -->
```


Retrieve information from Time State set prior to the current time from either the current or any other thread. If called multiple times will always return the same value unless a call to `sleep`, `sync`, `set` or `cue` is interleaved. Also, calls to `get` will always return the same value across Runs for deterministic behaviour - which means you may safely use it in your compositions for repeatable music. If no value is stored with the relevant key, will return `nil`.

May be used within a `time_warp` to retrieve past events. If in a time warp, `get` can not be called from a future position. Does not advance time.

Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
get :foo



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> returns the last value set as :foo, or nil



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
 
 
 
 
 
#=> always returns 3 (no race conditions here!)
 



```
<!--- #end tr -->

</td>
</tr>
</table>

