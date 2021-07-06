# Block-scoped inhibition of synth triggers if too late

```
with_timing_guarantees 
 <!--- #tr -->bool (true_or_false)<!--- #end tr -->
```


For the given block, if set to true, synths will not trigger if it is too late. If false, some synth triggers may be late. After the block has completed, the previous value is restored. 

Introduced in v2.10

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
with_timing_guarantees true do
  sample :loop_amen 
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
#=> if time is behind by any margin, this will not trigger
 



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
with_timing_guarantees false do
  sample :loop_amen 
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
#=> unless time is too far behind, this will trigger even when late.
 



```
<!--- #end tr -->

</td>
</tr>
</table>

